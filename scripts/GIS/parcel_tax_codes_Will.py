## Will Processing Scenario: Tax Code present in Assessor table but not parcels.  No condo table present. ##

from arcpy.sa import *

# Year variable
year = '2018'

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Dissolve/summarize Parcels and Assessor Data
arcpy.Dissolve_management("Parcels_Will_"+year,'Parcels_Will_'+year+'_Dissolve_PIN','PIN')
arcpy.Statistics_analysis("AssessorData_Will_"+year,'AssessorData_Will_'+year+'_Summary_PIN_TaxCode',"PIN COUNT","PIN; tax_code")

# Re-summarize assessor data to identify/export PINs with unique tax codes
arcpy.Statistics_analysis("AssessorData_Will_"+year+"_Summary_PIN_TaxCode",'AssessorData_Will_'+year+'_Summary_PIN',"PIN COUNT","PIN")
arcpy.AddJoin_management("AssessorData_Will_"+year+"_Summary_PIN_TaxCode","PIN","AssessorData_Will_"+year+"_Summary_PIN","PIN")
arcpy.SelectLayerByAttribute_management("AssessorData_Will_"+year+"_Summary_PIN_TaxCode","NEW_SELECTION",'AssessorData_Will_'+year+'_Summary_PIN.COUNT_PIN = 1')
arcpy.RemoveJoin_management("AssessorData_Will_"+year+"_Summary_PIN_TaxCode")
arcpy.TableToTable_conversion("AssessorData_Will_"+year+"_Summary_PIN_TaxCode",arcpy.env.workspace,'AssessorData_Will_'+year+'_UniqueTaxCodes')

# Add Tax Code Field, Join, and Calculate
arcpy.AddField_management("Parcels_Will_"+year+"_Dissolve_PIN",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Parcels_Will_"+year+"_Dissolve_PIN","PIN","AssessorData_Will_"+year+"_UniqueTaxCodes","PIN")
arcpy.CalculateField_management("Parcels_Will_"+year+"_Dissolve_PIN","Parcels_Will_"+year+"_Dissolve_PIN.TAX_CODE",'!AssessorData_Will_'+year+'_UniqueTaxCodes.tax_code!',"PYTHON")
arcpy.RemoveJoin_management("Parcels_Will_"+year+"_Dissolve_PIN")

# DISSOLVE ON CENTROID XY AND FIND MAJORITY TAX_CODE FOR STACKED PARCELS
arcpy.FeatureToPoint_management("Parcels_Will_"+year+"_Dissolve_PIN", "Parcels_Will_"+year+"_Dissolve_PIN_centroids", "INSIDE")
arcpy.AddField_management("Parcels_Will_"+year+"_Dissolve_PIN",'COORD',"TEXT",'#','#',200)
with arcpy.da.UpdateCursor("Parcels_Will_"+year+"_Dissolve_PIN", ["SHAPE@XY", "COORD"]) as c:
    for r in c:
        r[1] = str(r[0])
        c.updateRow(r)
arcpy.Dissolve_management("Parcels_Will_"+year+"_Dissolve_PIN", 'Parcels_Will_unique_polygons', "COORD")
arcpy.SelectLayerByAttribute_management("Parcels_Will_"+year+"_Dissolve_PIN_centroids", "NEW_SELECTION", "TAX_CODE IS NOT NULL")
arcpy.SummarizeWithin_analysis('Parcels_Will_unique_polygons', "Parcels_Will_"+year+"_Dissolve_PIN_centroids", 'Parcels_Will_'+year+'_SummarizeWithin_CondoPoints', group_field='TAX_CODE', add_min_maj='ADD_MIN_MAJ')

### IMPORTANT: Manually review any SummarizeWithin polygons where Majority_TAX_CODE LIKE '%;%' and adjust to assign a single tax code

arcpy.SelectLayerByAttribute_management('Parcels_Will_'+year+'_SummarizeWithin_CondoPoints', "NEW_SELECTION", "Majority_TAX_CODE <> 'None'")
arcpy.Dissolve_management('Parcels_Will_'+year+'_SummarizeWithin_CondoPoints', 'Parcels_Will_Dissolve_TaxCode', "Majority_TAX_CODE",'#',"SINGLE_PART")

# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion("Parcels_Will_Dissolve_TaxCode","Majority_TAX_CODE",'Will_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("Will_TaxCodes_"+year+"_15ft",'FocalMaj_FirstPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('Will_TaxCodes_'+year+'_15ft', 'ANYNODATA')
i=1

# First pass: Re-assign null cells with focalMean until none remain or 100 passes
while True:
    rastIn = arcpy.Raster('FocalMaj_FirstPass'+str(i-1))
    outName = 'FocalMaj_FirstPass'+str(i)
    rastOut = Con(IsNull(rastIn),FocalStatistics(rastIn,NbrRectangle(5,5, "CELL"), "MAJORITY"),rastIn)
    rastOut.save(outName)
    noDataFlag = arcpy.GetRasterProperties_management(outName, 'ANYNODATA')
    i+=1
    arcpy.AddMessage(outName)
    if noDataFlag == 0:
        break
    if i == 100:
        break

if noDataFlag != 0:
	arcpy.CopyRaster_management('FocalMaj_FirstPass'+str(i-1),'FocalMaj_CirclePass0')

	# Generate initial noData flag
	noDataFlag = arcpy.GetRasterProperties_management('FocalMaj_FirstPass'+str(i-1), 'ANYNODATA')
	i=1

	# Re-assign null cells with Circular focalMean until none remain or 10 passes
	while True:
	    rastIn = arcpy.Raster('FocalMaj_CirclePass'+str(i-1))
	    outName = 'FocalMaj_CirclePass'+str(i)
	    rastOut = Con(IsNull(rastIn),FocalStatistics(rastIn,NbrCircle(5, "CELL"), "MAJORITY"),rastIn)
	    rastOut.save(outName)
	    noDataFlag = arcpy.GetRasterProperties_management(outName, 'ANYNODATA')
	    i+=1
	    arcpy.AddMessage(outName)
	    if noDataFlag == 0:
	        break
	    if i == 10:
	        break

if noDataFlag != 0:
	arcpy.CopyRaster_management('FocalMaj_CirclePass'+str(i-1),'FocalMaj_SecondPass0')

	# Generate initial noData flag
	noDataFlag = arcpy.GetRasterProperties_management('FocalMaj_CirclePass'+str(i-1), 'ANYNODATA')
	i=1

	# Second pass: Re-assign null cells with focalMean until none remain or 100 passes
	while True:
	    rastIn = arcpy.Raster('FocalMaj_SecondPass'+str(i-1))
	    outName = 'FocalMaj_SecondPass'+str(i)
	    rastOut = Con(IsNull(rastIn),FocalStatistics(rastIn,NbrRectangle(3,3, "CELL"), "MAXIMUM"),rastIn)
	    rastOut.save(outName)
	    noDataFlag = arcpy.GetRasterProperties_management(outName, 'ANYNODATA')
	    i+=1
	    arcpy.AddMessage(outName)
	    if noDataFlag == 0:
	        break
	    if i == 100:
	        break

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\GIS.gdb'

# Make copy of final grid, then convert to polygon
arcpy.CopyRaster_management(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),'Will_TaxCodes_'+year+'_Expanded')
arcpy.RasterToPolygon_conversion("Will_TaxCodes_"+year+"_Expanded",arcpy.env.workspace + r'/Will_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("Will_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'Will_TaxCodes_grid_attributes')
arcpy.AddField_management("Will_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Will_TaxCodes_"+year+"_poly","gridcode","Will_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("Will_TaxCodes_"+year+"_poly","Will_TaxCodes_"+year+"_poly.TAX_CODE",'!Will_TaxCodes_grid_attributes.Majority_TAX_CODE!','PYTHON')
arcpy.RemoveJoin_management("Will_TaxCodes_"+year+"_poly")

# Import and join tax code table, clip and export to "Results" GDB
arcpy.TableToTable_conversion("Will"+year+"$",arcpy.env.workspace,'Will_TaxCodes_Descriptive')
arcpy.AddJoin_management("Will_TaxCodes_"+year+"_poly","TAX_CODE","Will_TaxCodes_Descriptive","TaxCodesForJoin")
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '197'")

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'
arcpy.Clip_analysis("Will_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'Will_TaxDistricts_'+year)
del_field_list = ['Will_TaxCodes_'+year+'_poly_Id','Will_TaxCodes_'+year+'_poly_gridcode','Will_TaxCodes_Descriptive_OBJECTID','Will_TaxCodes_'+year+'_poly_TAX_CODE']
for del_field in del_field_list:
	arcpy.DeleteField_management('Will_TaxDistricts_'+year, del_field)
