##### DuPage Processing Scenario: Tax Code present in Parcels and assessor data.  Multipin parcels present. ######

# Year variable
year = '2018'

from arcpy.sa import *

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Dissolve/summarize Parcels and Assessor Data
arcpy.Dissolve_management("Parcels_DuPage_"+year,'Parcels_DuPage_'+year+'_Dissolve_PIN', 'PIN')
arcpy.Statistics_analysis("AssessorData_DuPage_"+year,'AssessorData_DuPage_'+year+'_Summary_PIN_TaxCode',"Parcel_TXT COUNT","Parcel_TXT; Tax_Code")  # Note: this doesn't actually do any summarization as Parcel_TXT is unique...
arcpy.CalculateField_management('AssessorData_DuPage_'+year+'_Summary_PIN_TaxCode', 'Parcel_TXT', "!Parcel_TXT!.replace('-','')", 'PYTHON')

# Re-summarize assessor data to identify/export PIN10s with unique tax codes
arcpy.Statistics_analysis("AssessorData_DuPage_"+year+"_Summary_PIN_TaxCode",'AssessorData_DuPage_'+year+'_Summary_PIN',"Parcel_TXT COUNT","Parcel_TXT")
arcpy.AddJoin_management("AssessorData_DuPage_"+year+"_Summary_PIN_TaxCode","Parcel_TXT","AssessorData_DuPage_"+year+"_Summary_PIN","Parcel_TXT")
arcpy.SelectLayerByAttribute_management("AssessorData_DuPage_"+year+"_Summary_PIN_TaxCode","NEW_SELECTION",'AssessorData_DuPage_'+year+'_Summary_PIN.COUNT_Parcel_TXT = 1')
arcpy.RemoveJoin_management("AssessorData_DuPage_"+year+"_Summary_PIN_TaxCode")
arcpy.TableToTable_conversion("AssessorData_DuPage_"+year+"_Summary_PIN_TaxCode",arcpy.env.workspace,'AssessorData_DuPage_'+year+'_UniqueTaxCodes')

# Add Tax Code Field, Join, and Calculate
arcpy.AddField_management("Parcels_DuPage_"+year+"_Dissolve_PIN",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Parcels_DuPage_"+year+"_Dissolve_PIN","PIN","AssessorData_DuPage_"+year+"_UniqueTaxCodes","Parcel_TXT")
arcpy.CalculateField_management("Parcels_DuPage_"+year+"_Dissolve_PIN","Parcels_DuPage_"+year+"_Dissolve_PIN.TAX_CODE",'!AssessorData_DuPage_'+year+'_UniqueTaxCodes.Tax_Code!',"PYTHON")
arcpy.RemoveJoin_management("Parcels_DuPage_"+year+"_Dissolve_PIN")

# NMP added: DISSOLVE ON MULTIPIN_PRIMARYPIN AND FIND MAJORITY TAX_CODE FOR STACKED PARCELS
arcpy.FeatureToPoint_management("Parcels_DuPage_"+year+"_Dissolve_PIN", "Parcels_DuPage_"+year+"_Dissolve_PIN_centroids", "INSIDE")
arcpy.SelectLayerByAttribute_management("Parcels_DuPage_"+year+"_Dissolve_PIN_centroids", "NEW_SELECTION", "TAX_CODE IS NOT NULL")
arcpy.Dissolve_management("Parcels_DuPage_"+year, "Parcels_DuPage_unique_polygons", "MULTIPIN_PRIMARYPIN")
arcpy.SummarizeWithin_analysis('Parcels_DuPage_unique_polygons', "Parcels_DuPage_"+year+"_Dissolve_PIN_centroids", 'Parcels_DuPage_'+year+'_SummarizeWithin_CondoPoints', group_field='TAX_CODE', add_min_maj='ADD_MIN_MAJ')

### IMPORTANT: Manually review any SummarizeWithin polygons where Majority_TAX_CODE LIKE '%;%' and adjust to assign a single tax code

arcpy.SelectLayerByAttribute_management('Parcels_DuPage_'+year+'_SummarizeWithin_CondoPoints', "NEW_SELECTION", "MULTIPIN_PRIMARYPIN IS NOT NULL AND Majority_TAX_CODE <> 'None'")
arcpy.Dissolve_management('Parcels_DuPage_'+year+'_SummarizeWithin_CondoPoints', 'Parcels_DuPage_Dissolve_TaxCode', "Majority_TAX_CODE",'#',"SINGLE_PART")

# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion('Parcels_DuPage_Dissolve_TaxCode',"Majority_TAX_CODE",'DuPage_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("DuPage_TaxCodes_"+year+"_15ft",'FocalMaj_NewPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('DuPage_TaxCodes_'+year+'_15ft', 'ANYNODATA')
i=1

# First pass: Re-assign null cells with focalMean until none remain or 100 passes
while True:
    rastIn = arcpy.Raster('FocalMaj_NewPass'+str(i-1))
    outName = 'FocalMaj_NewPass'+str(i)
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
	arcpy.CopyRaster_management('FocalMaj_NewPass'+str(i-1),'FocalMaj_CirclePass0')

	# Generate initial noData flag
	noDataFlag = arcpy.GetRasterProperties_management('FocalMaj_NewPass'+str(i-1), 'ANYNODATA')
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
	    rastOut = Con(IsNull(rastIn),FocalStatistics(rastIn,NbrRectangle(5,5, "CELL"), "MAXIMUM"),rastIn)
	    rastOut.save(outName)
	    noDataFlag = arcpy.GetRasterProperties_management(outName, 'ANYNODATA')
	    i+=1
	    arcpy.AddMessage(outName)
	    if noDataFlag == 0:
	        break
	    if i == 100:
	        break

### Make copy of final grid, then convert to polygon ###
### Erase tax districts from County polygon, merge and eliminate ###
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\GIS.gdb'
arcpy.CopyRaster_management(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),'DuPage_TaxCodes_'+year+'_Expanded')
arcpy.RasterToPolygon_conversion("DuPage_TaxCodes_"+year+"_Expanded",'DuPage_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("DuPage_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'DuPage_TaxCodes_grid_attributes')
arcpy.AddField_management("DuPage_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("DuPage_TaxCodes_"+year+"_poly","gridcode","DuPage_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("DuPage_TaxCodes_"+year+"_poly","DuPage_TaxCodes_"+year+"_poly.TAX_CODE",'!DuPage_TaxCodes_grid_attributes.Majority_TAX_CODE!','PYTHON')
arcpy.RemoveJoin_management("DuPage_TaxCodes_"+year+"_poly")

# Import and join tax code table, clip and export to "Results" GDB
arcpy.TableToTable_conversion("Dupage"+year+"$",arcpy.env.workspace,'DuPage_TaxCodes_Descriptive_v2')
arcpy.AddJoin_management("DuPage_TaxCodes_"+year+"_poly","TAX_CODE","DuPage_TaxCodes_Descriptive_v2","tax_code")
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '043'")

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'
arcpy.Clip_analysis("DuPage_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'DuPage_TaxDistricts_'+year)
del_field_list = ['DuPage_TaxCodes_'+year+'_poly_Id','DuPage_TaxCodes_'+year+'_poly_gridcode','DuPage_TaxCodes_Descriptive_v2_OBJECTID','DuPage_TaxCodes_'+year+'_poly_TAX_CODE']
for del_field in del_field_list:
	arcpy.DeleteField_management('DuPage_TaxDistricts_'+year, del_field)
