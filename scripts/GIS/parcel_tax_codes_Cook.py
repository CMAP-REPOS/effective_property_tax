### Purpose: Generate unique tax codes from parcel/assessor data, and join to descriptive tax districts

from arcpy.sa import *

# Year variable
year = '2018'

## Cook Processing Scenario: Tax Code present in Assessor table but not parcels.  No condo table present. ##

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Dissolve/summarize Parcels and Assessor Data
arcpy.Dissolve_management("Parcels_Cook_"+year,"Parcels_Cook_"+year+"_Dissolve_PIN10",'PIN10')
arcpy.Statistics_analysis("AssessorData_Cook_"+year,"AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode","PIN10 COUNT","PIN10; taxCode_CORRECT")

# Re-summarize assessor data to identify/export PIN10s with unique tax codes
arcpy.Statistics_analysis("AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode",'AssessorData_Cook_'+year+'_Summary_PIN10',"PIN10 COUNT","PIN10")
arcpy.AddJoin_management("AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode","PIN10","AssessorData_Cook_"+year+"_Summary_PIN10","PIN10")
arcpy.SelectLayerByAttribute_management("AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode","NEW_SELECTION",'AssessorData_Cook_'+year+'_Summary_PIN10.COUNT_PIN10 = 1')
arcpy.RemoveJoin_management("AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode")
arcpy.TableToTable_conversion("AssessorData_Cook_"+year+"_Summary_PIN10_TaxCode",arcpy.env.workspace,'AssessorData_Cook_'+year+'_UniqueTaxCodes')

# Add Tax Code Field, Join, and Calculate
arcpy.AddField_management("Parcels_Cook_"+year+"_Dissolve_PIN10",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Parcels_Cook_"+year+"_Dissolve_PIN10","PIN10","AssessorData_Cook_"+year+"_UniqueTaxCodes","PIN10")
arcpy.CalculateField_management("Parcels_Cook_"+year+"_Dissolve_PIN10","Parcels_Cook_"+year+"_Dissolve_PIN10.TAX_CODE",'!AssessorData_Cook_'+year+'_UniqueTaxCodes.taxCode_CORRECT!',"PYTHON")
arcpy.RemoveJoin_management("Parcels_Cook_"+year+"_Dissolve_PIN10")


### WARNING: There are some overlapping parcels with different PIN10s and different tax codes, which are not addressed by this script.
### The PolygonToRaster_conversion() function will assign those areas to a single tax code, but the choice may be totally random (??)


# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion("Parcels_Cook_"+year+"_Dissolve_PIN10","Tax_Code",'Cook_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("Cook_TaxCodes_"+year+"_15ft",'FocalMaj_NewPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('Cook_TaxCodes_'+year+'_15ft', 'ANYNODATA')
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
arcpy.CopyRaster_management(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),'Cook_TaxCodes_'+year+'_Expanded')
arcpy.RasterToPolygon_conversion("Cook_TaxCodes_"+year+"_Expanded",'Cook_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("Cook_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'Cook_TaxCodes_grid_attributes')
arcpy.AddField_management("Cook_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Cook_TaxCodes_"+year+"_poly","gridcode","Cook_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("Cook_TaxCodes_"+year+"_poly","Cook_TaxCodes_"+year+"_poly.TAX_CODE",'!Cook_TaxCodes_grid_attributes.TAX_CODE!','PYTHON')
arcpy.RemoveJoin_management("Cook_TaxCodes_"+year+"_poly")

# Import and join tax code table, clip and export to "Results" GDB
arcpy.TableToTable_conversion("Cook"+year+"$",arcpy.env.workspace,'Cook_TaxCodes_Descriptive')
arcpy.AddJoin_management("Cook_TaxCodes_"+year+"_poly","TAX_CODE","Cook_TaxCodes_Descriptive","tax_code")
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '031'")

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'
arcpy.Clip_analysis("Cook_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'Cook_TaxDistricts_'+year)
del_field_list = ['Cook_TaxCodes_'+year+'_poly_Id','Cook_TaxCodes_'+year+'_poly_gridcode','Cook_TaxCodes_Descriptive_OBJECTID','Cook_TaxCodes_'+year+'_poly_TAX_CODE']
for del_field in del_field_list:
	arcpy.DeleteField_management('Cook_TaxDistricts_'+year, del_field)
