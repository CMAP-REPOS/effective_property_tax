##### Kane Processing Scenario: Tax Code present in Parcels.  Condo table(s) present. ######

from arcpy.sa import *

# Year variable
year = '2018'

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Export condo table, summarize by PIN and Polygon
arcpy.TableToTable_conversion("Condos",arcpy.env.workspace,"CondoTable_Kane_"+year)
arcpy.Statistics_analysis("CondoTable_Kane_"+year, "CondoTable_Kane_"+year+"_UniquePIN", "PIN COUNT", "PIN;Polygon")

# Join to Assessor table, Add/Calc tax code
arcpy.AddField_management("CondoTable_Kane_"+year+"_UniquePIN", "TAX_CODE", "TEXT", field_length=25)
arcpy.AddJoin_management("CondoTable_Kane_"+year+"_UniquePIN", "PIN", "AssessorData_Kane_"+year, "PIN")
arcpy.CalculateField_management("CondoTable_Kane_"+year+"_UniquePIN","CondoTable_Kane_"+year+"_UniquePIN.TAX_CODE",'!AssessorData_Kane_'+year+'.Tax_Code!',"PYTHON")
arcpy.RemoveJoin_management("CondoTable_Kane_"+year+"_UniquePIN")
arcpy.AddJoin_management("Parcels_Kane_"+year, "PIN", "CondoTable_Kane_"+year+"_UniquePIN", "Polygon")
arcpy.SelectLayerByAttribute_management("Parcels_Kane_"+year,"NEW_SELECTION", 'TAX_CODE IS NOT NULL')

# Get most frequent tax code by primary pin, select PINs with multiple tax codes
arcpy.Statistics_analysis("CondoTable_Kane_"+year+"_UniquePIN",'CondoTable_Kane_'+year+'_PIN_TaxCode_Frequency',"TAX_CODE COUNT","Polygon;TAX_CODE")
arcpy.Statistics_analysis("CondoTable_Kane_"+year+"_PIN_TaxCode_Frequency",'CondoTable_Kane_'+year+'_PIN_TaxCode_Max_TaxCode',"TAX_CODE COUNT","Polygon")
arcpy.AddJoin_management("CondoTable_Kane_"+year+"_PIN_TaxCode_Frequency","Polygon","CondoTable_Kane_"+year+"_PIN_TaxCode_Max_TaxCode","Polygon")
arcpy.SelectLayerByAttribute_management("CondoTable_Kane_"+year+"_PIN_TaxCode_Frequency","NEW_SELECTION","CondoTable_Kane_"+year+'_PIN_TaxCode_Max_TaxCode.FREQUENCY > 1')

#### For each Polygon PIN, Manually Unselect the row with HIGHEST frequency (and non-null tax code);  ####
#### then switch selection and export as "CondoTable_Kane_"+year+"_PIN_Final_TaxCodes"                ####

# Add Parcel join and export condo parcels
arcpy.AddJoin_management("Parcels_Kane_"+year, "PIN", "CondoTable_Kane_"+year+"_PIN_Final_TaxCodes", "Polygon")
arcpy.SelectLayerByAttribute_management("Parcels_Kane_"+year, "NEW_SELECTION", "CondoTable_Kane_"+year+'_PIN_Final_TaxCodes.TAX_CODE IS NOT NULL')
arcpy.FeatureClassToFeatureClass_conversion("Parcels_Kane_"+year, arcpy.env.workspace, 'Kane_'+year+'_CondoParcels_with_TaxCodes')

# Select and export Non-Condo Parcels, add/calculate PIN field
arcpy.SelectLayerByAttribute_management("Parcels_Kane_"+year, "SWITCH_SELECTION")
arcpy.FeatureClassToFeatureClass_conversion("Parcels_Kane_"+year, arcpy.env.workspace, 'Kane_'+year+'_NonCondoParcels')

# Join, add, calc Tax Code
arcpy.AddJoin_management("Kane_"+year+"_NonCondoParcels", "PIN", "AssessorData_Kane_"+year,"PIN")
arcpy.CalculateField_management("Kane_"+year+"_NonCondoParcels", "Kane_"+year+"_NonCondoParcels.TAX_CODE", "!AssessorData_Kane_"+year+".Tax_Code!", "PYTHON")
arcpy.RemoveJoin_management("Kane_"+year+"_NonCondoParcels")

# Merge and Dissolve to create clean poly tax code
arcpy.Merge_management("Kane_"+year+"_CondoParcels_with_TaxCodes;Kane_"+year+"_NonCondoParcels",'Kane_'+year+'_TaxCode_Parcels_Merged')
arcpy.SelectLayerByAttribute_management("Kane_"+year+"_TaxCode_Parcels_Merged","NEW_SELECTION",'TAX_CODE IS NOT NULL')
arcpy.Dissolve_management("Kane_"+year+"_TaxCode_Parcels_Merged",'Kane_'+year+'_TaxCode_Dissolved',"TAX_CODE",'#',"SINGLE_PART")


# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion('Kane_'+year+'_TaxCode_Dissolved',"TAX_CODE",'Kane_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("Kane_TaxCodes_"+year+"_15ft",'FocalMaj_NewPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('Kane_TaxCodes_'+year+'_15ft', 'ANYNODATA')
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
	    if i == 10:
	        break

# Convert to polygon
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\GIS.gdb'
arcpy.RasterToPolygon_conversion(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),'Kane_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("Kane_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'Kane_TaxCodes_grid_attributes')
arcpy.AddField_management("Kane_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Kane_TaxCodes_"+year+"_poly","gridcode","Kane_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("Kane_TaxCodes_"+year+"_poly","Kane_TaxCodes_"+year+"_poly.TAX_CODE",'!Kane_TaxCodes_grid_attributes.TAX_CODE!','PYTHON')
arcpy.RemoveJoin_management("Kane_TaxCodes_"+year+"_poly")

# Clip by County Boundary
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '089'")
arcpy.Clip_analysis("Kane_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'Kane_TaxCodes_'+year+'_poly_clipped')

# Erase county boundary then manually explode/copy/paste/merge back to _poly_clipped #
arcpy.Erase_analysis("Cnty7_NIPC_05", "Kane_TaxCodes_"+year+"_poly_clipped", 'Kane_TaxCodes_'+year+'_poly_clip_erase_County')

arcpy.TableToTable_conversion("Kane"+year+"$",arcpy.env.workspace,'Kane_TaxCodes_Descriptive')
arcpy.AddJoin_management("Kane_TaxCodes_"+year+"_poly_clipped","TAX_CODE","Kane_TaxCodes_Descriptive","tax_code")

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'
arcpy.FeatureClassToFeatureClass_conversion("Kane_TaxCodes_"+year+"_poly_clipped", arcpy.env.workspace, 'Kane_TaxDistricts_'+year)

del_field_list = ['Id','gridcode','OBJECTID','tax_code_1']
for del_field in del_field_list:
	arcpy.DeleteField_management('Kane_TaxDistricts_'+year, del_field)
