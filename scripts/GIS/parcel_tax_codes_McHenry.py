## McHenry Processing Scenario: Tax Code present in Assessor table.  Condo table present. ##

from arcpy.sa import *

# Year variable
year = '2018'
yearmonth = year  #'201612' <-- old format, no longer used in McHenry filenames

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Export condo table, add field that can join to Assessor Table (no dashes), dissolve
arcpy.TableToTable_conversion("CondoParcels",arcpy.env.workspace,"CondoTable_McHenry_"+yearmonth)
arcpy.AddField_management("CondoTable_McHenry_"+yearmonth,'PIN_JOINER',"TEXT",'#','#',25)
arcpy.CalculateField_management("CondoTable_McHenry_"+yearmonth,"PIN_JOINER",'str(!PIN!).replace("-","")','PYTHON')
arcpy.Statistics_analysis("CondoTable_McHenry_"+yearmonth,"CondoTable_McHenry_"+yearmonth+"_UniquePIN","PIN COUNT","PIN_JOINER;PRIMARYPIN")

# Join to Assessor table, Add/Calc tax code
arcpy.AddField_management("CondoTable_McHenry_"+yearmonth+"_UniquePIN", "TAX_CODE", "TEXT", '#', '#', 25)
arcpy.AddJoin_management("CondoTable_McHenry_"+yearmonth+"_UniquePIN","PIN_JOINER","AssessorData_McHenry_"+year,"Parcel_Number")
arcpy.CalculateField_management("CondoTable_McHenry_"+yearmonth+"_UniquePIN","CondoTable_McHenry_"+yearmonth+"_UniquePIN.TAX_CODE",'!AssessorData_McHenry_'+year+'.Tax_Code!.replace("-","")',"PYTHON")
arcpy.RemoveJoin_management("CondoTable_McHenry_"+yearmonth+"_UniquePIN")
arcpy.AddJoin_management("Parcels_McHenry_"+yearmonth,"PIN","CondoTable_McHenry_"+yearmonth+"_UniquePIN","PRIMARYPIN")
arcpy.SelectLayerByAttribute_management("Parcels_McHenry_"+yearmonth,"NEW_SELECTION",'TAX_CODE IS NOT NULL')

# Get most frequent tax code by primary pin, select PINs with multiple tax codes
arcpy.Statistics_analysis("CondoTable_McHenry_"+yearmonth+"_UniquePIN",'CondoTable_McHenry_'+yearmonth+'_PIN_TaxCode_Frequency',"TAX_CODE COUNT","PRIMARYPIN;TAX_CODE")
arcpy.Statistics_analysis("CondoTable_McHenry_"+yearmonth+"_PIN_TaxCode_Frequency",'CondoTable_McHenry_'+yearmonth+'_PIN_TaxCode_Max_TaxCode',"TAX_CODE COUNT","PRIMARYPIN")
arcpy.AddJoin_management("CondoTable_McHenry_"+yearmonth+"_PIN_TaxCode_Frequency","PRIMARYPIN","CondoTable_McHenry_"+yearmonth+"_PIN_TaxCode_Max_TaxCode","PRIMARYPIN")
arcpy.SelectLayerByAttribute_management("CondoTable_McHenry_"+yearmonth+"_PIN_TaxCode_Frequency","NEW_SELECTION","CondoTable_McHenry_"+yearmonth+'_PIN_TaxCode_Max_TaxCode.FREQUENCY > 1')


#### Manually Unselect the PINs with HIGHER frequencies; switch selection and export as "CondoTable_McHenry_"+yearmonth+"_PIN_Final_TaxCodes"  ####


# Add Parcel join and export condo parcels
arcpy.AddJoin_management("Parcels_McHenry_"+yearmonth,"PIN","CondoTable_McHenry_"+yearmonth+"_PIN_Final_TaxCodes","PRIMARYPIN")
arcpy.SelectLayerByAttribute_management("Parcels_McHenry_"+yearmonth,"NEW_SELECTION","CondoTable_McHenry_"+yearmonth+'_PIN_Final_TaxCodes.TAX_CODE IS NOT NULL')
arcpy.FeatureClassToFeatureClass_conversion("Parcels_McHenry_"+yearmonth,arcpy.env.workspace,'McHenry_'+yearmonth+'_CondoParcels_with_TaxCodes')

# Select and export Non-Condo Parcels, add/calculate PIN field
arcpy.SelectLayerByAttribute_management("Parcels_McHenry_"+yearmonth,"SWITCH_SELECTION")
arcpy.FeatureClassToFeatureClass_conversion("Parcels_McHenry_"+yearmonth,arcpy.env.workspace,'McHenry_'+yearmonth+'_NonCondoParcels')
arcpy.CalculateField_management("McHenry_"+yearmonth+"_NonCondoParcels","PIN_JOINER",'str(!PIN!).replace("-","")',"PYTHON")

# Join, add, calc Tax Code
arcpy.AddJoin_management("McHenry_"+yearmonth+"_NonCondoParcels","PIN_JOINER","AssessorData_McHenry_"+year,"Parcel_Number")
arcpy.CalculateField_management("McHenry_"+yearmonth+"_NonCondoParcels", "McHenry_"+yearmonth+"_NonCondoParcels.TAX_CODE", "!AssessorData_McHenry_"+year+".Tax_Code!.replace('-','')", "PYTHON")
arcpy.RemoveJoin_management("McHenry_"+yearmonth+"_NonCondoParcels")

# Merge and Dissolve to create clean poly tax code
arcpy.Merge_management("McHenry_"+yearmonth+"_CondoParcels_with_TaxCodes;McHenry_"+yearmonth+"_NonCondoParcels",'McHenry_'+yearmonth+'_TaxCode_Parcels_Merged')
arcpy.SelectLayerByAttribute_management("McHenry_"+yearmonth+"_TaxCode_Parcels_Merged","NEW_SELECTION",'TAX_CODE IS NOT NULL')
arcpy.Dissolve_management("McHenry_"+yearmonth+"_TaxCode_Parcels_Merged",'McHenry_'+yearmonth+'_TaxCode_Dissolved',"TAX_CODE",'#',"SINGLE_PART")

# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion("McHenry_"+yearmonth+"_TaxCode_Dissolved","TAX_CODE",'McHenry_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("McHenry_TaxCodes_"+year+"_15ft",'FocalMaj_NewPass0')

from arcpy.sa import *
# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management("McHenry_TaxCodes_"+year+"_15ft", 'ANYNODATA')
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
arcpy.CopyRaster_management(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),"McHenry_TaxCodes_"+year+"_Expanded")
arcpy.RasterToPolygon_conversion("McHenry_TaxCodes_"+year+"_Expanded",arcpy.env.workspace + r"/McHenry_TaxCodes_"+year+"_poly","NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("McHenry_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'McHenry_TaxCodes_grid_attributes')
arcpy.AddField_management("McHenry_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("McHenry_TaxCodes_"+year+"_poly","gridcode","McHenry_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("McHenry_TaxCodes_"+year+"_poly","McHenry_TaxCodes_"+year+"_poly.TAX_CODE",'!McHenry_TaxCodes_grid_attributes.TAX_CODE!','PYTHON')
arcpy.RemoveJoin_management("McHenry_TaxCodes_"+year+"_poly")

# Import and join tax code table, clip and export to "Results" GDB
arcpy.TableToTable_conversion("McHenry"+year+"$",arcpy.env.workspace,'McHenry_TaxCodes_Descriptive')
arcpy.AddJoin_management("McHenry_TaxCodes_"+year+"_poly","TAX_CODE","McHenry_TaxCodes_Descriptive","tax_code")
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '111'")

#arcpy.env.workspace = r'S:\AdminGroups\PlanDevelopment\Data_Tool_Analysis_Needs\LRP_DTD\strat_tax\data\output\Results.gdb'
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'
arcpy.Clip_analysis("McHenry_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'McHenry_TaxDistricts_'+year)
del_field_list = ['McHenry_TaxCodes_'+year+'_poly_Id','McHenry_TaxCodes_'+year+'_poly_gridcode','McHenry_TaxCodes_Descriptive_OBJECTID','McHenry_TaxCodes_'+year+'_poly_TAX_CODE']
for del_field in del_field_list:
	arcpy.DeleteField_management('McHenry_TaxDistricts_'+year, del_field)
