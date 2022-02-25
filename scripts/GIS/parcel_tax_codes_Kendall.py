##### Kendall Processing Scenario: Tax Code in separate table.  Stacked polygons present. ######

from arcpy.sa import *

# Year variable
year = '2018'

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

# Convert csvs to table
arcpy.TableToTable_conversion("Kendall"+year+"$",arcpy.env.workspace,'Kendall_TaxCodes_'+year)
arcpy.TableToTable_conversion("AssessorData_Kendall_"+year, arcpy.env.workspace, 'Kendall_Name_and_Address_'+year)
f_info = arcpy.Describe('Kendall_Name_and_Address_'+year).fields
for f in f_info:
    if not (f.name in ['property_key', 'tax_code'] or f.type == 'OID'):
        arcpy.DeleteField_management('Kendall_Name_and_Address_'+year, f.name)

# Calculate new PIN field
arcpy.AddField_management("Kendall_Name_and_Address_"+year, "PIN_NO", "TEXT", field_length=20)
arcpy.CalculateField_management("Kendall_Name_and_Address_"+year, "PIN_NO", "'0'+str(!property_key!)[0:1]+'-'+str(!property_key!)[1:3]+'-'+str(!property_key!)[3:6]+'-'+str(!property_key!)[-3:]", "PYTHON")

# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "Kendall_Name_and_Address_"+year

# Join Parcels to table that includes Tax Codes, export records with Tax Codes
arcpy.AddJoin_management("Parcels_Kendall_"+year,"PIN","Kendall_Name_and_Address_"+year,"PIN_NO")
arcpy.FeatureClassToFeatureClass_conversion("Parcels_Kendall_"+year,arcpy.env.workspace,'Parcels_Kendall_'+year+'_jn_TaxCode','Kendall_Name_and_Address_'+year+'.tax_code IS NOT NULL')

# Convert parcels to centroids
arcpy.FeatureToPoint_management("Parcels_Kendall_"+year+"_jn_TaxCode",'Parcels_Kendall_'+year+'_TaxCode_centroids',"INSIDE")

# Calculate x/y centroid of polygon and Dissolve Parcels based on coord to get unique geogs
arcpy.AddField_management("Parcels_Kendall_"+year+"_jn_TaxCode",'COORD',"TEXT",'#','#',200)
with arcpy.da.UpdateCursor("Parcels_Kendall_"+year+"_jn_TaxCode", ["SHAPE@XY", "COORD"]) as c:
    for r in c:
        r[1] = str(r[0])
        c.updateRow(r)
arcpy.Dissolve_management("Parcels_Kendall_"+year+"_jn_TaxCode", 'Parcels_Kendall_unique_polygons', "COORD")

### Open ArcGIS Pro and run Summarize Within tool ###
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'
arcpy.SummarizeWithin_analysis('Parcels_Kendall_unique_polygons', 'Parcels_Kendall_'+year+'_TaxCode_centroids','Kendall_Parcels_'+year+'_SummarizeWithin_CondoPoints', 'KEEP_ALL','#','#','#', 'tax_code', 'ADD_MIN_MAJ')

# Add join and Dissolve based on Majority Tax Code
arcpy.Dissolve_management("Kendall_Parcels_"+year+"_SummarizeWithin_CondoPoints",'Parcels_Kendall_Dissolve_TaxCode',"Majority_tax_code",'#',"SINGLE_PART")

# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.PolygonToRaster_conversion("Parcels_Kendall_Dissolve_TaxCode","Majority_tax_code",'Kendall_Tax_Code_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("Kendall_Tax_Code_"+year+"_15ft",'FocalMaj_NewPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('Kendall_Tax_Code_'+year+'_15ft', 'ANYNODATA')
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
	    if i == 30:
	        break

# Convert to polygon
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\GIS.gdb'
arcpy.RasterToPolygon_conversion("rastOut",'Kendall_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

### Manually edit spurious tax codes

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("Kendall_Tax_Code_"+year+"_15ft",arcpy.env.workspace,'Kendall_Tax_Code_grid_attributes')
arcpy.AddField_management("Kendall_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",20)
arcpy.AddJoin_management("Kendall_TaxCodes_"+year+"_poly","gridcode","Kendall_Tax_Code_grid_attributes","Value")
arcpy.CalculateField_management("Kendall_TaxCodes_"+year+"_poly","Kendall_TaxCodes_"+year+"_poly.TAX_CODE",'!Kendall_Tax_Code_grid_attributes.Majority_tax_code!','PYTHON')
arcpy.RemoveJoin_management("Kendall_TaxCodes_"+year+"_poly")

# Clip by County Boundary
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '093'")
arcpy.Clip_analysis("Kendall_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'Kendall_Tax_Code_'+year+'_poly_clipped')

# Erase county boundary to check for unclassified areas then manually explode/copy/paste/merge #
arcpy.Erase_analysis("Cnty7_NIPC_05", "Kendall_Tax_Code_"+year+"_poly_clipped", 'Kendall_TaxCodes_'+year+'_poly_clip_erase_County')

arcpy.AddJoin_management("Kendall_Tax_Code_"+year+"_poly_clipped","TAX_CODE","Kendall_TaxCodes_"+year,"TAX_CODE")
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\Results.gdb'

arcpy.Clip_analysis("Kendall_Tax_Code_"+year+"_poly_clipped","Cnty7_NIPC_05",'Kendall_TaxDistricts_'+year)

del_field_list = ['Kendall_Tax_Code_'+year+'poly_clipped_Id','Kendall_Tax_Code_'+year+'_poly_clipped_gridcode','Kendall_TaxCodes_'+year+'_OBJECTID','Kendall_TaxCodes_'+year+'_poly_clipped_TAX_CODE']
for del_field in del_field_list:
	arcpy.DeleteField_management('Kendall_TaxDistricts_'+year, del_field)

##manually deleted two fields
# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "Kendall_TaxDistricts_"+year
arcpy.DeleteField_management(in_table="Kendall_TaxDistricts_"+year, drop_field="Kendall_TaxCodes_"+year+"_poly_clipped_TAX_CODE")
# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "Kendall_TaxDistricts_"+year
arcpy.DeleteField_management(in_table="Kendall_TaxDistricts_"+year, drop_field="Kendall_TaxCodes_"+year+"_OBJECTID")

### Begin editing session and manually explode etc ###
