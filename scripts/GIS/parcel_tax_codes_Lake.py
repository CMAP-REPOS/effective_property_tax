##### Lake Processing Scenario: Tax Code in separate table.  Stacked polygons present. ######

from arcpy.sa import *

# Year variable
year = '2018'

# Set workspace
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'

arcpy.AddField_management("Parcels_Lake_"+year, "Tax_Code", "TEXT", field_length=20)

## Read parcel tax codes from assessor table into dict, then write to parcel polys
parcel_taxcode = {}
with arcpy.da.SearchCursor("AssessorData_Lake_"+year, ['PIN', 'tax_code']) as c:
    for r in c:
        pin = r[0].replace('-', '')
        taxcode = r[1].split('-')[0]
        parcel_taxcode[pin] = taxcode
with arcpy.da.UpdateCursor("Parcels_Lake_"+year, ["PIN", "Tax_Code"]) as c:
    for r in c:
        r[1] = parcel_taxcode.get(r[0], None)
        c.updateRow(r)

# Convert parcels to centroids (same as Kendall)
arcpy.FeatureToPoint_management("Parcels_Lake_"+year,'Parcels_Lake_'+year+'_TaxCode_centroids',"INSIDE")

# Calculate x/y centroid of polygon and Dissolve Parcels based on coord to get unique geogs
arcpy.AddField_management("Parcels_Lake_"+year, 'COORD', "TEXT", field_length=200)
with arcpy.da.UpdateCursor("Parcels_Lake_"+year, ["SHAPE@XY", "COORD"]) as c:
    for r in c:
        r[1] = str(r[0])
        c.updateRow(r)
arcpy.Dissolve_management("Parcels_Lake_"+year, 'Parcels_Lake_unique_polygons', "COORD")

### Open ArcGIS Pro and run Summarize Within tool ###
arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb'
arcpy.SummarizeWithin_analysis('Parcels_Lake_unique_polygons', 'Parcels_Lake_'+year+'_TaxCode_centroids', 'Lake_Parcels_'+year+'_SummarizeWithin_CondoPoints', 'KEEP_ALL','#','#','#', 'Tax_Code', 'ADD_MIN_MAJ')
arcpy.CalculateField_management("Lake_Parcels_"+year+"_SummarizeWithin_CondoPoints", "Majority_Tax_Code", '!Majority_Tax_Code!.rpartition(";")[2]', 'PYTHON')

# Convert tax code polygon to raster, make copy for focal majority fill input
arcpy.SelectLayerByAttribute_management("Lake_Parcels_"+year+"_SummarizeWithin_CondoPoints","NEW_SELECTION","Majority_Tax_Code <> 'None'")
arcpy.PolygonToRaster_conversion("Lake_Parcels_"+year+"_SummarizeWithin_CondoPoints","Majority_Tax_Code",'Lake_TaxCodes_'+year+'_15ft',"CELL_CENTER","NONE",15)
arcpy.CopyRaster_management("Lake_TaxCodes_"+year+"_15ft",'FocalMaj_NewPass0')

# Generate initial noData flag
noDataFlag = arcpy.GetRasterProperties_management('Lake_TaxCodes_'+year+'_15ft', 'ANYNODATA')
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
	        ##EK changed this to 100 from 10
	    if i == 100:
	        break

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\GIS.gdb'

arcpy.CopyRaster_management(r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass'+str(i-1),'Lake_TaxCodes_'+year+'_Expanded')
arcpy.RasterToPolygon_conversion(r"S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\proc.gdb\FocalMaj_SecondPass"+str(i-1),'Lake_TaxCodes_'+year+'_poly',"NO_SIMPLIFY")

# Export/join raster attribute table and calculate Tax Code field
arcpy.TableToTable_conversion("Lake_TaxCodes_"+year+"_15ft",arcpy.env.workspace,'Lake_TaxCodes_grid_attributes')
arcpy.AddField_management("Lake_TaxCodes_"+year+"_poly",'TAX_CODE',"TEXT",'#','#',20)
arcpy.AddJoin_management("Lake_TaxCodes_"+year+"_poly","gridcode","Lake_TaxCodes_grid_attributes","Value")
arcpy.CalculateField_management("Lake_TaxCodes_"+year+"_poly","Lake_TaxCodes_"+year+"_poly.TAX_CODE",'!Lake_TaxCodes_grid_attributes.Majority_Tax_Code!','PYTHON')
arcpy.RemoveJoin_management("Lake_TaxCodes_"+year+"_poly")

# Clip by County Boundary
arcpy.SelectLayerByAttribute_management("Cnty7_NIPC_05","NEW_SELECTION","FIPSCNTY = '097'")
arcpy.Clip_analysis("Lake_TaxCodes_"+year+"_poly","Cnty7_NIPC_05",'Lake_TaxCodes_'+year+'_poly_clipped')

# Erase county boundary then explode/merge/eliminate
arcpy.Erase_analysis("Cnty7_NIPC_05", "Lake_TaxCodes_"+year+"_poly_clipped", 'Lake_TaxCodes_'+year+'_poly_clip_erase_County')
arcpy.MultipartToSinglepart_management('Lake_TaxCodes_'+year+'_poly_clip_erase_County', "Lake_TaxCodes_"+year+"_Explode")
arcpy.Merge_management("Lake_TaxCodes_"+year+"_poly_clipped;Lake_TaxCodes_"+year+"_Explode",'Lake_TaxCodes_Merged')
arcpy.SelectLayerByAttribute_management("Lake_TaxCodes_Merged","NEW_SELECTION",'TAX_CODE IS NULL')
arcpy.Eliminate_management("Lake_TaxCodes_Merged",'Lake_TaxCodes_Merged_Eliminate')
#manually exported this to the results gdb

arcpy.TableToTable_conversion("Lake"+year+"$",arcpy.env.workspace,'LakeTaxCodesDescriptive_'+year)

arcpy.env.workspace = r'S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\data\output\results.gdb'
arcpy.AddJoin_management('Lake_TaxCodes_Merged_Eliminate', "TAX_CODE", "LakeTaxCodesDescriptive_"+year, "tax_code")

## Final export
arcpy.FeatureClassToFeatureClass_conversion('Lake_TaxCodes_Merged_Eliminate', arcpy.env.workspace, 'Lake_TaxDistricts_'+year)
del_field_list = ['Id','gridcode','FIPSCNTY','CNTYNAME','CNTYNAME_U','COUNTIES','ORIG_FID','OBJECTID','tax_code_1']
for del_field in del_field_list:
	arcpy.DeleteField_management('Lake_TaxDistricts_'+year, del_field)
