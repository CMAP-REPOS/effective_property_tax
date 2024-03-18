# Effective Property Tax Rates

This analysis package calculates effective property tax rates for all geographies within the CMAP region's 7 counties. It attempts to be as comprehensive as possible, seeking to identify property tax rates for all known districts. The only general exception to this is the absence of non-ad valorem special service areas, which the analysis discounts. (These are districts where extensions are not assigned to property owners by property value.)

## Project status
As of February 26th, 2024, effective tax rate analysis for tax year 2021 is complete.

## Repository structure
This repository is structured with 5 main folders. Many of these folders contain readmes that further explain each folder's contents. In general:

1. **scripts** contains the R scripts that run the analysis and produce outputs.
2. **raw** contains source files used by the `1_extract_data.R`, which processes these files for later use. All files stored in this directory are unchanged from their native source. Most are downloaded from public websites of various county clerks, while some are collected via communication with county staff.
3. **resources** contains source files used by various scripts that are either created by CMAP staff or modified by CMAP staff before use.
4. **internal** is a repository for files created by this analysis for use in later scripts. For example, the `1_extract_data.R` script saves many files into this folder for use by the second and third scripts.
5. **outputs** contains excel files and shapefiles produced by this analysis

Note that this repo is almost but not entirely self-sufficient. `1_extract_data.R` relies on county assessor files stored on CMAP's internal V drive, with filepaths hard coded directly into the script. This analysis cannot be updated until all files in `raw` and `resources` have been updated to the correct tax year, AND CMAP has obtained and internally published all seven county assessor files for the given tax year.

## Branch structure and future plans
This repo contains only two branches, one for 2018 and one for 2021. 

There is one part of this analysis that has previously been handled by CMAP’s Research & Analysis team, but is now handled by the script "4_generate_tax_maps.R". This is the conversion of outputs from `2_process_taxcodes.R` into a shapefile that delineates the geographies of every tax code in the region.