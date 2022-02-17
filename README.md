# Effective Property Tax Rates

This analysis package calculates effective property tax rates for all geographies within the CMAP region's 7 counties. It attempts to be as comprehensive as possible, seeking to identify property tax rates for all known districts. The only general exception to this is the absence of non-ad valorem special service areas, which the analysis discounts. (These are districts where extensions are not assigned to property owners by property value.)

## Project status
As of February 18th, 2022, effective tax rate analysis for tax year 2018 is complete.

## Repository structure
This repository is structured with 5 main folders. Many of these folders contain readmes that further explain each folder's contents. In general:

1. **scripts** contains the R scripts that run the analysis and produce outputs.
2. **raw** contains source files used by the `1_extract_data.R`, which processes these files for later use. All files stored in this directory are unchanged from their native source. Most are downloaded from public websites of various county clerks, while some are collected via communication with county staff.
3. **resources** contains source files used by various scripts that are either created by CMAP staff or modified by CMAP staff before use.
4. **internal** is a repository for files created by this analysis for use in later scripts. For example, the `1_extract_data.R` script saves many files into this folder for use by the second and third scripts.
5. **outputs** contains excel files produced by this analysis

## Branch structure and future plans
Currently, this repo contains only one branch. However, following the lead of other CMAP repos, I imagine eventually archiving this branch as `2018 final analysis` or somesuch and then updating the analysis in the main branch to work for 2020 (and so on/so forth). Note that the [raw/For 2020](raw/For 2020/) folder contains some of the necessary 2020 input files already. 

Additionally, there is one part of this analysis that has always been handled by the R&A team -- this is the conversion of outputs from `2_process_taxcodes.R` into a shapefile that delineates the geographies of every tax code in teh region. THis has been handled by a python script located on the CMAP S drive at `S:\Projects_FY21\Policy Development and Analysis\Tax Analysis\Property tax data\2018 Data\tax_district_analysis\proc\`. It may make sense to integrate that script into this repository in the future.
