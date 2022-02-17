# Analysis scripts

## 0_naming_table_builder.R
This is a helper file with a simple function in it that imports the naming table from excel into the current R environment. It is used by scripts 2 and 3.

## 1_extract_data.R
This data extracts and processes data from various sources, including the CMAP V Drive, this repo's resources directory, and this repo's raw directory.

## 2_process_taxcodes.R
This script repeats similar analyses on data from each county, but each is performed separately because of format differences and policy distinctions across counties. In general, the flow of this script follows a pattern, with QA/QC stops in italics:

1. Filter list of taxcodes & districts so it includes only tax codes that actually have PINs in them.
2. Join districts to naming table to correct names and obtain district types
3. *check for extra rows to confirm that naming table join did not duplicate any districts.*
4. Manually assign district types to districts that did not join to the naming table
5. *identify districts that are still missing types, as these will be ignored by the rest of the analysis.*
6. Process, reshape, and clean data
7. Export the data to excel

## 3_calc_mv_eff_rates.R
This script steps through a multi-step process to calculate extensions and market values by land use, and then divide extensions by market values to get effective rates. Unlike step 2, this process is essentially identical for every county, so this script is structured mostly by building named or anonynous functions and then mapping those functions across all counties simultaneously. This script goes through the following steps, with QA/QC stops in italics:

1. Join PIN list to class table, use classes to estimate market value from assessed value, and summarize by tax code and category producing estimated market values by land use for each tax code (e.g. tax code 01001 has $1.2 M of residential MV),
2. Take the output from script 2 above and pivot it from a wide format to a long format.
3. Join extension data to naming table to correct names and obtain district types
4. Combine table 28 data with extension data that is filtered to keep only district types that don't exist in table 28 
5. Join MVs by land use and taxcode to districts by taxcode and then summarize (via pivot) the MVs in each taxcode to the district level (producing market value totals for each taxing district). Join this with the extension data, which is already formatted at the district level.
6. *Identify taxing districts that have extensions but are missing market values*
7. *Identify taxing districts that have market values but are missing extensions*
8. Calculate effective rates for each district by dividing extensions by market values
9. *Identify any taxing districts with infinite rates*
10. Re-introduce the districts by tax code data to summarize effective rates to the taxcode
11. Write final outputs.

