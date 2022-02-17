# Output files

This directory contains final outputs of scripted analysis. 

## Raw districts by tax code
**1_dists_by_taxcode_raw.xlsx** contains clean inputs of each county's "districts by taxcode" source file. There is one sheet per county. This is a good place to evaluate the success of PDF interpretation via spot checking to county source files. This output is produced by `1_extract_data.R`.

## Processed districts by tax code
**2_dists_by_taxcode_proc_*.xlsx** are the processed, final versions of the districts by taxcode analysis. There is one workbook for each county. This file is produced by `2_process_taxcodes.R`. Each workbook contains three sheets:

- The `output` worksheet in each file is a "wide" format table that lists taxing districts, categorized by type and with improved names (from the naming table), for each tax code. 
- The `report` worksheet lists all of the unique values from the `output` sheet, for QA/QC. 
- The `not_included` worksheet contains the taxing districts that the analysis ignores. Inspecting these values is an important QA/QC step, as no districts that have unique property tax extensions should be listed. If something significant is missing, that county's script must be revisited.

## Effective rates
**3_effective_rates_*.xlsx** are the final effective rates calculated by the script. There is one workbook for each county. This file is produced by `3_calc_mv_eff_rates.R`. Each workbook contains four sheets:

- `eff_rates - taxcode` contains the final residential and commercial/industrial effective rates for the tax code. In the future, this could be expanded to include the tax code's total MV for each land use as well, as this would help the reader contextualize the significance of the effective rate.
- `eff_rates - district` contains the final residential and commercial/industrial effective rates for each taxing district. These are the rates that are summed to produce the tax-code results based on the districts by taxcode analysis done in step 2. This spreadsheet also includes also the market values and extensions that are used to calculate the effective rates. It may be helpful to sum non-commercial/industrial/residential/vacant market values into an other bucket for legibility -- this is already done for extensions. 
- `dists without exts` are districts identified by the market value analysis but do not have matching extensions. This is often the case for inactive districts, or districts whose extensions are embedded into others. (e.g. general assistance extensions are included in table 28 township data, municipal library extensions in table 28 muni data.) These should be inspected to make sure no districts that should have extensions are listed here.
- `dists without MVs` are districts identified by the extension analysis but do not have matching market values -- in other words, the analysis finds they are not in any taxcodes, or there is no market value located in the taxcodes they are in. This is problematic if the extension is non-zero.