# Resource files

This directory contains files created and maintained or downloaded and modified by CMAP staff.

## lake_twp_dists.csv
This file is created and mantained by CMAP staff to introduce township-based districts not included as of 2018 in the raw data available from Lake County. RBTs are township road and bridge districts; GRVs are special road improvement districts. I would like additional clarity from Lindsay Hollander as to how these were determined and how they can be checked in the future before she departs.

## NamingTable.csv
This file has been maintained by CMAP staff since the first round of effective rate analysis in 2014. The purpose of this file is to align disparate naming conventions across source files so that text-based matching can be used on district names. Primarily, input data is matched on `Name`, and then that name is replaced with `IDOR Name`. `Type of District` is additionally introduced. Some notes:

- Kane, Lake, and McHenry counties match on some sort of naming code rather than an actual name. 
- The spreadsheet must be sorted alpha by county upon import into R, but the code could be easily tweaked to do this sorting in R after import.
- Even though the column is titled `IDOR Name`, this field is also used to correct naming between tax code and extension input data -- so especially in the case of SSAs the `IDOR Name` may not actually be used by IDOR.  
- There are opportunities to improve on how this matching is done, including by shifting more/all counties to a code-to-IDOR-name match, and/or joining based on code rather than name altogether.

## property classes.xlsx
This staff-maintained file converts property class codes used by each counties in their assessor files into more legible descriptions, and categorizes codes into the larger buckets used by this analysis. These buckets attempt to mirror IDOR categorization. Cook and McHenry counties include assessment ratio columns, as in these locations assessment varies by property class:

- Cook county's assessment ratios are mostly directly from statute. The exception to this is the incentive classification assessment ratios. By statute, most of these increase from 10% after 10 years if not renewed to 15% and 20% for one year each before losing the incentive class and reverting to 25%. The ratios for these classes are averages based on an analysis of 2019 data from the Cook Assessor performed by Austen Edwards during 2021.
- The two 1:1 assessments in McHenry County are based on Lindsay Hollander's interpretation of state statute. If correct, these rules for wind turbines and wooded transition areas apply statewide, but our analysis has historically only found them in McHenry County.

## Y2018Tbl28.xlsx
Table 28 is published by IDOR and is available [here](https://www2.illinois.gov/rev/research/taxstats/PropertyTaxStatistics/Pages/default.aspx). This table contains extensions by land use/property class for every taxing district in the state. However, this file cannot be used as published because it is modified by CMAP staff. The issue here is that Table 28 in its original form includes SSAs within muni and county extensions. This is bad news for effective rate calculation because not all county/muni taxpayers pay into those SSAs. IDOR table 27 contains (among other things) SSA extension totals for various units of government in IL. Table 28 is modified to remove SSAs from the topline totals, so that that SSA extensions can be applied to the specific tax codes where they are levied. Because this file is modified by CMAP staff, it is stored in `resources` rather than `raw`. Future iterations of this script could be improved upon to do this table 27-based SSA removal in R, rather than in excel.

Note that in the City of Chicago, Home Equity Assurance Districts are treated like SSAs and are also removed. It is difficult to locate these in Table 27 -- in 2018 they are categorized as BINDS AND INTEREST. For future years, your best bet is to look for these districts in the Cook County Extension Detail Report and match the values to Table 27 line items.