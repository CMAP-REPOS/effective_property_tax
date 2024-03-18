# Resource files

This directory contains files created and maintained or downloaded and modified by CMAP staff.

## lake_code_translation.xlsx
Lake County changed the format of their tax codes names between 2018 and 2021. The previous code used 2018 data so this translation was provided rather than re-writing the entire core. Matching was done manually as translating between the two formats was relatively straightforward. 

## NamingTable.csv
This file has been maintained by CMAP staff since the first round of effective rate analysis in 2014. The purpose of this file is to align disparate naming conventions across source files so that text-based matching can be used on district names. Primarily, input data is matched on `Name`, and then that name is replaced with `IDOR Name`. `Type of District` is additionally introduced. Some notes:

- Kane, Lake, and McHenry counties match on some sort of naming code rather than an actual name. 
- The spreadsheet must be sorted alpha by county upon import into R, but the code could be easily tweaked to do this sorting in R after import.
- Even though the column is titled `IDOR Name`, this field is also used to correct naming between tax code and extension input data -- so especially in the case of SSAs the `IDOR Name` may not actually be used by IDOR.  
- There are opportunities to improve on how this matching is done, including by shifting more/all counties to a code-to-IDOR-name match, and/or joining based on code rather than name altogether.

## property classes.xlsx
This staff-maintained file converts property class codes used by each counties in their assessor files into more legible descriptions, and categorizes codes into the larger buckets used by this analysis. These buckets attempt to mirror IDOR categorization. Cook and McHenry counties include assessment ratio columns, as in these locations assessment varies by property class:

- Cook county's assessment ratios are mostly directly from statute. The exception to this is the incentive classification assessment ratios. By statute, most of these increase from 10% after 10 years if not renewed to 15% and 20% for one year each before losing the incentive class and reverting to 25%. The ratios for these classes are averages based on an analysis of 2019 data from the Cook Assessor performed by CMAP staff during 2021.
- The two 1:1 assessments in McHenry County are based on CMAP staff's interpretation of state statute. If correct, these rules for wind turbines and wooded transition areas apply statewide, but our analysis has historically only found them in McHenry County.

## ptaxsim.db and ptaxsim-2021.0.4 (2).db
These are database files downloaded from the [Cook County Assessor PTaxSim Github Page](https://github.com/ccao-data/ptaxsim). The database used in this analysis was downloaded on December 15, 2023. 
