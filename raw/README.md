# Raw input files

The files in this directory are collected directly from County Clerks. There are two files for each county:

1. Tax code reports list taxing districts in each tax code. These are key data sources for the `2_process_taxcodes.R` script. They are all publicly available. 
2. extension reports from which extension by land use are collected or calculated for ad-valorem SSAs and other districts without extension data in Table 28. In the case of all counties except for Lake and Will, this data comes from extension detail reports that include all districts in the county. Lake and Will have no publicly available data that includes SSAs, so their spreadsheets are obtained via private communication with Clerk staff.

The one exception to this is Will County. For 2018 and prior years, the county only seems to post township-specific PDFs which need to be downloaded and combined. The folder `Will Townships 2018` contains these PDFs. The file `Will All Townships 2019.pdf` is here for posterity--it is the single combined PDF for tax year 2019 which was accidentally used until late in the analysis. Interpretation of both the 2019 file and the combination of 2018 files seems to yield identical results in terms of tax codes and districts.

## Data sources
As of the 2022 data analysis completed in late 2024, here is where each file can be found:

- [Cook County](https://www.cookcountyclerkil.gov/service/tax-extension-and-rates)
  - Cook County Tax Code Agency Rates (Excel)
  - Agency EAV and extension by Class
  - PTaxSim -- instructions on downloading database here -- https://github.com/ccao-data/ptaxsim
- [DuPage County](https://www.dupagecounty.gov/elected_officials/county_clerk/Property_Tax_Information/propertyreports.php)
  - Tax Rate Booklet
  - as of tax year 2023, DuPage switched vendors and no longer produces a "4 Tax Extension by Township per District.pdf" report 
      - have to assume that, for each class, if the class makes up X% of the EAV it will make up exactly X% of the extension 
      - this only is necessary for SSAs -- like other counties, Table 28 is used for non-SSA extensions by class
- [Kane County](https://clerk2.kanecountyil.gov/TaxExtension/Pages/taxExtension.aspx)
  - District Value within taxcode report 
  - Tax extension detail report
- [Kendall County](https://www.kendallcountyil.gov/offices/county-clerk-recorder/county-clerk/tax-reports)
  - Tax Codes By District Listing
  - Tax extension detail report 
- [Lake County](https://www.lakecountyil.gov/268/Tax-Extension-Data)
  - EAVs, Rates, and Districts by Tax Code
  - *SSA extension data by email* -- TaxExtension@lakecountyil.gov
- [McHenry County](https://www.mchenrycountyil.gov/departments/county-clerk/taxes/tax-extension)
  - District Rates by Tax Code
  - Tax Computation Report ALL
- [Will County](https://www.willcountyclerk.gov/taxes-2/tax-extensions/tax-codes-and-rates-by-township/)
  - ALL TOWNSHIPS
  - *SSA extension data by email* -- taxextension@willcounty.gov
 
- [Table 28 -- IDOR](https://tax.illinois.gov/research/taxstats/propertytaxstatistics.html)
- Table 27 -- IDOR -- the 2021 version did not have district, so had to email Bradley.Kriener@illinois.gov to get a revised version; may not be necesssary in future years