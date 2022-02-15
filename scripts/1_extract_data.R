
# Chapter 1: Extract data from necessary sources -------------------------------

# This script reads source data from the "raw" and "resources" folder of the
# github library as well as the CMAP "data depot" V drive and interprets it.
# Each imported and processed list is then exported to an RData file in the
# "internal" folder.

# This script DOES NOT need to be repeated entirely every time this the analysis
# is run. Rather, components of this script will need to be rerun as their
# underlying data changes or issues with that particular resource are
# identified.


# load packages
library(tidyverse)
library(sf)
library(janitor)
library(here)
library(openxlsx)
library(pdftools)

counties <- c("cook", "dupage", "kane", "kendall", "lake", "mchenry", "will")

## 0. Helper functions for this script -----------------------------------------

# helper function to clean header information from a list of lists
rm_header <- function(list, header_search){
  
  for (i in seq.int(length(list))) {
    
    # grab current "page" (top level list item)
    page <- pluck(list, i)
    
    # create a blank vector of lines to remove
    lines_to_remove <- integer()
    
    # identify last header row and mark all lines through it for removal
    header_row <- last(str_which(page, header_search))
    if(!is.na(header_row)){
      lines_to_remove <- union(lines_to_remove, seq.int(header_row))
    }
    
    # identify any rows that are blank and mark for remval
    blank_rows <- str_which(list[[i]], "^$")
    lines_to_remove <- union(lines_to_remove, blank_rows)
    
    # remove the lines
    if(length(lines_to_remove) > 0){
      page <- page[-(lines_to_remove)]
    }
    
    # replace the processed page
    list[[i]] <- page
  }
  
  return(list)
}


# helper function to keep only pages with first lines that match a certain query.
# This is not currently used.
clean_pages <- function(list, header_search){
  
  for (i in seq.int(length(list))) {
    
    # grab current "page" (top level list item)
    page <- pluck(list, i)
    
    # identify pages to drop (if header search is not found in first line)
    drop <- str_detect(pluck(page, 1), header_search, negate = TRUE)
    
    # delete the page if drop is TRUE
    if(drop){
      list[[i]] <- NA
    }
  }
  
  return(list[!is.na(list)])
}


## 1. Parcel data from assessor files ------------------------------------------

# The goal of this section is to use CMAP's copies of assessor PIN data to
# produce two outputs: (1) a list dfs of all PINs with their class, tax code,
# and EAV or MV; and (2) a list of vectors of tax codes with PINs in them.

# This code section is time consuming and needs to only be run if there are
# changes the assessor files, or for a new year. Each year, field names will
# need to be checked to confirm the right PIN, CLASS, TAX CODE, and EAV/MV
# fields are named.

pins <- list()

pins$cook <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Cook.gdb",
                     layer = "AssessorData_Cook_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin = pin,
         class = overall_class,
         tax_code = taxcode_correct,
         eav = n_total_value)

pins$dupage <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_DuPage.gdb",
                       layer = "AssessorData_DuPage_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(tax_code = as.character(tax_code)) %>%
  select(pin = parcel_no,
         class = p_class,
         tax_code,
         eav = fcv_total)

pins$kane <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Kane.gdb",
                     layer = "AssessorData_Kane_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin,
         class = use_code,
         tax_code,
         eav = tot_assmt)

pins$kendall <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Kendall.gdb",
                        layer = "AssessorData_Kendall_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(eav = bor_farmland_value+bor_farm_bldg_value+bor_non_farmland_value+bor_non_farm_bldg_value,
         property_class = str_pad(property_class, 4, side = "left", 0)) %>%
  select(pin = property_key,
         class = property_class,
         tax_code,
         eav)

# For 2018 and forward, Lake has delivered us two assessor tables. the analysis
# in `investigate lake secondary table.R` demonstrates that, at least for 2018,
# the secondary table contains only duplicate records, at least as far as the
# market value field is concerned. It also indicates that there is one duplicate
# in the primary table, so for safety it is removed here.

pins$lake <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Lake.gdb",
                     layer = "AssessorData_Lake_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(class = str_sub(land_use_code, end = 2),
         tax_code = str_sub(tax_code, end = 5)) %>%
  select(pin,
         class,
         tax_code,
         mv = mkt_total_taxyr) %>%
  distinct() # removes duplicates


pins$mchenry <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_mchenry.gdb",
                        layer = "AssessorData_mchenry_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  transmute(pin = parcel_number,
            class = str_pad(property_class,4,"left",pad = "0"),
            tax_code = str_remove(tax_code,"-"),
            eav = parse_number(total_assessment))


pins$will <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_will.gdb",
                     layer = "AssessorData_will_2018") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(br_open_space = parse_number(br_open_space), # something causing this field to import as char
         eav = br_land + br_building + br_farm_land + br_farm_building + br_open_space) %>%
  select(pin,
         class = property_class,
         tax_code,
         eav)

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(pins))

# confirm that all tables have identical structures.
compare_df_cols(pins)


## OUTPUT STEPS: 
# write entire pin list
save(pins, file = here("internal", "pins.RData"))

# write list of unique taxcodes only.
tax_codes <- map(pins, function(df){unique(df$tax_code)})
save(tax_codes, file = here("internal", "tax_codes.RData"))


## 2. Tax districts by code from clerk data ------------------------------------

# The goal of this section is to determine which tax districts are in each
# taxcode. This involves PDF interpretation in most cases. The result is a list
# of "long format" tables by county, where each table identifies tax codes, tax
# district identifiers, and tax district names.

dists_by_taxcode_raw <- list()

dists_by_taxcode_raw$cook <- read.xlsx(here("raw", "Cook 2018 Tax Code Agency Rate.xlsx")) %>% 
  as_tibble() %>% 
  select(tax_code = "Tax.Code", 
         tax_district = "Agency", 
         tax_district_name = "Agency.Name") %>% 
  # clean up tax codes
  mutate(tax_district_name = str_squish(tax_district_name))


dists_by_taxcode_raw$dupage <- here("raw", "Dupage Tax Rate Book.pdf") %>% 
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # discard pre-table pages. Page start may change year over year.
  .[19:length(.)] %>% 
  # basic cleanup
  rm_header("CODE[:space:]+TAXING BODIES") %>% 
  unlist() %>% 
  as_tibble() %>% 
  # isolate tax code
  separate(value, into = c("tax_code", "value"),
           sep = "[[:space:]]{2,}", extra = "merge") %>% 
  # separate the two taxing districts per line
  # (to confirm there are max 2 on each line, stop here and run `max(str_count(test$value, ","))`)
  separate(value, into = c("dist1", "dist2", "rates"),
           sep = "[[:space:]]*,[[:space:]]*", fill = "right") %>% 
  # at this point, could capture rates, but dropping
  select(-rates) %>% 
  # fill tax codes down
  mutate(tax_code = na_if(tax_code, "")) %>% 
  fill("tax_code") %>% 
  # lengthen data
  pivot_longer(cols = c("dist1", "dist2")) %>% 
  select(-name, tax_district_name = value) %>% 
  filter(!is.na(tax_district_name)) %>% 
  # clean up tax district name extra spaces
  mutate(tax_district_name = str_squish(tax_district_name))


dists_by_taxcode_raw$kane <- here("raw", "Kane District Value by Taxcode.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("^\\s+Kane County$") %>% 
  unlist() %>% 
  as_tibble() %>% 
  # extract tax codes 
  mutate(tax_code = case_when(
    str_detect(value, "^Tax Code") ~ str_trim(str_extract(value, "(?<=Tax Code).*(?=Tax Code Rate)"))
  )) %>% 
  fill(tax_code) %>% 
  # remove tax code, header, totals, and footer lines
  filter(str_detect(value, "^Tax Code|^District|^\\s+Totals for|^\\(C \\)2019 DEVNET", negate = TRUE)) %>% 
  # separate remaining columns
  separate(
    col = "value",
    into = c("taxdist", "pollution_value", "net_taxable_value", "railroad", "rate", "total_val"),
    sep = "\\s{2,}",
    fill = "right"
  ) %>% 
  separate(
    col = "taxdist",
    into = c("tax_district", "tax_district_name"),
    sep = " - "
  ) %>% 
  # clean up tax district name extra spaces
  mutate(tax_district_name = str_squish(tax_district_name)) %>% 
  select(tax_code, tax_district, tax_district_name)


dists_by_taxcode_raw$kendall <- here("raw", "Kendall Tax Codes By District.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("District[:space:]+Tax Codes") %>% 
  unlist() %>% 
  as_tibble() %>% 
  # isolate tax district code and tax district name via hyphen search
  mutate(
    tax_district = str_squish(str_extract(value, "^.+(?= - )")),
    tax_district_name = str_squish(str_extract(value, "(?<= - ).+$"))
  ) %>% 
  fill(tax_district, tax_district_name) %>% 
  # remove tax district and footer lines
  filter(str_detect(value, " - |DEVNET", negate = TRUE)) %>% 
  # interpret tax codes
  mutate(value = str_squish(value)) %>% 
  separate(value, 
           into = c("tc1", "tc2", "tc3", "tc4", "tc5"),
           sep = "[[:space:]](?!TF)",
           fill = "right") %>% 
  pivot_longer(
    cols = starts_with("tc"),
    names_to = NULL,
    values_to = "tax_code",
    values_drop_na = TRUE
  ) %>% 
  # clean up
  mutate(tax_district_name = str_squish(tax_district_name)) %>% 
  select(tax_code, tax_district, tax_district_name) %>% 
  arrange(tax_code)


dists_by_taxcode_raw$lake <- here("raw", "Lake 2018-TCD-Rate-EAV-Auth.csv") %>% 
  # input file
  read_csv(col_types = "c__cccccccccccccccc_") %>% 
  # In 2018, some tax codes exist over 2 lines, due to doubled sanitation
  # districts. Deal with this by combining then unnesting to create two SAN cols.
  group_by(TCA) %>% 
  summarize(across(everything(), ~na_if(paste(na.omit(.x), collapse = ","),""))) %>% 
  separate(col = "SAN", into = c("SAN1", "SAN2"), sep = ",", fill = "right") %>% 
  # basic cleanup
  mutate(TCA = str_pad(TCA, 5, side = c("left"), pad = "0"),
         ESD = str_pad(ESD, 3, side = c("left"), pad = "0"),
         USD = str_pad(USD, 3, side = c("left"), pad = "0"),
         CTY = "LAKE", # manually add county to all taxcodes
         FOR = "LAKE", # manually add forest district to all taxcodes
         TWPCODE = str_sub(TCA, end = 2)) %>% 
  rename(tax_code = TCA) %>% 
  # manually introduce missing district types: township, road & bridge, and
  # special road improvement dists (GRVs)
  left_join(read_csv(here("resources", "lake_twp_dists.csv"),
                     show_col_types = FALSE),
            by = "TWPCODE") %>% 
  select(-TWPCODE) %>% 
  # rearrange
  pivot_longer(-tax_code,
               values_drop_na = TRUE) %>% 
  mutate(name = str_sub(name, end = 3)) %>% # cheap way of dropping numbers from SSA and SAN columns. 
  unite(tax_district, name, value)
  

dists_by_taxcode_raw$mchenry <- here("raw", "McHenry District Rates by Taxcode.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("McHenry County") %>% 
  unlist() %>% 
  as_tibble() %>% 
  # extract tax code
  mutate(tax_code = str_trim(str_extract(
    value, 
    "(?<=Tax Code)[[:space:]]+[[:alnum:]]{5,6}(?= -)"))) %>% 
  fill(tax_code) %>% 
  # remove tax code, subheader, total, and footer lines
  filter(str_detect(value, "^Tax Code|^District|Totals for|DEVNET", negate = TRUE)) %>% 
  # separate remaining fields
  separate(
    col = "value",
    into = c("tax_district", "tax_district_name", "rate"),
    sep = " - |[[:blank:]]{5,}") %>% 
  # clean up
  mutate(tax_district_name = str_squish(tax_district_name)) %>% 
  select(tax_code, tax_district, tax_district_name) %>% 
  arrange(tax_code)


dists_by_taxcode_raw$will <- here("raw", "Will All Townships 2018.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("TAX BODY RATES AND PERCENTAGES") %>% 
  unlist() %>% 
  as_tibble() %>%
  mutate(value = str_trim(value)) %>%  # remove leading/trailing white space
  # extract townships and tax codes
  mutate(
    tax_code = case_when(
      str_detect(value, "^TAX CODE") ~ str_trim(str_extract(value, "(?<=TAX CODE)\\s+\\d{4}"))),
    township = case_when(
      str_detect(value, "Township$") ~ str_trim(str_remove(value, "Township$")))
  ) %>% 
  fill(tax_code, township) %>% 
  # remove tax code, header, totals, and township lines
  filter(str_detect(value, "^TAX CODE|^TAX BODY|^TOTALS|Township$", negate = TRUE)) %>% 
  # separate remaining columns
  separate(
    col = "value",
    into = c("taxdist1", "taxdist2", "tax_district_name", "rate", "percentage"),
    sep = "\\s{2,}",
    fill = "right"
  ) %>% 
  unite("tax_district", "taxdist1", "taxdist2", sep = " ") %>% 
  # clean up
  mutate(tax_district_name = str_squish(tax_district_name)) %>% 
  select(tax_code, tax_district, tax_district_name)
  

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(dists_by_taxcode_raw))

# confirm that all tables have identical structures.
compare_df_cols(dists_by_taxcode_raw)


## OUTPUT STEPS: 
# write all districts by taxcode to RData
save(dists_by_taxcode_raw, file = here("internal", "dists_by_taxcode_raw.RData"))

# write to excel workbook.
write.xlsx(dists_by_taxcode_raw, 
           here("outputs", "1_dists_by_taxcode_raw.xlsx"),
           overwrite = TRUE)


## 3. Extensions by taxing district --------------------------------------------

extensions <- list()

extensions$cook <- here("raw", "Cook 2018 Agency Extension by Class Report.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("Requested$") %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = str_squish(value)) %>% 
  # remove footer rows
  filter(str_detect(value, "^Page ", negate = TRUE)) %>% 
  # split off first column
  separate(
    col = "value",
    into = c("tax_district", "value"),
    sep = "[[:space:]]",
    extra = "merge"
  ) %>% 
  # split off agency name
  separate(
    col = "value",
    into = c("tax_district_name", "value"),
    sep = "[[:space:]](?=[[:digit:]]{1,2}\\.[[:digit:]]{3})", # space followed by number of format 0.000 or 00.000
    extra = "merge"
  ) %>% 
  # split up value columns
  separate(
    col = "value",
    into = c(NA, "ext_tot", "ext_res", "ext_farm", "ext_com", "ext_ind", "ext_railroad", NA),
    sep = " "
  ) %>% 
  mutate(across(starts_with("ext"), parse_number))


extensions$dupage <- here("raw", "Dupage Tax Extension by Township per District Report.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("\\*{4}TOTAL\\*{4}$") %>% 
  unlist() %>% 
  as_tibble() %>%
  mutate(value = str_trim(value)) %>% # need to leave internal white space for now 
  # keep only tax district title and total lines
  filter(str_detect(value, "^[[:digit:]]+[[:space:]]|^G R A N D T O T A L|^\\*{3} TOTAL \\*{3}")) %>% 
  # reshape table
  mutate(values = ifelse(str_detect(value, "^\\*{3} TOTAL \\*{3}", negate = TRUE), 
                         lead(value), 
                         NA)) %>% 
  rename(name = value) %>% 
  filter(!is.na(values)) %>% 
  filter(name != "G R A N D T O T A L") %>% 
  # clean up name
  separate(
    col = "name",
    into = c("tax_district", "tax_district_name"),
    sep = "[[:space:]]+",
    extra = "merge"
  ) %>% 
  mutate(tax_district_name = str_remove(tax_district_name, "[[:space:]]+[[:digit:]]*\\.[[:digit:]]+$")) %>% 
  # to clean up values, we can't just cut based on spacing because 0 columns are
  # blank rather than listing zero. This causes columns that follow empties to
  # shift left. Rather, we approximate breaks based on fixed string lengths then
  # parse the values.
  extract(
    col = "values",
    into = c(NA, "ext_res", "ext_farm", "ext_com",  "ext_ind", "ext_totreal", "ext_railroad", "ext_tot", NA),
    regex = "(\\*{3} TOTAL \\*{3})(.{20})(.{14})(.{18})(.{17})([[:space:]]+[[:graph:]]+)(.{16})([^\\*]{10,22})([[:space:]]*\\*$)",
    remove = FALSE
  ) %>% 
  # stop here to inspect results carefully to see whether the spacing specified
  # above works for all rows. Look for hanging digits in incorrect columns.
  select(tax_district, tax_district_name, ext_res, ext_farm, ext_com, ext_ind, ext_railroad, ext_tot) %>% 
  mutate(across(starts_with("ext"), parse_number))


extensions$kane <- here("raw", "Kane 2018 Tax Extension Detail Report.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # filter each list for necessary rows: the one that starts with taxing district, and the total extension row
  map(~.[str_which(., "^[[:space:]]*Taxing District|^Totals[[:space:]]{5}")]) %>% 
  # basic cleanup
  unlist() %>% 
  as_tibble() %>%
  mutate(value = str_squish(value)) %>% 
  #reshape table
  mutate(values = ifelse(str_detect(value, "^Taxing District"), 
                         lead(value), 
                         NA)) %>% 
  rename(name = value) %>% 
  filter(str_detect(values, "^Totals ")) %>% 
  # clean up name
  extract(
    col = "name", 
    into = c(NA, "tax_district", NA, "tax_district_name"),
    regex = "(Taxing District )([[:digit:]]{3,4})( - )(.+)"
  ) %>% 
  # clean up values
  separate(
    col = "values",
    into = c(NA, "ext_tot", "ext_res", "ext_rural", "ext_com", "ext_ind", "ext_railroad_state", "ext_railroad_local", "ext_mineral"),
    sep = " "
  ) %>% 
  mutate(across(starts_with("ext"), parse_number))


# Kendall is very similar to Kane
extensions$kendall <- here("raw", "Kendall 2018 Tax Extension Detail Report.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # filter each list for necessary rows: the one that starts with taxing district, and the total extension row
  map(~.[str_which(., "^[[:space:]]*Taxing District|^Totals[[:space:]]{5}")]) %>% 
  # basic cleanup
  unlist() %>% 
  as_tibble() %>%
  mutate(value = str_squish(value)) %>% 
  #reshape table
  mutate(values = ifelse(str_detect(value, "^Taxing District"), 
                         lead(value), 
                         NA)) %>% 
  rename(name = value) %>% 
  filter(str_detect(values, "^Totals ")) %>%
  # clean up name
  extract(
    col = "name", 
    into = c(NA, "tax_district", NA, "tax_district_name"),
    regex = "(Taxing District )([[:alnum:][:space:]]{3,9})( - )(.+)" # how the district code is identified is only difference from Kane
  ) %>% 
  # clean up values
  separate(
    col = "values",
    into = c(NA, "ext_tot", "ext_res", "ext_rural", "ext_com", "ext_ind", "ext_railroad_state", "ext_railroad_local", "ext_mineral"),
    sep = " "
  ) %>% 
  mutate(across(starts_with("ext"), parse_number)) 


# Lake. The only raw data available from Lake is their P251 form, which as of
# early 2022 contains SSA extensions but nothing that splits up SSA extensions
# or EAVs by land use. Spreadsheets were obtained from Lake County staff for
# 2018 and 2020 with sufficient data for ad valorem SSAs.
extensions$lake <- here("raw", "Lake_AVSSA_2018.xlsx") %>% 
  # import sheet
  read.xlsx() %>% 
  select(tax_district = Auth, tax_district_name = Name, Class, Ext) %>%
  # recode and collapse rows
  mutate(Class = recode(Class, 
                        FA = "ext_farm",
                        FB = "ext_farm",
                        O = "ext_other",
                        RES = "ext_res",
                        COM = "ext_com",
                        IND = "ext_ind")) %>% 
  group_by(tax_district, tax_district_name, Class) %>% 
  summarize(Ext = sum(Ext), .groups = "drop") %>% 
  # reshape wider and make a total
  pivot_wider(names_from = Class, values_from = Ext) %>% 
  mutate(ext_tot = rowSums(across(starts_with("ext")), na.rm = TRUE))
  


# McHenry county doesn't list extension by land use, so we need to generate it
# by using the land use EAV ratios.
extensions$mchenry <- here("raw", "McHenry TaxComputationFinalReportA.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>% 
  # basic cleanup
  rm_header("^[[:space:]]{10,}McHenry County") %>% 
  unlist() %>% 
  as_tibble() %>%
  mutate(value = str_squish(value)) %>%
  # apply taxing district name to all data rows
  mutate(tdist = ifelse(str_detect(value, "^Taxing District"), str_remove_all(value, "^Taxing District"), NA)) %>% 
  fill(tdist) %>% 
  filter(str_detect(value, "^Taxing District", negate = TRUE)) %>% 
  # label value column
  extract(
    col = "value",
    into = c("name", "value"),
    regex = "(^[^[:digit:]]*)(.*)"
  ) %>% 
  mutate(name = str_trim(name)) %>%
  # repair some `value` fields for Mineral EAV: in some cases, the location of
  # the text "Road and Bridge Transfer" bumps the EAV values for Mineral onto a
  # new line. See Algonquin village for example. This should repair the issue.
  # Note that where and and how this break occurs may change year over year.
  mutate(value = if_else(str_detect(name, "^Mineral") & str_detect(value, "^$"), lead(value), value),
         name = if_else(str_detect(name, "^Mineral"), "Mineral", name)) %>% 
  # filter each list for necessary rows: each EAV type, and the total extension row
  filter(str_detect(
    name, 
    "^Farm|^Residential|^Commercial|^Industrial|^Mineral|^State Railroad|^Local Railroad|^County Total|^Totals \\(All\\)"
    )) %>% 
  # rename some extension types to match standard format
  mutate(name = recode(name,
                       Residential = "res",
                       Commercial = "com",
                       Industrial = "ind",
                       `State Railroad` = "railroad_state",
                       `Local Railroad` = "railroad_local",
                       `County Total` = "total",
                       `Totals (All)` = "ext_tot"),
         name = tolower(name)) %>% 
  # reshape table
  pivot_wider() %>% 
  # split up taxing district name
  extract(
    col = "tdist",
    into = c("tax_district", NA, "tax_district_name", NA, "equalization_factor"),
    regex = "([[:space:]]*[[:alnum:]]{3,6})( - )(.+(?=Equalization Factor))(Equalization Factor )(.+)"
  ) %>% 
  mutate(tax_district = str_squish(tax_district),
         tax_district_name = str_squish(tax_district_name)) %>% 
  # retrieve rate setting EAV--the second number--from each EAV type
  mutate(across(c(farm, res, com, ind, mineral, railroad_state, railroad_local, total), 
                # this regex looks for a combination of digits and commas that follows [a combination of digits and commas plus 1-5 spaces]
                ~parse_number(str_extract(., "(?<=[[:digit:],]{1,13}[[:space:]]{1,5})[[:digit:],]+")))) %>% 
  # then convert each rate setting EAV into a ratio against the total
  mutate(across(c(farm, res, com, ind, mineral, railroad_state, railroad_local, total),
                ~ifelse(total == 0, 0, ./total))) %>% 
  # retrieve the McHenry extension
  separate(
    col = "ext_tot",
    into = c(NA, NA, NA, NA, NA, NA, "ext_tot", NA),
    sep = " "
  ) %>% 
  mutate(ext_tot = parse_number(ext_tot)) %>% 
  # manufacture extensions by land use
  mutate(across(c(farm, res, com, ind, mineral, railroad_state, railroad_local),
                ~ext_tot*.,
                .names = "ext_{.col}")) %>% 
  # select necessary columns
  select(tax_district, tax_district_name, starts_with("ext"))


# Will. Available data online does not break out data by land use class. Data
# has been obtained directly from county clerk.
extensions$will <- here("raw", "Will extensions by class SSA 2018.pdf") %>%  
  # import PDF
  pdf_text() %>% 
  str_split("\n") %>%  
  # basic cleanup
  rm_header("LEVY YEAR 2018$") %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = str_squish(value)) %>% 
  # apply taxing district name to all data rows
  mutate(tdist = ifelse(str_detect(value, "^[[:digit:]]{3} .+ [[:digit:]]{4}$"), value, NA)) %>% 
  fill(tdist) %>% 
  filter(str_detect(value, "^[[:digit:]]{3} .+ [[:digit:]]{4}$", negate = TRUE)) %>%
  # filter out column names
  filter(str_detect(value, "^TOWNSHIP", negate = TRUE)) %>% 
  # Isolate township
  extract(
    col = "value",
    into = c("twp", "value"),
    regex = "([[:alpha:][:space:]]*)(.*)"
  ) %>% 
  mutate(twp = na_if(twp, "")) %>% 
  fill(twp) %>% 
  # keep only totals
  filter(str_detect(twp, "TBODY TOTALS")) %>% 
  # second row for each is Farm B. Recode.
  mutate(twp = ifelse(str_detect(value, "[[:space:]]"), "values", "ext_farmB")) %>% 
  # Reshape table
  pivot_wider(names_from = "twp") %>% 
  # Separate values
  separate(
    col = "values",
    into = c("ext_com", "ext_ind", "ext_farmA", "ext_res", "ext_mineral", "ext_railroad", "ext_tot"),
    sep = " "
  ) %>% 
  # separate taxing district ID from name. Note that there appear to be two
  # unique codes here, a 3 digit before and a 4 digit after. These are combined below
  extract(
    col = "tdist",
    into = c("tax_district1", "tax_district_name", "tax_district2"),
    regex = "(^[[:digit:]]{3} )(.+)( [[:digit:]]{4}$)"
  ) %>% 
  # clean up
  mutate(tax_district = paste(str_squish(tax_district1), str_squish(tax_district2), sep = "-")) %>% 
  mutate(across(starts_with("ext_"), parse_number)) %>%
  mutate(ext_farm = ext_farmA + ext_farmB) %>% 
  select(tax_district, tax_district_name, ext_res, ext_com, ext_ind, ext_farm, ext_mineral, ext_railroad, ext_tot)


## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(extensions))

# confirm that all tables have identical structures.
# - are "Farm" and "Rural" equivalent?
# - are "ext_railroad" and "ext_railroad_state" equivalent? see especially kane, kendall dif btwn EAV and Ext tables.
compare_df_cols(extensions)


## OUTPUT STEPS: 
# write all extensions to RData
save(extensions, file = here("internal", "extensions.RData"))


## 4. Property class summaries -------------------------------------------------

# interpret staff-maintained property class summarizing spreadsheet to create
# a list of tables with consistent naming: class, description, summary category,
# and in some cases a class-specific assessment rate.

classes <- list()


classes$cook <- read.xlsx(here("resources", "property classes.xlsx"), sheet = "Cook") %>% 
  rename_with(tolower) %>% 
  select(class, description = name, category, assessment_rate = avgassessmentrate_perccaodatabase)

classes$dupage <-read.xlsx(here("resources", "property classes.xlsx"), sheet = "DuPage") %>% 
  rename_with(tolower) %>% 
  select(class, description, category)

classes$kane <- read.xlsx(here("resources", "property classes.xlsx"), sheet = "kane") %>% 
  rename_with(tolower) %>% 
  rename(class = use_code)

classes$kendall <- read.xlsx(here("resources", "property classes.xlsx"), sheet = "kendall") %>% 
  rename_with(tolower)

classes$lake <- read.xlsx(here("resources", "property classes.xlsx"), sheet = "lake 2018 later") %>% 
  rename_with(tolower) %>% 
  select(class = class.code, description = land_use_code, category) %>% 
  group_by(class) %>% 
  summarize(description = paste(description, collapse = ","),
            category = paste(unique(category), collapse = ", "))

classes$mchenry <- read.xlsx(here("resources", "property classes.xlsx"),sheet = "mchenry") %>% 
  rename_with(tolower) %>% 
  select(class = class4, description, category, assessment_rate = ast.ratio)

classes$will <- read.xlsx(here("resources", "property classes.xlsx"),sheet = "will") %>% 
  rename_with(tolower)

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(classes))

# confirm that all tables have identical structures.
compare_df_cols(classes)


## OUTPUT STEPS: 
# write all classes to RData
save(classes, file = here("internal", "classes.RData"))


## 5. Naming table -------------------------------------------------------------

# This is an excel file CMAP has maintained for years identifying taxing
# districts un the region by their various names, so that data from various 
# sources can be matched.

# I have found that the naming table needs to be reinterpreted from Excel very
# frequently when tinkering with the code in the other two scripts. It makes
# sense to me to move this code to the top of both other scripts, rather than
# relying on the RData object this block of code creates.


naming_table_raw <- read.xlsx(here("resources", "NamingTable.xlsx"),
                          sheet = "naming") %>% 
  # Confirm field names in naming table
  select(county = County, tax_district_name = Name, 
         IDOR_name = IDOR.Name, IDOR_code = IDOR.Code,
         district_type = Type.of.District, other_name = Other.Name) %>% 
  # naming table was originally created to match imports exactly, including with
  # extra spaces. This is now unnecessary and problematic, as all extra spaces 
  # have been removed from input files. Clean up naming table to match.
  mutate(across(everything(), str_squish))

# split naming table into list of tables by county
naming_table <- naming_table_raw %>% 
  mutate(county = as_factor(tolower(county))) %>% 
  split(., .$county) %>% 
  map(select, -county)

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(naming_table))

## OUTPUT STEPS: 
# write all classes to RData
save(naming_table, file = here("internal", "naming_table.RData"))


## 6. IDOR Table 28 ------------------------------------------------------------

tbl28_raw <- read.xlsx(here("resources", "Y2018Tbl28.xlsx"), sheet = "Table28Data") %>% 
  set_names(~tolower(str_replace_all(.,"\\.","_")))

tbl28 <- as_tibble(tbl28_raw) %>% 
  # convert county to factor for splitting later, keeping only counties in the region
  mutate(primary_county = factor(tolower(primary_county), levels = counties)) %>% 
  filter(!is.na(primary_county)) %>% 
  # clean up table
  arrange(primary_county) %>% 
  select(1:5, 
         ext_tot = total_extension_nossa,
         ext_res = residential_extension_new,
         ext_com = commercial_extension_new,
         ext_ind = industrial_extension_new,
         ends_with("_new"), # this keeps only extensions in the format that ends with "_new"
         -total_farm_extension_new) %>% # but drop total farm extension, because farm A and farm B duplicate this
  mutate(
    # sum these non R/C/I extensions
    ext_other = rowSums(select(.,ends_with("_new")), na.rm = TRUE),
    # verity that sub extensions sum to total extension
    ext_tot2 = ext_res + ext_com + ext_ind + ext_other) %>% 
  # align names with other tables and drop unnecessary columns
  select(tax_district = district_id, 
         tax_district_name = district_name,
         tax_district_type = type_code,
         starts_with("ext_"),
         primary_county) %>% 
  # split into list of dfs, dropping unnecessary columns
  split(., .$primary_county, drop = TRUE) %>% 
  map(select, -primary_county, -ext_tot2)

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(tbl28))

# confirm that all tables have identical structures.
compare_df_cols(tbl28)


## OUTPUT STEPS: 
# write all classes to RData
save(tbl28, file = here("internal", "tbl28.RData"))
