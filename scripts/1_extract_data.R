
# Chapter 1: Extract data from necessary sources -------------------------------

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

districts_by_taxcode <- list()

districts_by_taxcode$cook <- read.xlsx(here("raw", "Cook 2018 Tax Code Agency Rate.xlsx")) %>% 
  as_tibble() %>% 
  select(tax_code = "Tax.Code", 
         tax_district = "Agency", 
         tax_district_name = "Agency.Name") %>% 
  # clean up tax codes
  mutate(tax_district_name = str_squish(tax_district_name))


districts_by_taxcode$dupage <- here("raw", "Dupage Tax Rate Book.pdf") %>% 
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


districts_by_taxcode$kane <- here("raw", "Kane District Value by Taxcode.pdf") %>%  
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


districts_by_taxcode$kendall <- here("raw", "Kendall Tax Codes By District.pdf") %>%  
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


districts_by_taxcode$lake <- here("raw", "Lake 2018-TCD-Rate-EAV-Auth.csv") %>% 
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
  

districts_by_taxcode$mchenry <- here("raw", "McHenry District Rates by Taxcode.pdf") %>%  
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


districts_by_taxcode$will <- here("raw", "Will All Townships 2018.pdf") %>%  
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
identical(counties, names(districts_by_taxcode))

# confirm that all tables have identical structures.
compare_df_cols(districts_by_taxcode)


## OUTPUT STEPS: 
# write all districts by taxcode to RData
save(districts_by_taxcode, file = here("internal", "districts_by_taxcode.RData"))

# write to excel workbook.
write.xlsx(districts_by_taxcode, 
           here("outputs", "1_dists_by_taxcode_raw.xlsx"),
           overwrite = TRUE)


## 3. Extensions by taxing district --------------------------------------------

