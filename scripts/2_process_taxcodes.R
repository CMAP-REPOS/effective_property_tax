
# Chapter 2: Process tax districts by tax code -------------------------------

# This script uses tax code data from CMAP copy of county assessor records and
# tax code detail reports from each county to determine exactly which taxing
# districts are in which tax codes.

# load packages
library(tidyverse)
library(janitor)
library(here)
library(openxlsx)

counties <- c("cook", "dupage", "kane", "kendall", "lake", "mchenry", "will")

## 0. Helper functions for this script -----------------------------------------

# helper function to run "reports" on each output
report <- function(df){
  df <- df %>% 
    map(unique) %>% # this converts each table column to a list, and takes unique entries from each
    map(sort) %>%  # sort those unique entries
    map(paste, collapse = "; ") %>% 
    as_tibble() %>% 
    pivot_longer(everything(), names_to = "district type", values_to = "districts")
  
  unsep <- filter(df, str_detect(districts, ","))
  
  if(nrow(unsep) > 0){
    message("Some districts not yet separated:")
    print(unsep)
  }
  
  return(df)
} 

# helper function to drop a column that may or may not exist
# https://stackoverflow.com/a/58935650
drop_cols <- function(df, ...){
  df %>% 
    select(-one_of(map_chr(enquos(...), quo_name)))
}

## 1. Load required resources --------------------------------------------------

# tax codes actually used in each county
load(here("internal", "tax_codes.RData"))

# districts by taxcode
load(here("internal", "dists_by_taxcode_raw.RData"))

# naming table 
source(here("scripts", "0_naming_table_builder.R"))
# (run `build_naming_table()` after sourcing this file to rebuild the naming
# table in this session from the Excel file)

## 2. Interpret data -----------------------------------------------------------

dists_by_taxcode_proc <- list()

### Cook County ----------------------------------------------------------------

# remove tax codes not present in the PIN data
cook.data <- filter(dists_by_taxcode_raw$cook, tax_code %in% tax_codes$cook)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
cook.data <- left_join(cook.data, naming_table$cook, by = "tax_district_name")

# If there are extra rows, this code can be used to identify possible issues 
cook.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table). 

# Note that the need for these sections (they exist for all 7 counties) would be
# eliminated if all districts were added to the naming table. The naming table
# as originally designed was for IDOR 28 matching only, but at this point is
# used more generally for improving naming conventions, SSA name matching, and
# district categorization broadly.
cook.data <- mutate(
  cook.data, 
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    is.na(district_type) & str_detect(tax_district_name, "SPECIAL")         ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "SPEC SERV")       ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "SSA")             ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "MENTAL HLTH")   ~ "Mental Health District",
    is.na(district_type) & str_detect(tax_district_name, "MENTAL HEALTH") ~ "Mental Health District",
    is.na(district_type) & str_detect(tax_district_name, "RECLAMATION BOND") ~ "Water Reclamation District",
    is.na(district_type) & str_detect(tax_district_name, "WATER COMMISSION")               ~ "Water",
    is.na(district_type) & str_detect(tax_district_name, "LIBRARY FUND")       ~ "Municipal Library",
    is.na(district_type) & str_detect(tax_district_name, "GENERAL ASSISTANCE") ~ "General Assistance",
    is.na(district_type) & str_detect(tax_district_name, "HOME EQUITY ASSURANCE") ~ "Home Equity Assurance District", # prob don't need this line anymore, these have been updated in naming table
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
cook.na <- cook.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_district = paste(sort(unique(tax_district)), collapse = ","),
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))

# Process and reshape the data
dists_by_taxcode_proc$cook <- cook.data %>% 
  filter(tax_code != 25030) %>% # This tax code has one parcel, which is exempt, and is problematically in two park districts. Remove. 
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  pivot_wider(id_cols = tax_code, 
              names_from = district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>%
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% #replaces column name whitespace with underscore
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  unnest_wider(Unit_School_District, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # flatten RPM TIF into a single district
  mutate(Tax_Increment_Financing_District = recode(
    Tax_Increment_Financing_District,
    "TIF TRANSIT CITY OF CHICAGO-RPM1, BOARD OF EDUCATION - TIF RPM1" = "TIF CITY OF CHICAGO-RPM")) %>% 
  # Clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA)
  drop_cols("NA") # drop the "NA" column, which contains taxing districts we want to drop.


cook.data.report <- report(dists_by_taxcode_proc$cook)

write.xlsx(list(output = dists_by_taxcode_proc$cook, 
                report = cook.data.report,
                not_included = cook.na), 
           here("outputs", "2_dists_by_taxcode_proc_cook.xlsx"), overwrite = TRUE)

rm(cook.data, cook.data.report, cook.na)

### DuPage County --------------------------------------------------------------

# remove tax codes not present in the PIN data
dupage.data <- filter(dists_by_taxcode_raw$dupage, tax_code %in% tax_codes$dupage)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
dupage.data <- left_join(dupage.data, naming_table$dupage, by = "tax_district_name")

# If there are extra rows, this code can be used to identify possible issues 
dupage.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
dupage.data <- mutate(
  dupage.data,
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "SPEC SER") ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "SPC SER") ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "LIBR") ~ "Municipal Library",
    is.na(district_type) & str_detect(tax_district_name, "ST & BR") ~ "Municipal Road and Bridge District",
    is.na(district_type) & str_detect(tax_district_name, "ST &BR") ~ "Municipal Road and Bridge District",
    is.na(district_type) & str_detect(tax_district_name, "ST&BR") ~ "Municipal Road and Bridge District",
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  )) %>% 
  # this tax code has only 2 taxable properties in it and is in two municipalities. Remove one.
  filter(!(tax_code == "9169" & IDOR_name == "DARIEN"))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
dupage.na <- dupage.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))


# Process and reshape the data
dists_by_taxcode_proc$dupage <- dupage.data %>% 
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  pivot_wider(id_cols = tax_code, 
              names_from=district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% #replaces column name whitespace with underscore
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  unnest_wider(Water, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # Create some districts manually
  mutate(
    County = "DUPAGE CO",
    Forest_Preserve_District = "DUPAGE CO FST PRSV DIST",
    Township = case_when(
      str_detect(tax_code, "^1") ~ "WAYNE TWP",
      str_detect(tax_code, "^2") ~ "BLOOMINGDALE TWP",
      str_detect(tax_code, "^3") ~ "ADDISON TWP",
      str_detect(tax_code, "^4") ~ "WINFIELD TWP",
      str_detect(tax_code, "^5") ~ "MILTON TWP",
      str_detect(tax_code, "^6") ~ "YORK TWP",
      str_detect(tax_code, "^7") ~ "NAPERVILLE TWP",
      str_detect(tax_code, "^8") ~ "LISLE TWP",
      str_detect(tax_code, "^9") ~ "DOWNERS GROVE TWP"),
    Township_Road_and_Bridge_District = case_when(
      str_detect(tax_code, "^1") ~ "WAYNE TWP ROAD",
      str_detect(tax_code, "^2") ~ "BLOOMINGDALE TWP ROAD",
      str_detect(tax_code, "^3") ~ "ADDISON TWP ROAD",
      str_detect(tax_code, "^4") ~ "WINFIELD TWP ROAD",
      str_detect(tax_code, "^5") ~ "MILTON TWP ROAD",
      str_detect(tax_code, "^6") ~ "YORK TWP ROAD",
      str_detect(tax_code, "^7") ~ "NAPERVILLE TWP ROAD",
      str_detect(tax_code, "^8") ~ "LISLE TWP ROAD",
      str_detect(tax_code, "^9") ~ "DOWNERS GROVE TWP ROAD")) %>%  
  # Clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA 
  drop_cols("NA") %>%  # drop the "NA" column, which contains taxing districts we want to drop.
  select(., tax_code, sort(names(.))) # alpha sort columns (needed because of new columns added above)

dupage.data.report <- report(dists_by_taxcode_proc$dupage)

write.xlsx(list(output = dists_by_taxcode_proc$dupage, 
                report = dupage.data.report,
                not_included = dupage.na), 
           here("outputs", "2_dists_by_taxcode_proc_dupage.xlsx"), overwrite = TRUE)

rm(dupage.data, dupage.data.report, dupage.na)

### Kane County ----------------------------------------------------------------

# remove tax codes not present in the PIN data
kane.data <- filter(dists_by_taxcode_raw$kane, tax_code %in% tax_codes$kane)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
kane.data <- left_join(kane.data, naming_table$kane, by = c("tax_district" = "tax_district_name"))

# If there are extra rows, this code can be used to identify possible issues 
kane.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
kane.data <- mutate(
  kane.data,
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "LIBRARY") ~ "Municipal Library",
    is.na(district_type) & str_detect(tax_district_name, "SSA") ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    is.na(district_type) & str_detect(tax_district_name, "CEMETERY") ~ "Cemetery District",
    is.na(district_type) & str_detect(tax_district_name, "TWP FIRE") ~ "Fire Protection District",
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
kane.na <- kane.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))

# Process and reshape the data
dists_by_taxcode_proc$kane <- kane.data %>%
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  pivot_wider(id_cols = tax_code, 
              names_from = district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% #replaces column name whitespace with underscore
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # Clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA 
  drop_cols("NA")  # drop the "NA" column, which contains taxing districts we want to drop.


kane.data.report <- report(dists_by_taxcode_proc$kane)

write.xlsx(list(output = dists_by_taxcode_proc$kane, 
                report = kane.data.report,
                not_included = kane.na), 
           here("outputs", "2_dists_by_taxcode_proc_kane.xlsx"), overwrite = TRUE)

rm(kane.data, kane.data.report, kane.na)


### Kendall County -------------------------------------------------------------

# remove tax codes not present in the PIN data
kendall.data <- filter(dists_by_taxcode_raw$kendall, tax_code %in% tax_codes$kendall)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
kendall.data <- left_join(kendall.data, naming_table$kendall, by = "tax_district_name")

# If there are extra rows, this code can be used to identify possible issues 
kendall.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
kendall.data <- mutate(
  kendall.data,
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "SSA") ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    is.na(district_type) & str_detect(tax_district_name, "ROAD DISTRICT") ~ "Township Road and Bridge District",
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
kendall.na <- kendall.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))

# Process and reshape the data
dists_by_taxcode_proc$kendall <- kendall.data %>%  
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  pivot_wider(id_cols = tax_code, 
              names_from = district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% 
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # clean up
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  drop_cols("NA")  # drop the "NA" column, which contains taxing districts we want to drop.


kendall.data.report <- report(dists_by_taxcode_proc$kendall)

write.xlsx(list(output = dists_by_taxcode_proc$kendall, 
                report = kendall.data.report,
                not_included = kendall.na), 
           here("outputs", "2_dists_by_taxcode_proc_kendall.xlsx"), overwrite = TRUE)

rm(kendall.data, kendall.data.report, kendall.na)


### Lake County ----------------------------------------------------------------

# remove tax codes not present in the PIN data
lake.data <- filter(dists_by_taxcode_raw$lake, tax_code %in% tax_codes$lake)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
lake.data <- left_join(lake.data, naming_table$lake, by = c("tax_district" = "tax_district_name"))

# If there are extra rows, this code can be used to identify possible issues 
lake.data %>% 
  count(tax_code, tax_district) %>% 
  filter(n > 1)

# The following step is removed for Lake entirely right now. All things that
# matter are in the naming table. The records that exist in the NA table as a
# result of not running this script for the most part have no market value.
# Specifically, LKBLF* and NOCHI* are dropped because these districts only
# experience a fraction of the muni's extension--per Kipp @ county Clerk's
# office, NOCHI* taxcodes only pay the city's library extension, and LKBLF*
# taxcodes only pay the village's fire protection levy. Ideally these would be
# factored in here, but for now they are ignored.


# # Manually assign district types specifically for districts that have levies but
# # do not exist in table 28 (and therefore aren't in the naming table)
# lake.data <- mutate(
#   lake.data,
#   district_type = case_when(
#     is.na(district_type) & str_detect(tax_district, "ESD_") ~ "Elementary School District",
#     is.na(district_type) & str_detect(tax_district, "FIR_") ~ "Fire Protection District",
#     is.na(district_type) & str_detect(tax_district, "HSD_") ~ "High School District",
#     is.na(district_type) & str_detect(tax_district, "MUN_") ~ "Municipality",
#     is.na(district_type) & str_detect(tax_district, "PRK_") ~ "Park District",
#     is.na(district_type) & str_detect(tax_district, "TIF_") ~ "Tax Increment Financing District",
#     is.na(district_type) & str_detect(tax_district, "SSA_") ~ "Special Service Area",
#     is.na(district_type) & str_detect(tax_district, "SAN_") ~ "Wastewater",
#     TRUE ~ district_type # in all other cases, leave the value what it was prior.
#   ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
lake.na <- lake.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district) %>% 
  summarise(
    other_name = paste(sort(unique(other_name)), collapse = ","),
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))


# Process and reshape the data
dists_by_taxcode_proc$lake <- lake.data %>% 
  mutate(IDOR_name = coalesce(IDOR_name, tax_district))  %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  filter(!(tax_code == "17105" & tax_district == "SAN_DMWDS")) %>% # remove duplicate san district in only tax code where it shows up (it has no levy)
  pivot_wider(id_cols = tax_code, 
              names_from = district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% 
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>%
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # create road and bridge district manually
  mutate(Township_Road_and_Bridge_District = case_when(
    str_detect(Township, "ANTIOCH TWP") ~ "ANTIOCH TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "AVON TWP") ~ "AVON TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "FREMONT TWP") ~ "FREMONT TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "GRANT TWP") ~ "GRANT TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "LAKE VILLA TWP") ~ "LAKE VILLA TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "LIBERTYVILLE TWP") ~ "LIBERTYVILLE TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "SHIELDS TWP") ~ "SHIELDS TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "WARREN TWP") ~ "WARREN TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "WAUKEGAN TWP") ~ "WAUKEGAN TWP ROAD AND BRIDGE DISTRICT",
    str_detect(Township, "VERNON TWP") ~ "VERNON TWP ROAD AND BRIDGE DISTRICT")) %>% 
  # clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA 
  drop_cols("NA") %>%  # drop the "NA" column, which contains taxing districts we want to drop.
  select(., tax_code, sort(names(.))) # alpha sort columns (needed because of new columns added above)


lake.data.report <- report(dists_by_taxcode_proc$lake)

write.xlsx(list(output = dists_by_taxcode_proc$lake, 
                report = lake.data.report,
                not_included = lake.na), 
           here("outputs", "2_dists_by_taxcode_proc_lake.xlsx"), overwrite = TRUE)

rm(lake.data, lake.data.report, lake.na)


### McHenry County -------------------------------------------------------------


# remove tax codes not present in the PIN data
mchenry.data <- filter(dists_by_taxcode_raw$mchenry, tax_code %in% tax_codes$mchenry)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
mchenry.data <- left_join(mchenry.data, naming_table$mchenry, by = c("tax_district" = "tax_district_name"))

# If there are extra rows, this code can be used to identify possible issues 
mchenry.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
mchenry.data <- mutate(
  mchenry.data,
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "TWP RD & BR") ~ "Township Road and Bridge District",
    is.na(district_type) & str_detect(tax_district_name, "TWP RD &BR") ~ "Township Road and Bridge District",
    is.na(district_type) & str_detect(tax_district_name, "SSA") ~ "Special Service Area",
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
mchenry.na <- mchenry.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))


# Process and reshape the data
dists_by_taxcode_proc$mchenry <- mchenry.data %>% 
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  pivot_wider(id_cols = tax_code, 
              names_from=district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% 
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  # make new columns
  mutate(Township_Road_and_Bridge_District = case_when(
    str_detect(Township, "ALDEN TWP") ~ "ALDEN TWP RD & BR",
    str_detect(Township, "ALGONQUIN TWP") ~ "ALGONQUIN TWP RD & BR",
    str_detect(Township, "BURTON TWP") ~ "BURTON TWP RD & BR",
    str_detect(Township, "CHEMUNG TWP") ~ "CHEMUNG TWP RD & BR",
    str_detect(Township, "CORAL TWP") ~ "CORAL TWP RD & BR",
    str_detect(Township, "DORR TWP") ~ "DORR TWP RD & BR",
    str_detect(Township, "DUNHAM TWP") ~ "TWP RD & BR",
    str_detect(Township, "GRAFTON TWP") ~ "GRAFTON TWP RD & BR",
    str_detect(Township, "GREENWOOD TWP") ~ "GREENWOOD TWP RD & BR",
    str_detect(Township, "HARTLAND TWP") ~ "HARTLAND TWP RD & BR",
    str_detect(Township, "HEBRON TWP") ~ "HEBRON TWP RD & BR",
    str_detect(Township, "MARENGO TWP") ~ "MARENGO TWP RD & BR",
    str_detect(Township, "MCHENRY TWP") ~ "MCHENRY TWP RD & BR",
    str_detect(Township, "NUNDA TWP") ~ "NUNDA TWP RD & BR",
    str_detect(Township, "RICHMOND TWP") ~ "RICHMOND TWP RD & BR",
    str_detect(Township, "RILEY TWP") ~ "RILEY TWP RD & BR",
    str_detect(Township, "SENECA TWP") ~ "SENECA TWP RD & BR")) %>% 
  # Clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA 
  drop_cols("NA") %>%  # drop the "NA" column, which contains taxing districts we want to drop.
  select(., tax_code, sort(names(.))) # alpha sort columns (needed because of new column added above)


mchenry.data.report <- report(dists_by_taxcode_proc$mchenry)

write.xlsx(list(output = dists_by_taxcode_proc$mchenry, 
                report = mchenry.data.report,
                not_included = mchenry.na), 
           here("outputs", "2_dists_by_taxcode_proc_mchenry.xlsx"), overwrite = TRUE)

rm(mchenry.data, mchenry.data.report, mchenry.na)


### Will County ----------------------------------------------------------------

# remove tax codes not present in the PIN data
will.data <- filter(dists_by_taxcode_raw$will, tax_code %in% tax_codes$will)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
will.data <- left_join(will.data, naming_table$will, by = "tax_district_name")

# If there are extra rows, this code can be used to identify possible issues 
will.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
will.data <- mutate(
  will.data,
  district_type = case_when(
    is.na(district_type) & str_detect(tax_district_name, "TIF") ~ "Tax Increment Financing District",
    is.na(district_type) & str_detect(tax_district_name, "RD & BR") ~ "Municipal Road and Bridge District", 
    is.na(district_type) & str_detect(tax_district_name, "SSA") ~ "Special Service Area",
    ## The following two lines adjust 1 tax code each and had been added by SL 
    ## but per discussion in feb 2022 with LH it was determined that neither the 
    ## SAUK VILLAGE BOND nor PLDF LIBRARY SPECIAL districts belong categorized 
    ## in this manner. Neither match to an extension in script 3.
    #is.na(district_type) & str_detect(tax_district_name, "BOND") ~ "Municipality",
    #is.na(district_type) & str_detect(tax_district_name, "LIBRARY") ~ "Library District",
    TRUE ~ district_type # in all other cases, leave the value what it was prior.
  ))

# Identify taxing districts that still don't have tax district names. These
# items will be ignored during the pivot stage in the next step.
#
# The things we WANT to ignore are districts that overlap perfectly with other
# districts, or districts that don't have levies that are distinct from others.
# Bond funds, for example.
#
# Anything in here that we DO want to keep should probably be added to the
# naming.table, or to the above `case_when` function.
will.na <- will.data %>% 
  filter(is.na(district_type)) %>% 
  group_by(tax_district_name) %>% 
  summarise(
    tax_codes = n(),
    tax_codes_which = paste(sort(unique(tax_code)), collapse = ",")) %>% 
  arrange(desc(tax_codes))


# process and reshape
dists_by_taxcode_proc$will <- will.data %>%
  mutate(IDOR_name = coalesce(IDOR_name, tax_district_name)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  pivot_wider(id_cols = tax_code, 
              names_from=district_type, 
              values_from = IDOR_name, 
              names_sort = TRUE,
              values_fn = list) %>% 
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "_")) %>% 
  # The pivot made list columns, because some tax codes contain multiple districts
  # of the same type. First, unnest those at are known about
  unnest_wider(Special_Service_Area, names_sep = "_") %>% 
  # Then, flatten all remaining list columns by force. If the unnests were
  # successful, this won't actually collapse any lists (it will only convert
  # column types). If it does (search the df for ","), consider adding more
  # unnests.
  rowwise() %>% 
  mutate_if(is.list, paste, collapse = ", ") %>% 
  mutate(Township_Road_and_Bridge_District = case_when(
    str_detect(Township, "CHANNAHON TWP") ~ "CHAN TWP ROAD FUNDS",
    str_detect(Township, "CRETE TWP") ~ "CRETE TWP ROAD FUNDS",
    str_detect(Township, "CUSTER TWP") ~ "CUSTER TWP ROAD FUNDS",
    str_detect(Township, "FLORENCE TWP") ~ "FLOR TWP ROAD FUNDS",
    str_detect(Township, "FRANKFORT TWP") ~ "FRKFT TWP ROAD FUNDS",
    str_detect(Township, "GREEN GARDEN TWP") ~ "GR GAR TWP ROAD FUNDS",
    str_detect(Township, "HOMER TWP") ~ "HOMER TWP ROAD FUNDS",
    str_detect(Township, "JACKSON TWP") ~ "JACKSN TWP ROAD FUNDS",
    str_detect(Township, "JOLIET TWP") ~ "JOLIET TWP ROAD FUNDS",
    str_detect(Township, "LOCKPORT TWP") ~ "LOCKPT TWP ROAD FUNDS",
    str_detect(Township, "MANHATTAN TWP") ~ "MANHTN TWP ROAD FUNDS",
    str_detect(Township, "MONEE TWP") ~ "MONEE TWP ROAD FUNDS",
    str_detect(Township, "NEW LENOX TWP") ~ "N LENX TWP ROAD FUNDS",
    str_detect(Township, "PEOTONE TWP") ~ "PEOTONE TWP ROAD FUNDS",
    str_detect(Township, "PLAINFIELD TWP") ~ "PLFD TWP ROAD FUNDS",
    str_detect(Township, "REED TWP") ~ "REED TWP ROAD FUNDS",
    str_detect(Township, "TROY TWP") ~ "TROY TWP ROAD FUNDS",
    str_detect(Township, "WASHINGTON TWP") ~ "WSHGTN TWP ROAD FUNDS",
    str_detect(Township, "WESLEY TWP") ~ "WESLEY TWP ROAD FUNDS",
    str_detect(Township, "WHEATLAND TWP") ~ "WHEAT TWP ROAD FUNDS",
    str_detect(Township, "WILL TWP") ~ "WILL TWP ROAD FUNDS",
    str_detect(Township, "WILMINGTON TWP") ~ "WILM TWP ROAD FUNDS",
    str_detect(Township, "WILTON TWP") ~ "WILTON TWP ROAD FUNDS")) %>% 
  # Clean up
  mutate_if(is.character, list(~na_if(.,""))) %>%  #turns blank cells into NA
  drop_cols("NA") %>%  # drop the "NA" column, which contains taxing districts we want to drop.
  select(., tax_code, sort(names(.)))  # alpha sort columns (needed because of new column added above)


will.data.report <- report(dists_by_taxcode_proc$will)

write.xlsx(list(output = dists_by_taxcode_proc$will, 
                report = will.data.report,
                not_included = will.na), 
           here("outputs", "2_dists_by_taxcode_proc_will.xlsx"), overwrite = TRUE)

rm(will.data, will.data.report, will.na)


## 3. Save the list as RData ---------------------------------------------------

## CHECK STEPS: 
# confirm list is named and ordered correctly:
identical(counties, names(dists_by_taxcode_proc))

# Confirm that all tables have identical structures. 
# MS: There is probably some work to do here to further align these columns
# across counties, but it's not a huge deal.
compare_df_cols(dists_by_taxcode_proc)

## OUTPUT STEP: 
# write all districts by taxcode to RData
save(dists_by_taxcode_proc, file = here("internal", "dists_by_taxcode_proc.RData"))

