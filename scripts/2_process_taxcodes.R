
# Chapter 2: Process tax districts by tax code -------------------------------

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

## 1. Load required files ------------------------------------------------------

# tax codes actually used in each county
load(here("internal", "tax_codes.RData"))

# districts by taxcode
load(here("internal", "districts_by_taxcode.RData"))

# This is an excel file CMAP has maintained for years identifying taxing
# districts un the region by their various names, so that data from various 
# sources can be matched.
naming.table <- read.xlsx(here("resources", "NamingTable.xlsx"),
                           sheet = "naming") %>% 
  # Confirm field names in naming table
  select(county = County, tax_district_name = Name, 
         IDOR_name = IDOR.Name, IDOR_code = IDOR.Code,
         district_type = Type.of.District, other_name = Other.Name) %>% 
  # naming table was originally created to match imports exactly, including with
  # extra spaces. This is now unnecessary and problematic, as all extra spaces 
  # have been removed from input files. Clean up naming table to match.
  mutate(tax_district_name = str_squish(tax_district_name))

## 2. Interpret data -----------------------------------------------------------

districts_by_taxcode_out <- list()

### Cook County ----------------------------------------------------------------

# remove tax codes not present in the PIN data
cook.data <- filter(districts_by_taxcode$cook, tax_code %in% tax_codes$cook)

# join w/ naming table. It is important to verify that this adds no additional
# rows. Additional rows likely signify duplicate entries in the `naming.table`.
cook.data <- left_join(cook.data, 
                       select(filter(naming.table, county == "Cook"), -county), 
                       by = "tax_district_name")

# If there are extra rows, this code can be used to identify possible issues 
cook.data %>% 
  count(tax_code, tax_district_name) %>% 
  filter(n > 1)

# Manually assign district types specifically for districts that have levies but
# do not exist in table 28 (and therefore aren't in the naming table)
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
    is.na(district_type) & str_detect(tax_district_name, "HOME EQUITY ASSURANCE") ~ "Home Equity Assurance District",
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
districts_by_taxcode_out$cook <- cook.data %>% 
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


cook.data.report <- report(districts_by_taxcode_out$cook)

write.xlsx(list(output = districts_by_taxcode_out$cook, 
                report = cook.data.report,
                not_included = cook.na), 
           here("outputs", "2_dists_by_taxcode_proc_cook.xlsx"), overwrite = TRUE)

### DuPage County --------------------------------------------------------------

### Kane County ----------------------------------------------------------------

### Kendall County -------------------------------------------------------------

### Lake County ----------------------------------------------------------------

### McHenry County -------------------------------------------------------------

### Will County ----------------------------------------------------------------

## 3. Save the list as RData ---------------------------------------------------
