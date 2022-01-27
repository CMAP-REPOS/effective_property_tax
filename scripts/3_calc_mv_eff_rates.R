
# Chapter 3: Calculate market values and effective rates -----------------------

# load packages
library(tidyverse)
library(janitor)
library(here)
library(openxlsx)

## 1. Load required resources --------------------------------------------------

# propery class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

# taxing districts by tax code.
load(here("internal", "dists_by_taxcode_proc.RData"))

## 2. Calculate market values --------------------------------------------------

# Below each tax code's market value is calculated and matched with the property class.

# This function summarizes pins by taxcode and land use with help from a class summary table
sum_by_taxcode_and_category <- function(pin_table, class_table){
  
  # Join the pins to the class table.
  df <- left_join(pin_table, class_table, by = "class")
  
  # if class table does not have an assessment rate field, it is always 1/3
  if(!("assessment_rate" %in% names(class_table))){assessment_rate <- 1/3}
  
  # if pin data does not contain market values already, calculate them
  if(!("mv" %in% names(pin_table))){df <- mutate(df, mv = eav / assessment_rate)}
  
  # group, summarize, return.
  group_by(df, tax_code, category) %>% 
    summarize(mv = sum(mv, na.rm = TRUE), .groups = "drop")
}

# apply function to each pin table + class table combination 
market_vals <- map2(pins, classes, sum_by_taxcode_and_category)

# inspect columns for parallelism
compare_df_cols(market_vals)


# At this point, I checked my MV tables (market value by tax code and use type)
# Against Stephanie's. I do this by running Stephanie's code until I have a file 
# like `dupage.mv` and then comparing it to my table `market_vals$dupage` using
# code something like this:
# 
# test <- full_join(dupage.mv, market_vals$dupage, by = c("tax_code", "category")) %>%
#   mutate(test = sum_MV - mv)
#
# All have the same numbers, except for Lake County:
# Cook:   OK
# DuPage: OK but taxcode was integer
# Kane:   OK but taxcode was integer
# Kendall:OK 
# McHenry:OK
# Will:   OK but taxcode was double
# Lake:   Very different. Seems like multiple issues here with old script. 
#         New script seems more correct. Issues identified:
#          (1) SL's script appended the secondary table to the primary one, 
#              but this duplicates MVs. 
#          (2) SL's script did not remove duplicate records
#          (3) SL's script converted taxcodes to numbers, which dropped leading
#              zeroes and screwed up taxcodes with letters in them.


## 3. Convert districts by taxcode to a long format ----------------------------

districts.long <- map(
  dists_by_taxcode_proc, 
  function(df){
    df %>% 
      pivot_longer(
        -tax_code,
        names_to = "district_type",
        values_to = "district_name") %>% 
      drop_na(district_name) %>% 
      # clean up district type field
      mutate(district_type = str_replace_all(
        str_remove(district_type, "_[[:digit:]]$"),
        "_", " "))
  })

# inspect columns for parallelism
compare_df_cols(districts.long)
