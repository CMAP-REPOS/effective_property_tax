
# Chapter 3: Calculate market values and effective rates -----------------------

# load packages
library(tidyverse)
library(janitor)
library(here)

## 1. Load required resources --------------------------------------------------

# propery class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

# taxing districts by tax code
load(here("internal", "dists_by_taxcode_proc.RData"))

# extension data by district by tax code (source for SSAs. It's worth looking
# into whether we can use this data as the source of all extensions, and no
# longer rely on Table 28)
load(here("internal", "extensions.RData"))

# naming table
load(here("internal", "naming_table.RData"))

# table 28 (source for extensions, but does not include SSAs)
load(here("internal", "tbl28.RData"))


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
      mutate(
        district_type = str_replace_all(
          str_remove(district_type, "_[[:digit:]]$"),
          "_", " ")
        )
  })

# inspect columns for parallelism
compare_df_cols(districts.long)


## 4. Create combined extensions by by county ----------------------------------

# This section combines SSA extensions from the the tax computation reports
# imported into the `extensions` list with non-SSA extensions imported from IDOR
# Table 28. It may be worth looking into using extension data only in the
# future, but Lindsay has concerns about this.

create_final_extension_list <- function(extensions, naming_table, tbl28){
  
  # identify SSAs from extension data, join to naming table, calculate `other` extension.
  ssa <- extensions %>% 
    left_join(naming_table, by = "tax_district_name") %>% 
    filter(district_type == "Special Service Area" | str_detect(tax_district_name, "SPEC SERV|SSA|SPECIAL|SPC SER")) %>% 
    mutate(tax_district_name = coalesce(IDOR_name, tax_district_name),
           ext_other = ext_tot - ext_res - ext_com - ext_ind,
           ext_src = paste0("clerk_", tax_district)) %>% 
    select(tax_district_name, ext_res, ext_com, ext_ind, ext_other, ext_tot, ext_src)
  
  # get all other taxing districts
  non_ssa <- tbl28 %>% 
    mutate(ext_src = paste0("tbl28_", tax_district)) %>% 
    select(-tax_district, -tax_district_type)
  
  # bind and return
  bind_rows(non_ssa, ssa)
}

final_extensions <- pmap(
  list(extensions, naming_table, tbl28),
  create_final_extension_list
)


# inspect columns for parallelism
compare_df_cols(final_extensions)



## 5. Summarize tax codes to districts with market values and extensions -------

# define a function that does this
sum_with_mv_ext <- function(districts_df, market_vals_df, extensions_df){
  
  browser()
  # start with districts by taxcode
  districts_df %>%
    # join with market values by taxcode and LU category. 
    left_join(market_vals_df, by = "tax_code") %>%
    # drop taxcode
    select(-1) %>%
    # pivot on LU category, creating sums of market value for each land use category
    pivot_wider(names_from = category,
                values_from= mv,
                values_fn = list(mv = sum)) %>%
    # apply a series of renaming functions to clean up market value columns
    rename_at(-1:-2, function(nms){
      nms %>% 
        tolower() %>% 
        str_replace_all("\\s|/", "_") %>% 
        paste0("mv_", .)
    }) %>% 
    # introduce extensions from table 28, which is filtered by county
    left_join(extensions_df,
              by = c("district_name" = "tax_district_name"))
}

# map this function across three parallel variables
extensions_and_values <- pmap(
  list(districts.long,
       market_vals, 
       extensions),
  sum_with_mv_ext)

# inspect columns for parallelism
compare_df_cols(extensions_and_values)

View(extensions_and_values$cook)

## 6. Identify taxing districts without extension data -------------------------

no_extensions <- map(
  extensions_and_values,
  function(df){
    filter(df, is.na(ext_src)) %>% 
      arrange(district_type) %>% 
      select(-starts_with("ext_"))
  }
)


## 7. Calculate effective rates ------------------------------------------------

# create a function that does this. Note unique treatment of Cook County.
calc_effective_rates <- function(df, nm){
  
  # create an dummy mv_vacant variable. Any county df without an mv_vacant
  # column (every county except for Cook) will look here instead.
  mv_vacant <- 0
  
  df <- df %>% 
    # replace NAs with 0s
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # calculate effective rates
    mutate(effective_rate_res = (ext_res)/(mv_residential + mv_vacant),
           effective_rate_ci = (ext_com + ext_ind)/(mv_commercial + mv_industrial)) %>% 
    # where effective rate is 0/0, rate will be "NaN". Replace these.
    mutate_all(~replace(., is.nan(.), 0))
  
  # where table 28 has an extension but no MV, the effective rate is infinite.
  # This could be an issue but is probably not (often the extension is
  # essentially 0). Identify rows where this is an issue.
  infinites <- df %>% 
    filter(
      is.infinite(effective_rate_res)|is.infinite(effective_rate_ci)) %>% 
    select(
      1:2, mv_residential, mv_commercial, mv_industrial, 
      ext_res, ext_com, ext_ind, effective_rate_res, effective_rate_ci)
  
  # If there are any infinites...
  if(nrow(infinites)>0){
    # report them...
    message(paste(toupper(nm), "has some infinite effective rates, which are corrected to 0:"))
    print(infinites)
    
    # and fix them.
    df$effective_rate_res[is.infinite(df$effective_rate_res)] <- 0
    df$effective_rate_ci[is.infinite(df$effective_rate_ci)] <- 0
  }
  
  return(df)
}

# map function across each table
effective_rates <- map2(
  extensions_and_values,
  names(extensions_and_values),
  calc_effective_rates
)

# An excel-based comparison to SL's final effective rate analysis at this point
# finds many discrepancies, but all evidence of differences points to the
# current script being either more accurate or more likely to be accurate.




# 8. Match districts to tax codes to get ER by tax code ----------------------
#Once the SSAs are completed and you have the SSA extensions, the last step is to sum
#the effective rates from each district by the tax codes. 

