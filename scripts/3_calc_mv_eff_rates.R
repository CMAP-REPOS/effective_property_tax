
# Chapter 3: Calculate market values and effective rates -----------------------

# This script uses assessor data, extension data from IDOR Table 28 and county
# resources, and the output of script 2 to tabulate the market values and
# extensions for each taxing district in the region, by land use. Then, it
# calculates effective tax rates and summarizes effective tax rates across
# districts to the tax code.

# load packages
library(tidyverse)
library(janitor)
library(here)
library(openxlsx)

## 1. Load required resources --------------------------------------------------

# property class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

# taxing districts by tax code
load(here("internal", "dists_by_taxcode_proc.RData"))

# extension data by district by tax code (source for SSAs, although some
# counties are complete lists of all taxcodes that could theoretically replace
# table 28 down the road)
load(here("internal", "extensions.RData"))

# naming table 
source(here("scripts", "0_naming_table_builder.R"))
# (run `build_naming_table() after sourcing this file to rebuild the naming
# table in this session from the Excel file)

# table 28 (source for extensions, but does not include SSAs)
load(here("internal", "tbl28.RData"))


## 2. Calculate market values --------------------------------------------------

# Below each tax code's market value is calculated and matched with the property class.

# optionally in the future, this part of the script could be changed to produce
# only mv_res, mv_com, mv_ind, and mv_other (sum of remainder), to match
# extension data.

# This function summarizes pins by taxcode and land use with help from a class summary table
sum_by_taxcode_and_category <- function(pin_table, class_table){
  
  # Join the pins to the class table.
  df <- left_join(pin_table, class_table, by = "class")
  
  # if class table does not have an assessment rate field, it is always 1/3
  if(!("assessment_rate" %in% names(class_table))){assessment_rate <- 1/3}
  
  # if pin data does not contain market values already, calculate them
  if(!("mv" %in% names(pin_table))){df <- mutate(df, mv = eav / assessment_rate)}
  
  # right here, it would be easy to recode categories that aren't R, C, or I
  # into an other category. The question is how to handle exempt and railroad
  # properties. 
  
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
# Table 28. Some county extension data contains information that duplicates
# table 28. These are ignored for now (filtered out).

# First, join extensions with naming table. This can't be mapped easily across
# all counties in the below function because some counties join on name and
# others on code.
extensions$cook <- left_join(extensions$cook, naming_table$cook, by = "tax_district_name")
extensions$dupage <- left_join(extensions$dupage, naming_table$dupage, by = "tax_district_name")
extensions$kane <- left_join(extensions$kane, naming_table$kane, by = c("tax_district" = "tax_district_name"))
extensions$kendall <- left_join(extensions$kendall, naming_table$kendall, by = "tax_district_name")
extensions$lake <- left_join(extensions$lake, naming_table$lake, by = c("tax_district" = "tax_district_name"))
extensions$mchenry <- left_join(extensions$mchenry, naming_table$mchenry, by = c("tax_district" = "tax_district_name"))
extensions$will <- left_join(extensions$will, naming_table$will, by = "tax_district_name")


create_final_extension_list <- function(extensions, tbl28){
  
  # identify SSAs from extension data, resolving names from naming table, and calculate `other` extension.
  ssa <- extensions %>% 
    filter(district_type == "Special Service Area" | str_detect(tax_district_name, "SPEC SER|SSA|SPECIAL|SPC SER|SPEC REFUSE|HOME EQUITY ASSURANCE")) %>% 
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

final_extensions <- map2(
  extensions, 
  tbl28,
  create_final_extension_list
)


# inspect columns for parallelism
compare_df_cols(final_extensions)

# at this point, a valuable check to add would be seeing if there are any
# duplicate names within any county's list of districts. This could cause errors
# related to summing of market values.


## 5. Summarize tax codes to districts with market values and extensions -------

# define a function that does this
sum_with_mv_ext <- function(districts_df, market_vals_df, extensions_df){
  
  # start with districts by taxcode
  df <- districts_df %>%
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
    }) 
  
  # introduce extensions. This is a full join, so that missing values can be
  # inspected in the next step.
  df <- full_join(
    df, 
    extensions_df,
    by = c("district_name" = "tax_district_name")
    )
  
  return(df)
}

# map this function across three parallel variables
exts_and_vals <- pmap(
  list(districts.long,
       market_vals, 
       final_extensions),
  sum_with_mv_ext)

# inspect columns for parallelism
compare_df_cols(exts_and_vals)


## 6. Identify and remove  districts missing either extensions or MVs ----------


# No districts with non-zero extensions should be missing market values. Inspect.
id_missing_mv <- function(df, nm){
  
  nomatch <- df %>% 
    filter(is.na(district_type)) %>% 
    select(district_name, ext_tot, ext_src) %>% 
    arrange(desc(ext_tot), district_name)
  
  if(nrow(nomatch)>0){
    # report them...
    message(paste(toupper(nm), "has some extension records without MV records, which will be dropped down the road.\n  (Non-zero extensions without MVs are problematic)."))
    print(nomatch)
  }
  
  return(nomatch)
}

exts_and_vals_no_vals <- map2(exts_and_vals, names(exts_and_vals), id_missing_mv)


# some taxing districts identified during tax code processing may not have
# related extension data. This is expected in some cases. For example, TIF
# district extensions are included in municipality extensions, township road and
# bridge extensions are included in township districts, etc. However, tables
# should be inspected for districts that should have extensions -- like school
# districts and munis. Unmatched districts will result in erroneously low
# effective tax rates.
id_missing_ext <- function(df, nm){
  
  nomatch <- df %>% 
    filter(is.na(ext_src)) %>% 
    select(-starts_with("ext_")) %>% 
    arrange(district_type, district_name)
  
  if(nrow(nomatch)>0){
    # report them...
    message(paste(toupper(nm), "has the following types of taxing districts identified, but no correlated extension data:"))
    count(nomatch, district_type) %>% 
      arrange(desc(n)) %>% 
      print()
  }
  
  return(nomatch)
}

exts_and_vals_no_exts <- map2(exts_and_vals, names(exts_and_vals), id_missing_ext)


# remove taxing districts that have no extension or market value
exts_and_vals <- pmap(
  list(
    exts_and_vals,
    exts_and_vals_no_vals,
    exts_and_vals_no_exts
  ),
  function(df, dfrm1, dfrm2){
    df %>% 
      anti_join(dfrm1, by = "district_name") %>% 
      anti_join(dfrm2, by = "district_name")
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
    mutate(eff_rate_res = (ext_res)/(mv_residential + mv_vacant),
           eff_rate_ci = (ext_com + ext_ind)/(mv_commercial + mv_industrial)) %>% 
    # where effective rate is 0/0, rate will be "NaN". Replace these.
    mutate_all(~replace(., is.nan(.), 0))
  
  # where table 28 has an extension but no MV, the effective rate is infinite.
  # This could be an issue but is probably not (often the extension is
  # essentially 0). Identify rows where this is an issue.
  infinites <- df %>% 
    filter(
      is.infinite(eff_rate_res)|is.infinite(eff_rate_ci)) %>% 
    select(
      1:2, mv_residential, mv_commercial, mv_industrial, 
      ext_res, ext_com, ext_ind, eff_rate_res, eff_rate_ci)
  
  # If there are any infinites...
  if(nrow(infinites)>0){
    # report them...
    message(paste(toupper(nm), "has some infinite effective rates, which are corrected to 0: \n (If the relevant extension isn't tiny, this is problematic.)"))
    print(infinites)
    
    # and fix them.
    df$eff_rate_res[is.infinite(df$eff_rate_res)] <- 0
    df$eff_rate_ci[is.infinite(df$eff_rate_ci)] <- 0
  }
  
  return(df)
}

# map function across each table
effective_rates_districts <- map2(
  exts_and_vals,
  names(exts_and_vals),
  calc_effective_rates
)

# An excel-based comparison to SL's final effective rate analysis at this point
# finds many discrepancies, but all evidence of differences points to the
# current script being either more accurate or more likely to be accurate.




## 8. Match districts to tax codes to get ER by tax code -----------------------

effective_rates_taxcodes <- map2(
  districts.long,
  effective_rates_districts,
  function(dists, rates_by_dist){
    left_join(dists, rates_by_dist, by = c("district_type", "district_name")) %>% 
      group_by(tax_code) %>% 
      summarize(eff_rate_res = sum(eff_rate_res, na.rm = TRUE), 
                eff_rate_ci = sum(eff_rate_ci, na.rm = TRUE))
  }
)


## 9. Outputs ------------------------------------------------------------------

# output one xlsx workbook for each county. Each workbook contains an "effective
# rates - taxcode" sheet (the final analysis) and a variety of reporting sheets.
# Every taxing district in the analysis shows up on one of the three reporting
# sheets. Districts on the "effective rates - district" sheet were factored into
# the tax code effective rates. Districts on "dists without exts" are produced
# by the MV analysis but do not have matching extensions. This is often the case
# for inactive districts, or districts whose extensions are embedded into others
# -- e.g. general assistance extensions are included in table 28 township data,
# municipal library extensions in table 28 muni data. Districts on "dists
# without MVs" are in the extension data but don't seem to have any MV. This is
# problematic if the extension is non-zero.

# one annoying aspect of this code chunk is that, when correcting an issue for a
# single county, this function overwrites all counties. Unlike with a CSV, there
# must be something in the metadata of an xlsx file that updates on every write
# (e.g. date written or somesuch). This means every county's export shows up as
# a changed file for a github push even if results from only one county have
# changed. I'd prefer to have this function inspect the existing file, and only
# overwrite if the data in it are not identical.

pwalk(
  list(effective_rates_taxcodes,
       effective_rates_districts,
       exts_and_vals_no_exts,
       exts_and_vals_no_vals,
       names(effective_rates_taxcodes)),
  function(df1, df2, df3, df4, nm){
    write.xlsx(list(`eff rates - taxcode` = df1,
                    `eff rates - district` = df2,
                    `dists without exts` = df3,
                    `dists without MVs` = df4), 
               here("outputs", paste0("3_effective_rates_", nm, ".xlsx")), overwrite = TRUE)
  }
)

