
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

# first, some names need to be manually repaired. In Cook, this is due to agency
# names being cut off due to length in the their PDF report. Only fixing SSAs
# because that is all we presently use. Presuming that SSAs are reported in
# document in ascending order. Correcting to names that match the market value
# work. 

# For the future, it's worth looking at matching based on agency number/district
# code rather than name.
extensions$cook <- mutate(
  extensions$cook,
  tax_district_name = case_when(
    tax_district == "03-0030-102" ~ "VILLAGE OF BARRINGTON SPECIAL SERVICE AREA 4",
    tax_district == "03-0030-103" ~ "VILLAGE OF BARRINGTON SPECIAL SERVICE AREA 6",
    tax_district == "03-0050-102" ~ "VILLAGE OF BARTLETT SPEC SER AREA CENTEX ONE",
    tax_district == "03-0050-103" ~ "VIL OF BARTLETT SPEC SER WILLIAMSBURG HILLS3",
    tax_district == "03-0050-104" ~ "VIL OF BARTLETT SPEC SERV/AMBER GROVE UT 6&7",
    tax_district == "03-0140-100" ~ "VILLAGE OF BROOKFIELD SPECIAL SERVICE AREA 1",
    tax_district == "03-0140-101" ~ "VILLAGE OF BROOKFIELD SPECIAL SERVICE AREA 2",
    tax_district == "03-0140-102" ~ "VILLAGE OF BROOKFIELD SPECIAL SERVICE AREA 3",
    tax_district == "03-0140-103" ~ "VILLAGE OF BROOKFIELD SPECIAL SERVICE AREA 4",
    tax_district == "03-0150-100" ~ "VILLAGE OF BUFFALO GROVE SPEC SERVICE AREA 1",
    tax_district == "03-0150-101" ~ "VILLAGE OF BUFFALO GROVE SPEC SERVICE AREA 2",
    tax_district == "03-0150-102" ~ "VILLAGE OF BUFFALO GROVE SPEC SERVICE AREA 3",
    tax_district == "03-0500-100" ~ "VILLAGE OF HANOVER PARK SPEC SERVICE AREA 1",
    tax_district == "03-0500-101" ~ "VILLAGE OF HANOVER PARK SPEC SERVICE AREA 2",
    tax_district == "03-0520-100" ~ "VIL OF HARWOOD HEIGHTS SPECIAL SERVICE AREA",
    tax_district == "03-0630-113" ~ "VILLAGE OF INVERNESS SPECIAL SERVICE AREA 14",
    tax_district == "03-0660-100" ~ "VILLAGE OF LAGRANGE SPECIAL SERVICE AREA 4 A",
    tax_district == "03-0870-100" ~ "VILLAGE OF NORTHBROOK SPECIAL SERVICE AREA 1",
    tax_district == "03-0870-101" ~ "VILLAGE OF NORTHBROOK SPECIAL SERVICE AREA 2",
    tax_district == "03-0970-101" ~ "CITY OF PALOS HGTS SPEC SERV/LAKE KATHERINE",
    tax_district == "03-1040-104" ~ "CITY OF PROSPECT HEIGHTS SPEC SERVICE AREA 5",
    tax_district == "03-1110-104" ~ "CITY OF ROLLING MEADOWS SPECIAL SERV AREA 5",
    tax_district == "03-1180-100" ~ "VIL OF SO BARRINGTON SPECIAL SERVICE AREA #1",
    tax_district == "03-1240-101" ~ "VILL OF STREAMWOOD SPEC SERV 2 OAK RIDGE TLS",
    tax_district == "03-1240-104" ~ "VILLAGE OF STREAMWOOD SPECIAL SERVICE AREA 5",
    tax_district == "03-1240-105" ~ "VILLAGE OF STREAMWOOD SPECIAL SERVICE AREA 6",
    tax_district == "08-0390-100" ~ "WOODLEY ROAD SANITARY DIST SPEC SERV AREA 1",
    tax_district == "02-0110-007" ~ "LEYDEN TOWNSHIP SPEC REFUSE COLLECTION DIST",
    TRUE ~ tax_district_name
  )
)

# then, join extensions with naming table. This can't be mapped easily across
# all counties in the below function because some counties join on name and
# others on code.
extensions$cook <- left_join(extensions$cook, naming_table$cook, by = "tax_district_name")
extensions$dupage <- left_join(extensions$dupage, naming_table$dupage, by = "tax_district_name")
extensions$kane <- left_join(extensions$kane, naming_table$kane, by = c("tax_district" = "tax_district_name"))
extensions$kendall <- left_join(extensions$kendall, naming_table$kendall, by = "tax_district_name")
extensions$lake <- left_join(extensions$lake, naming_table$lake, by = c("tax_district" = "tax_district_name")) %>% 
  mutate(tax_district_name = as.character(tax_district_name)) # remove this after lake import is addressed.
extensions$mchenry <- left_join(extensions$mchenry, naming_table$mchenry, by = c("tax_district" = "tax_district_name"))
extensions$will <- left_join(extensions$will, naming_table$will, by = "tax_district_name")


create_final_extension_list <- function(extensions, tbl28){
  
  # identify SSAs from extension data, join to naming table, calculate `other` extension.
  ssa <- extensions %>% 
    filter(district_type == "Special Service Area" | str_detect(tax_district_name, "SPEC SER|SSA|SPECIAL|SPC SER|SPEC REFUSE")) %>% 
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



## 5. Summarize tax codes to districts with market values and extensions -------

# define a function that does this
sum_with_mv_ext <- function(districts_df, market_vals_df, extensions_df, nm){
  
  
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
  
  # introduce extensions from table 28, which is filtered by county
  df <- full_join(
    df, 
    extensions_df,
    by = c("district_name" = "tax_district_name")
    )
  
  
  # there should be no districts with non-zero extensions (from `extensions_df`) 
  # that fail to link with a district with non-zero market value. Inspect output
  # to confirm:
  nomatch <- df %>% 
    filter(is.na(district_type)) %>% 
    select(district_name, ext_tot, ext_src) %>% 
    arrange(desc(ext_tot), district_name)
  
  if(nrow(nomatch)>0){
    # report them...
    message(paste(toupper(nm), "has some extension records without MV records. These are dropped.\n  (but non-zero extensions without MVs are problematic)."))
    print(nomatch)
    
    # and rop them
    df <- filter(df, !is.na(district_type))
  }
  
  return(df)
}

# map this function across three parallel variables
extensions_and_values <- pmap(
  list(districts.long,
       market_vals, 
       final_extensions,
       names(final_extensions)),
  sum_with_mv_ext)

# inspect columns for parallelism
compare_df_cols(extensions_and_values)


# at this point there should be no districts with non-zero extensions but no values.

## LINDSAY: What's up with nortern moraine wtr reclamation district?

## 6. Identify taxing districts without extension data -------------------------

# some taxing districts identified during tax code processing may not have
# related extension data. This is expected in some cases. For example, TIF
# district extensions are included in municipality extensions, township road and
# bridge extensions are included in township districts, etc. However, the
# following tables should be inspected for districts that should have extensions
# -- like school districts and munis. Unmatched districts will result in
# erroneously low effective tax rates.

# Note: DuPage seems to have a lot of SSAs on the tax code books that don't have
# extensions/don't show up in tax extension report.

no_extensions <- map2(
  extensions_and_values,
  names(extensions_and_values),
  function(df, nm){
    df <- filter(df, is.na(ext_src)) %>% 
      arrange(district_type, district_name) %>% 
      select(-starts_with("ext_"))
    
    message(paste(toupper(nm), "has the following taxing districts identified, but no correlated extension data:"))
    count(df, district_type) %>% 
      arrange(desc(n)) %>% 
      print()
    
    return(df)
  }
)


# Questions for Lindsay

View(no_extensions$cook)
View(final_extensions$cook)

# Cook extension data has General assistance districts, drainage districts, home
# equity assurance districts, mental health districts, etc etc. Which of these
# are included in other extensions in table 28?

# TOWN LEYDEN - WESTDALE PARK DIST has an extension in raw file and is in the
# naming table but has no value in table 28?

View(no_extensions$kane)
View(final_extensions$kane)

# GENEVA TWP FIRE SPEC DIST has an extension but is not in table 28. It's
# categorized as a fire protection district. Should it be a "muni fire", presuming
# it's extension is included in the township extension? Or, is it essentially an SSA?

View(no_extensions$kendall)
View(final_extensions$kendall)

# AURORA LIBRARY has an extension but its not in table 28. categorized as
# library. should it be a municipal library?

View(no_extensions$lake)
View(final_extensions$lake)
# Many problems here with school districts and munis. SSAs not addressed yet for Lake.



## 7. Calculate effective rates ------------------------------------------------

# consider integrating with step 5 above.

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

