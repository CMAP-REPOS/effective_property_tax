
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
library(sf)
library(gdalUtilities)

analysis_year <- 2022

## 1. Load required resources --------------------------------------------------

# property class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

# taxing districts by tax code
load(here("internal", "dists_by_taxcode_proc.RData"))

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
}

# apply function to each pin table + class table combination 
market_vals <- map2(pins, classes, sum_by_taxcode_and_category)

# inspect columns for parallelism
compare_df_cols(market_vals)


# list of parcels by tax code ---------------------------------------------
cook_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_Cook_2021.gdb",
                        layer = "Parcels_Cook_2021")  |>  
  select(PIN = PIN10)

#https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
#spaital join initally gave this error -- fix stolen from above link
# Error in scan(text = lst[[length(lst)]], quiet = TRUE) : 
#   scan() expected 'a real', got 'ParseException:'
# Error in (function (msg)  : ParseException: Unknown WKB type 1

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

cook_parcels2 <- ensure_multipolygons(cook_parcels)

cook_parcels_distinct <- cook_parcels2 |>
  group_by(PIN) |>
  summarise(geometry = st_union(geom)) %>%
  ungroup()



