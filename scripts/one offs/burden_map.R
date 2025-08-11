library(sf)
library(tidyverse)
library(RSQLite)
library(gdalUtilities)
library(httr)
library(readxl)
library(here)

analysis_year <- "2023"

counties <- c("cook", "dupage", "kane", "kendall", "lake", "mchenry", "will")




# cook --------------------------------------------------------------------

cook_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Cook_", analysis_year,".gdb"),
                        layer = paste0("Parcels_Cook_", analysis_year))  |>  
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


cook_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17031") %>% 
  select(geoid = geoid_tract)

# cook_parcels_distinct <- cook_parcels2 |>
#   group_by(PIN) |>
#   summarise(geometry = st_union(geom)) %>%
#   ungroup()

cook_parcel_tract_join <- st_join(cook_parcels2 %>% st_point_on_surface(), cook_tracts)

cook_join_proc <- cook_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin = PIN, geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "cook")


write_csv(cook_join_proc, "C:\\Users\\abahls\\OneDrive - Chicago Metropolitan Agency for Planning\\Alex\\cook_temp.csv")

# cook_join_proc <- read_csv("C:\\Users\\abahls\\OneDrive - Chicago Metropolitan Agency for Planning\\Alex\\cook_temp.csv")

# dupage ------------------------------------------------------------------

dupage_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_DuPage_", analysis_year, ".gdb"),
                          layer = paste0("Parcels_DuPage_",analysis_year)) %>%
  select(pin = PIN, tax_code = TAXCODE)

dupage_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17043") %>% 
  select(geoid = geoid_tract)

dupage_parcel_tract_join <- st_join(dupage_parcels %>% st_point_on_surface(), dupage_tracts)

dupage_join_proc <- dupage_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin, geoid, tax_code) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(!tax_code) %>% 
  mutate(county = "dupage")


# Kane --------------------------------------------------------------------
kane_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Kane_",analysis_year,".gdb"),
                        layer = paste0("Parcels_Kane_", analysis_year)) %>% 
  select(pin = PIN)

kane_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17089") %>% 
  select(geoid = geoid_tract)

kane_parcel_tract_join <- st_join(kane_parcels %>% st_point_on_surface(), kane_tracts)

kane_join_proc <- kane_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin , geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "kane")




# kendall -----------------------------------------------------------------

kendall_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_kendall_", analysis_year,".gdb"),
                           layer = paste0("Parcels_kendall_",analysis_year)) %>% 
  mutate(pin = str_sub(pin_dashle, 2, 10)) %>% 
  select(pin)

kendall_bgs <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17093") %>% 
  select(geoid = geoid_tract)

kendall_parcel_tract_join <- st_join(kendall_parcels %>% st_point_on_surface(), kendall_bgs)

kendall_join_proc <- kendall_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin, geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "kendall")


# Lake --------------------------------------------------------------------

lake_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Lake_", analysis_year, ".gdb"),
                        layer = paste0("Parcels_Lake_",analysis_year)) %>% 
  st_transform(crs = cmapgeo::cmap_crs)

lake_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17097") %>% 
  select(geoid = geoid_tract)

lake_parcel_tract_join <- st_join(lake_parcels %>% st_point_on_surface(), lake_tracts)

lake_join_proc <- lake_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin = PIN, geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "lake")


# mchenry --------------------------------------------------------------------

mchenry_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_McHenry_", analysis_year, ".gdb"),
                           layer = paste0("Parcels_McHenry_",analysis_year)) %>% 
  mutate(pin = str_remove_all(PIN,"-")) %>% 
  select(pin)

mchenry_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17111") %>% 
  select(geoid = geoid_tract)

mchenry_parcel_tract_join <- st_join(mchenry_parcels %>% st_point_on_surface(), mchenry_tracts)

mchenry_join_proc <- mchenry_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin, geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "mchenry")


# will --------------------------------------------------------------------

will_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Will_", analysis_year, ".gdb"),
                                layer = paste0("Parcels_Will_",analysis_year)) %>% 
  select(pin = PIN) %>% 
  mutate(pin = case_when(analysis_year == 2022 & str_sub(pin, 1, 1) == 0 ~ str_sub(pin, 2, 16),
                         analysis_year == 2023 & str_length(pin) == 15 ~ str_c("0", pin),
                         T ~ pin))

will_tracts <- cmapgeo::tract_sf %>% 
  filter(county_fips == "17197") %>% 
  select(geoid = geoid_tract)

will_parcel_tract_join <- st_join(will_parcels %>% st_point_on_surface(), will_tracts)

will_join_proc <- will_parcel_tract_join %>% 
  as_tibble() %>% 
  select(pin, geoid) %>% 
  distinct() %>% 
  group_by(pin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(county = "will")


pin_tract_map <- rbind(cook_join_proc, dupage_join_proc, kane_join_proc, kendall_join_proc, lake_join_proc, mchenry_join_proc,
                  will_join_proc)

# write_csv(pin_tract_map, "C:\\Users\\abahls\\OneDrive - Chicago Metropolitan Agency for Planning\\Alex\\combined_temp.csv")
# 
# pin_tract_map <- read_csv("C:\\Users\\abahls\\OneDrive - Chicago Metropolitan Agency for Planning\\Alex\\combined_temp.csv")

pin_tract_map_list <-  pin_tract_map %>% 
  group_split(county, .keep = F)%>% 
  as.list()


# load_data ---------------------------------------------------------------

# property class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

# taxing districts by tax code
load(here("internal", "dists_by_taxcode_proc.RData"))

names(pin_tract_map_list) <- names(pins)


effective_tax_rates <- list()

for (county in counties){
  effective_tax_rates[[county]] <- read_excel(paste0("outputs\\3_effective_rates_", county, "_", analysis_year, ".xlsx"))
}



add_pin_fn_cook_dupage_kane <- function(pin_table, tract_xwalk, effect_tax_rate_table){
  
  df1 <- pin_table %>% 
    mutate(pin10 = str_sub(pin, 1, 10))
  
  df2 <- left_join(df1, tract_xwalk, by = c("pin10" = "pin"))
  
  
  df3 <- left_join(df2, effect_tax_rate_table, by = "tax_code") %>% 
    select(pin, class, tax_code, eav, geoid, starts_with("eff_rate"))
  
  return(df3)
}

add_pin_fn_else <- function(pin_table, tract_xwalk, effect_tax_rate_table){
  
  
  df1 <- left_join(pin_table, tract_xwalk, by = c("pin" = "pin"))
  
  
  df2 <- left_join(df1, effect_tax_rate_table, by = "tax_code") %>% 
    select(pin, class, tax_code, eav, geoid, starts_with("eff_rate"))
  
  return(df2)
}



pins$cook <- add_pin_fn_cook_dupage_kane(pins$cook, pin_tract_map_list$cook, effective_tax_rates$cook)
pins$dupage <- add_pin_fn_cook_dupage_kane(pins$dupage, pin_tract_map_list$dupage, effective_tax_rates$dupage)
pins$kane <- add_pin_fn_cook_dupage_kane(pins$kane, pin_tract_map_list$kane, effective_tax_rates$kane)
pins$will <- add_pin_fn_else(pins$will, pin_tract_map_list$will, effective_tax_rates$will)


pins$kendall <- pins$kendall %>% 
  left_join(pin_tract_map_list$kendall) %>% 
  left_join(effective_tax_rates$kendall)

pins$lake <- pins$lake %>% 
  mutate(pin = str_remove_all(pin, "-")) %>% 
  left_join(pin_tract_map_list$lake, by = c("pin")) %>% 
  left_join(effective_tax_rates$lake)

pins$mchenry <- pins$mchenry %>% 
  mutate(pin = str_remove_all(pin, "-")) %>% 
  left_join(pin_tract_map_list$mchenry, by = c("pin")) %>% 
  left_join(effective_tax_rates$mchenry)

# add mv ------------------------------------------------------------------

#from top of script 3 
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
  # group_by(df, tax_code, category) %>% 
  #   summarize(mv = sum(mv, na.rm = TRUE), .groups = "drop") %>% 
  #   mutate(tax_code = as.character(tax_code))
}

market_vals <- map2(pins, classes, sum_by_taxcode_and_category)
# 
# mv_all <- bind_rows(market_vals, .id = "county") %>% 
#   select(county, pin, tax_code, category, mv) %>% 
#   mutate(pin = case_when(
#     county == "cook" ~ str_sub(pin, 1, 10),
#     county == "kendall" ~ str_sub(pin, 2, 10),
#     county == "lake" | county == "mchenry" ~ str_remove_all(pin, "-"),
#     T ~ pin
#   ))
# 
# combined_with_mv <- combined %>% 
#   left_join(mv_all)
# 
# the_missings <- combined_with_mv %>% filter(is.na(mv))
# 
# table(combined_with_mv$county)
# table(the_missings$county)

# write_csv(combined_with_mv, "C:\\Users\\abahls\\OneDrive - Chicago Metropolitan Agency for Planning\\Alex\\combined_temp_with_mv.csv")

#from script 3
etr_all_county <- bind_rows(market_vals, .id = "county")

etr_all_county_proc <- etr_all_county %>% 
  filter(category == "Residential") %>% 
  mutate(burden = mv*eff_rate_res) %>% 
  group_by(geoid) %>% 
  reframe(average_burden = mean(burden, na.rm = T)) %>% 
  mutate(geoid = as.character(geoid))

tract_data_pull <- tidycensus::get_acs(
  geography = "tract",
  variables = "B19013_001",
  year = 2023, 
  output = "wide",
  state = "il",
  county = str_remove_all(cmapgeo::county_fips_codes$cmap, "17"),
  geometry = T
)

tract_proc <- tract_data_pull %>% 
  select(geoid = GEOID, median_income = B19013_001E)

#2023 MSA hh income - https://www2.census.gov/library/publications/2024/demo/acsbr-023.pdf
msa_median_inc <- 87071


tract_inc_rate_merge <- tract_proc %>% 
  left_join(etr_all_county_proc) %>% 
  mutate(percent_of_income = average_burden/median_income)

median_burden <- median(tract_inc_rate_merge$percent_of_income, na.rm = T)
  
tract_inc_rate_group <- tract_inc_rate_merge %>% 
  mutate(group = case_when(
           percent_of_income >= median_burden & median_income >= msa_median_inc ~ "Blue",
           percent_of_income >= median_burden & median_income < msa_median_inc ~ "Green",
           percent_of_income < median_burden & median_income >= msa_median_inc ~ "Red",
           percent_of_income < median_burden & median_income < msa_median_inc ~ "Yellow")) %>% 
  relocate(group)

burden_between_5_median <- tract_inc_rate_group %>% 
  filter(percent_of_income > 0.05 & percent_of_income < median_burden)

write_sf(burden_between_5_median, "outputs\\one offs\\burden_between_5_and_median.gpkg")


sf::write_sf(tract_inc_rate_group, "outputs\\one offs\\burden_map.gpkg")
sf::write_sf(tract_inc_rate_group, "outputs\\one offs\\burden_map.shp")

writexl::write_xlsx(tract_inc_rate_group %>% as_tibble() %>% select(!geometry), "outputs\\one offs\\burden_data_excel.xlsx")

#map colors
  #red -- db2429
  #blue -- 1b8ecf
  #green - 6dad4d
  #orange -- e6bb21

