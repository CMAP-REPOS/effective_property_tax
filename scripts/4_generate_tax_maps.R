# Chapter 1: Create Tax Map Objects -------------------------------

# This script reads source parcels and pins from CMAP's internal V: Drive and joins them to create shapefiles of each county
# divided by tax code

library(sf)
library(tidyverse)
library(RSQLite)
library(gdalUtilities)

# cook --------------------------------------------------------------------

#see https://github.com/ccao-data/ptaxsim
ptaxsim_db_conn <- dbConnect(SQLite(), "resources/ptaxsim-2021.0.4 (2).db")

cook_pins <- dbGetQuery(ptaxsim_db_conn, "select pin, tax_code_num from pin where year = 2021") 

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

cook_tax_codes_for_merge <- cook_pins |>
  mutate(PIN = substr(pin,1,10)) |> #PtaxSim has 14 digit pins, V drive file has 10 digit
  select(PIN, tax_code_num)

cook_parcels_distinct_with_tc <- cook_parcels_distinct |>
  left_join(cook_tax_codes_for_merge)

table(is.na(cook_parcels_distinct_with_tc$tax_code_num)) #how many missing codes -- looked after the fact and its mostly water

cook_valid <- cook_parcels_distinct_with_tc %>% 
  mutate(valid = st_is_valid(geometry),
         geometry = case_when(
           valid == F ~ st_make_valid(geometry),
           T ~ geometry
         )) 


cook_parcels_tc_dissolve <- cook_valid |>
  select(!valid) %>% 
  group_by(tax_code_num) |>
  summarise() |> 
  rename(tax_code = tax_code_num)

write_sf(cook_parcels_tc_dissolve,"outputs/tax_code_shapes/cook.gpkg")


# dupage ------------------------------------------------------------------

dupage_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_DuPage.gdb",
                       layer = "AssessorData_DuPage_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(tax_code = as.character(tax_code)) %>%
  select(pin = parcel_no,
         tax_code)


dupage_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_DuPage_2021.gdb",
                          layer = "Parcels_DuPage_2021") %>% 
  select(pin = PIN,parcel_taxcode = TAXCODE)

dupage_join <- dupage_parcels %>% 
  left_join(dupage_pins)

dupage_shape <- dupage_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(dupage_shape, "outputs/tax_code_shapes/dupage.gpkg")


# Kane --------------------------------------------------------------------
kane_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Kane.gdb",
                     layer = "AssessorData_Kane_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin,
         tax_code)

kane_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_Kane_2021.gdb",
                        layer = "Parcels_Kane_2021") %>% 
  select(pin = PIN)

# sf_use_s2(FALSE)

kane_join <- kane_parcels %>%
  left_join(kane_pins) %>%
  mutate(valid = st_is_valid(Shape))

kane_valid <- kane_join |> filter(valid == T)

kane_invalid <- kane_join |> filter(valid == F) |> mutate(Shape = st_make_valid(Shape))

kane_join_valid <- rbind(kane_valid, kane_invalid)

kane_shape <- kane_join_valid  %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(kane_shape, "outputs/tax_code_shapes/kane.gpkg")


# Kendall -----------------------------------------------------------------

kendall_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Kendall.gdb",
                        layer = "AssessorData_Kendall_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin = parcel_number,
         tax_code)

kendall_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_Kendall_2021.gdb",
                        layer = "Parcels_Kendall_2021") %>% 
  mutate(pin = str_remove_all(pin,"-")) %>% 
  select(pin)

kendall_join <- kendall_parcels %>% 
  left_join(kendall_pins)

kendall_shape <- kendall_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(kendall_shape, "outputs/tax_code_shapes/kendall.gpkg")


# Lake --------------------------------------------------------------------

lake_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Lake.gdb",
                       layer = "AssessorData_Lake_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>% 
  mutate(tax_code = str_sub(tax_code, end = 5),
         pin = str_remove_all(pin,"-")) %>% 
  select(pin,
         tax_code)

lake_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_Lake_2021.gdb",
                            layer = "Parcels_Lake_2021") 

lake_join <- lake_parcels %>% 
  left_join(lake_pins, by = c("PIN" = "pin"))

lake_shape <- lake_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(lake_shape, "outputs/tax_code_shapes/lake.gpkg")


# mchenry --------------------------------------------------------------------

mchenry_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_mchenry.gdb",
                        layer = "AssessorData_mchenry_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  transmute(pin,
            tax_code = str_remove(tax_code,"-")) |> 
  mutate(tax_code = case_when(
    str_length(tax_code) == 4 ~ str_c("0",tax_code),
    T ~ tax_code
  ),
  pin = as.character(pin)) %>% 
  mutate(pin = case_when(
    str_length(pin) == 9 ~ str_c("0",pin),
    T ~ pin
  ))
  

mchenry_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_McHenry_2021.gdb",
                        layer = "Parcels_McHenry_2021") %>% 
  mutate(pin = str_remove_all(PIN,"-")) %>% 
  select(pin)

mchenry_join <- mchenry_parcels %>% 
  left_join(mchenry_pins)

mchenry_shape <- mchenry_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(mchenry_shape, "outputs/tax_code_shapes/mchenry.gpkg")


# will --------------------------------------------------------------------

will_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_will.gdb",
                     layer = "AssessorData_will_2021") %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin,
         tax_code)

will_parcels <- st_read(dsn = "V:/Cadastral_and_Land_Planning/Parcels/Parcels_Will_2021.gdb",
                        layer = "Parcels_will_2021") %>% 
  select(pin = PIN)

will_join <- will_parcels %>% 
  left_join(will_pins)

will_shape <- will_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(will_shape, "outputs/tax_code_shapes/will.gpkg")
