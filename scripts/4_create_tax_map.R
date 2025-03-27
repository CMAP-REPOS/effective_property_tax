library(sf)
library(tidyverse)
library(RSQLite)
library(gdalUtilities)
library(httr)
library(readxl)

analysis_year <- "2022"

# load etr ----------------------------------------------------------------

annual_etrs_dir <- "T:\\ab\\annual_etrs\\3_effective_rates_"




# cook --------------------------------------------------------------------

ptaxsim_db_conn <- dbConnect(SQLite(), "T:\\ab\\ptaxsim-2023.0.0.db\\ptaxsim-2023.0.0.db")

cook_pins <- dbGetQuery(ptaxsim_db_conn, paste0("select pin, tax_code_num from pin where year =", analysis_year)) 

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
  summarise()

write_sf(cook_parcels_tc_dissolve,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/cook_map_", analysis_year,".gpkg"))
write_sf(cook_parcels_tc_dissolve,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/cook_map_", analysis_year,".shp"))


cook_tcs <- read_excel(paste0(annual_etrs_dir,"cook_",analysis_year, ".xlsx"))

cook_parcels_with_tax_code <- cook_parcels_tc_dissolve %>% 
  rename(Shape = geom) %>% 
  left_join(cook_tcs, by = c("tax_code_num" = "tax_code")) %>% 
  rename(tax_code = tax_code_num)

write_sf(cook_parcels_with_tax_code,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/cook_map_", analysis_year,"_with_rates.shp"))
write_sf(cook_parcels_with_tax_code,)paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/cook_map_", analysis_year,"_with_rates.gpkg")

# dupage ------------------------------------------------------------------
# 
dupage_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_DuPage.gdb",
                       layer = paste0("AssessorData_DuPage_",analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(tax_code = as.character(tax_code),
         pin = as.character(parcel_no)) %>%
  select(pin,
         tax_code)


dupage_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_DuPage_", analysis_year, ".gdb"),
                          layer = paste0("Parcels_DuPage_",analysis_year)) %>%
  select(pin = PIN, TAXCODE)

dupage_join <- dupage_parcels %>%
  left_join(dupage_pins) %>% 
  mutate(tax_code = coalesce(TAXCODE, tax_code)) %>% 
  select(!TAXCODE)

dupage_shape <- dupage_parcels %>%
  group_by(tax_code) %>%
  summarize()

write_sf(dupage_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/dupage_map_", analysis_year,".gpkg"))
write_sf(dupage_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/dupage_map_", analysis_year,".shp"))

dupage_tcs <- read_excel(paste0(annual_etrs_dir,"dupage_",analysis_year, ".xlsx"))

dupage_shape_with_tcs <- dupage_shape %>% 
  left_join(dupage_tcs)

write_sf(dupage_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/dupage_map_", analysis_year,"_with_rates.gpkg"))


# Kane --------------------------------------------------------------------
kane_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Kane.gdb",
                     layer = paste0("AssessorData_Kane_", analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin,
         tax_code)

kane_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Kane_",analysis_year,".gdb"),
                        layer = paste0("Parcels_Kane_", analysis_year)) %>% 
  select(pin = PIN)

kane_join <- kane_parcels %>% 
  left_join(kane_pins) %>% 
  mutate(valid = st_is_valid(Shape),
         Shape = case_when(
           valid == F ~ st_make_valid(Shape),
           T ~ Shape
         )) %>% 
  select(!valid)

kane_shape <- kane_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(kane_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/kane_map_", analysis_year,".gpkg"))

kane_tcs <- read_excel(paste0(annual_etrs_dir,"kane_",analysis_year, ".xlsx"))

kane_shape_with_tcs <- kane_shape %>% 
  left_join(kane_tcs)

write_sf(kane_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/kane_map_", analysis_year,"_with_rates.gpkg"))


# kendall -----------------------------------------------------------------

kendall_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_kendall.gdb",
                        layer = paste0("AssessorData_kendall_", analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin = parcel_number,
         tax_code) %>% 
  mutate(pin = as.character(pin))

kendall_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_kendall_", analysis_year,".gdb"),
                           layer = paste0("Parcels_kendall_",analysis_year)) %>% 
  mutate(pin = str_sub(pin_dashle, 2, 10)) %>% 
  select(pin)

kendall_join <- kendall_parcels %>% 
  left_join(kendall_pins)

kendall_shape <- kendall_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(kendall_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/kendall_map_", analysis_year,".gpkg"))


kendall_tcs <- read_excel(paste0(annual_etrs_dir,"kendall_",analysis_year, ".xlsx"))

kendall_shape_with_tcs <- kendall_shape %>% 
  left_join(kendall_tcs)

write_sf(kendall_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/kendall_map_", analysis_year,"_with_rates.gpkg"))


# Lake --------------------------------------------------------------------

lake_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Lake.gdb",
                       layer = paste0("AssessorData_Lake_", analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  mutate(tax_code = str_sub(tax_code, end = 5),
         pin = str_remove_all(pin,"-")) %>%
  select(pin,
         tax_code)

lake_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Lake_", analysis_year, ".gdb"),
                        layer = paste0("Parcels_Lake_",analysis_year))

lake_join <- lake_parcels %>%
  left_join(lake_pins, by = c("PIN" = "pin"))

lake_shape <- lake_join %>%
  group_by(tax_code) %>%
  summarize() %>% 
  st_transform(crs = cmapgeo::cmap_crs)

write_sf(lake_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/lake_map_", analysis_year,".gpkg"))

lake_tcs <- read_excel(paste0(annual_etrs_dir,"lake_",analysis_year, ".xlsx"))

lake_shape_with_tcs <- lake_shape %>% 
  left_join(lake_tcs)

write_sf(lake_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/lake_map_", analysis_year,"_with_rates.gpkg"))


# mchenry --------------------------------------------------------------------

mchenry_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_mchenry.gdb",
                        layer = paste0("AssessorData_mchenry_", analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  transmute(pin = parcel_number,
            tax_code = str_remove(tax_code,"-")) |> 
  mutate(tax_code = case_when(
    str_length(tax_code) == 4 ~ str_c("0",tax_code),
    T ~ tax_code
  ),
  pin = str_remove_all(as.character(pin), "-")) 
  

mchenry_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_McHenry_", analysis_year, ".gdb"),
                           layer = paste0("Parcels_McHenry_",analysis_year)) %>% 
  mutate(pin = str_remove_all(PIN,"-")) %>% 
  select(pin)

mchenry_join <- mchenry_parcels %>% 
  left_join(mchenry_pins)

mchenry_shape <- mchenry_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(mchenry_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/mchenry_map_", analysis_year,".gpkg"))


mchenry_tcs <- read_excel(paste0(annual_etrs_dir,"mchenry_",analysis_year, ".xlsx"))

mchenry_shape_with_tcs <- mchenry_shape %>% 
  left_join(mchenry_tcs)

write_sf(mchenry_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/mchenry_map_", analysis_year,"_with_rates.gpkg"))


# will --------------------------------------------------------------------

will_pins <- st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_will.gdb",
                     layer = paste0("AssessorData_will_", analysis_year)) %>%
  rename_with(tolower) %>%
  as_tibble() %>%
  select(pin,
         tax_code) %>% 
  mutate(pin = as.character(pin))

will_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Will_", analysis_year, ".gdb"),
                                layer = paste0("Parcels_Will_",analysis_year)) %>% 
  select(pin = PIN) %>% 
  mutate(pin = case_when(analysis_year == 2022 & str_sub(pin, 1, 1) == 0 ~ str_sub(pin, 2, 16),
                         T ~ pin))

will_join <- will_parcels %>% 
  left_join(will_pins)

will_shape <- will_join %>% 
  group_by(tax_code) %>% 
  summarize()

write_sf(will_shape,paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/will_map_", analysis_year,".gpkg"))


will_tcs <- read_excel(paste0(annual_etrs_dir,"will_",analysis_year, ".xlsx")) %>% 
  mutate(tax_code = case_when(analysis_year == 2022 & str_sub(tax_code, 1, 1) == 0 ~ str_sub(tax_code, 2, 4),
                              T ~ tax_code))

will_shape_with_tcs <- will_shape %>% 
  mutate(tax_code = as.character(tax_code)) %>% 
  left_join(will_tcs)

write_sf(will_shape_with_tcs, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/will_map_", analysis_year,"_with_rates.gpkg"))


# combine -----------------------------------------------------------------

# combined <- rbind(cook_parcels_with_tax_code, dupage_shape_with_tcs, kane_shape_with_tcs, kendall_shape_with_tcs, lake_shape_with_tcs, mchenry_shape_with_tcs, will_shape_with_tcs) %>% 
#   st_zm()

combined <- rbind(cook_parcels_with_tax_code, kane_shape_with_tcs, kendall_shape_with_tcs, mchenry_shape_with_tcs, will_shape_with_tcs) %>% 
  st_zm()

write_sf(combined, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/all_map_", analysis_year,"_with_rates.gpkg"))
write_sf(combined, paste0("S:/Projects/CCER/Complete_Shapefiles/tax_code_maps/all_map_", analysis_year,"_with_rates.shp"))
