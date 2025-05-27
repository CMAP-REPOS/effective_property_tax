library(sf)
library(tidyverse)
library(here)

analysis_year <- "2023"

# load etr ----------------------------------------------------------------

annual_etrs_dir <- "T:\\ab\\annual_etrs\\3_effective_rates_"

# assign parcels to CCAs  --------------------------------------------------------------------

cook_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Cook_", analysis_year,".gdb"),
                        layer = paste0("Parcels_Cook_", analysis_year))  |>  
  select(PIN = PIN10)

cook_ccas <- st_join(cook_parcels %>% st_centroid(), cmapgeo::cca_sf) %>% 
  filter(!is.na(cca_name)) %>% 
  rename(pin = PIN) %>% 
  as_tibble() %>% 
  select(pin, cca_name, cca_num) %>% 
  distinct() %>% 
  group_by(pin) %>% #0.01% (n = 63) have locations in two ccas, just going to randomly pick one fore time
  slice_sample(n = 1) %>%
  ungroup()

n_distinct(cook_ccas$pin)
# load pins ---------------------------------------------------------------

load(here("internal", "pins.RData"))

cook_pins_no_zeros <- pins$cook %>% 
  filter(eav > 0) %>% 
  select(pin, tax_code, eav, class) %>% 
  mutate(pin = str_sub(pin, 1 ,10)) %>% 
  distinct() %>% #122 pins with multiplier tax codes, , just going to randomly pick one fore time
  group_by(pin) %>%
  slice_sample(n = 1) %>%
  ungroup()
  
  
cook_pins_with_cca_no_zeros <- cook_ccas %>% 
  left_join(cook_pins_no_zeros)


# zeros -------------------------------------------------------------------

cook_pins_all <- pins$cook %>% 
  select(pin, tax_code, eav, class) %>% 
  mutate(pin = str_sub(pin, 1 ,10)) %>% 
  distinct() %>% #122 pins with multiplier tax codes, , just going to randomly pick one fore time
  group_by(pin) %>%
  slice_sample(n = 1) %>%
  ungroup()


cook_pins_with_cca_all <- cook_ccas %>% 
  left_join(cook_pins_all)


load(here("internal", "classes.RData"))

etr <- readxl::read_excel("outputs\\3_effective_rates_cook_2023.xlsx", 
                          sheet = "eff rates - taxcode")

no_zero_final <- cook_pins_with_cca_no_zeros %>% 
  left_join(classes$cook) %>% 
  left_join(etr) %>% 
  filter(!is.na(tax_code)) %>% 
  mutate(
    eff_rate_res = case_when(
      category == "Residential" ~ eff_rate_res,
      T ~ NA),
    eff_rate_ci = case_when(
      category %in% c("Commercial", "Industrial") ~ eff_rate_ci,
      T ~ NA
    )
  ) %>% 
  group_by(cca_name, cca_num) %>% 
  reframe(eff_rate_res_zeros_removed = mean(eff_rate_res, na.rm = T),
          eff_rate_ci_zeros_removed = mean(eff_rate_ci, na.rm = T))

all_final <- cook_pins_with_cca_all %>% 
  left_join(classes$cook) %>% 
  left_join(etr) %>% 
  filter(!is.na(tax_code)) %>% 
  mutate(
    eff_rate_res = case_when(
      category == "Residential" ~ eff_rate_res,
      T ~ NA),
    eff_rate_ci = case_when(
      category %in% c("Commercial", "Industrial") ~ eff_rate_ci,
      T ~ NA
    )
  ) %>% 
  group_by(cca_name, cca_num) %>% 
  reframe(eff_rate_res = mean(eff_rate_res, na.rm = T),
          eff_rate_ci = mean(eff_rate_ci, na.rm = T))

join <- cmapgeo::cca_sf %>% 
  select(!sqmi) %>% 
  left_join(all_final) %>% 
  left_join(no_zero_final) %>% 
  mutate(across(c(eff_rate_res, eff_rate_ci, eff_rate_res_zeros_removed, eff_rate_ci_zeros_removed), \(x) x*100))

write_sf(join, "T:\\ab\\etr_maps\\cca_by_average_rate\\cca_map_file.gpkg")
