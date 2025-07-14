library(sf)
library(tidyverse)
library(RSQLite)
library(gdalUtilities)
library(httr)
library(readxl)

analysis_year <- "2023"


#1. make parcel/cca xwalk ---------------------------------------------------------

cook_parcels <- st_read(dsn = paste0("V:/Cadastral_and_Land_Planning/Parcels/Parcels_Cook_", analysis_year,".gdb"),
                        layer = paste0("Parcels_Cook_", analysis_year))  |>  
  select(PIN = PIN10)

cca_intersect <- st_intersection(cmapgeo::cca_sf, cook_parcels)

#some parcels are in multiple CCAs
cca_pin_map <- cca_intersect %>%
  mutate(intersection_area = as.numeric(st_area(.))) %>% 
  group_by(cca_name, cca_num, PIN) %>% 
  reframe(parcel_area_in_cca = sum(intersection_area)) %>% 
  group_by(PIN) %>% 
  mutate(max_cca_area = max(parcel_area_in_cca)) %>% 
  ungroup() %>% 
  filter(parcel_area_in_cca == max_cca_area) %>% 
  as_tibble() %>% 
  select(cca_name, cca_num, pin = PIN)

#2. load pins ---------------------------------------------------------
load(here("internal", "pins.RData"))
load(here("internal", "classes.RData"))

cook_pins <- pins$cook %>% 
  left_join(classes$cook) %>% 
  mutate(mv = eav/assessment_rate) %>% 
  filter(category != "Exempt/Railroad",
         mv > 0) %>% 
  select(pin, tax_code, category, mv)

rm(pins, classes)

# zero_mv_test <- cook_pins_with_mv %>% filter(mv <= 0) ##these appear to just be errors
#                                                       #in assessor data, fine to remove,
#                                                       #not many and appear random



#3. add ccas to pins --------------------------------------------------------

cook_pins_with_cca <- cook_pins %>% 
  mutate(pin = str_sub(pin, 1, 10)) %>% 
  left_join(cca_pin_map) %>% 
  filter(!is.na(cca_name))


# 4. calc attiributabilty by tax code -------------------------------------

real_etrs <- read_excel("outputs\\3_effective_rates_cook_2023.xlsx")

#load github files
loadWorkbook_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url = url, destfile = temp_file, mode = "wb", quiet = TRUE)
  readxl::read_excel(temp_file)}

counterfactual_etrs <- loadWorkbook_url(paste0("https://github.com/CMAP-REPOS/effective_property_tax/raw",
                                               "/refs/heads/2023_analysis_cook_uniform/outputs/",
                                               "cook_one_third_modified_extension_2023.xlsx")) %>% 
  select(tax_code, eff_rate_res_cf = eff_rate_res, eff_rate_ci_cf = eff_rate_ci)

tc_attributability <- real_etrs %>% 
  left_join(counterfactual_etrs) %>% 
  mutate(percent_due_to_class_ci = (eff_rate_ci - eff_rate_ci_cf)/ eff_rate_ci) %>% 
  select(tax_code, percent_due_to_class_ci)


# 5. analysis -------------------------------------------------------------

joined_df <- cook_pins_with_cca %>% 
  left_join(tc_attributability)


#to simplify things, I'm going to calculate each variable requested separately

## 5a. -- median home value ---------------------------------------------------
median_res_value <- joined_df %>% 
  filter(category == "Residential") %>% 
  group_by(cca_name) %>% 
  reframe(median_home_value = median(mv))

## 5b. -- mv mix  --------------------------------------------------------------------

mv_mix <- joined_df %>%
  group_by(cca_name, category) %>% 
  reframe(cat_mv = sum(mv)) %>% 
  mutate(category = str_c(category, "_mv")) %>% 
  pivot_wider(id_cols = cca_name, names_from = category, values_from = cat_mv) %>% 
  janitor::clean_names() %>% 
  mutate(across(where(is.numeric), \(x) coalesce(x, 0)),
         total_mv = commercial_mv + industrial_mv + residential_mv + vacant_mv + farm_open_space_mv,
         pct_mv = across(commercial_mv:farm_open_space_mv, \(x) x/total_mv))

## 5c. attributability -----------------------------------------------------

attr <- joined_df %>% 
  filter(category %in% c("Commercial", "Industrial")) %>% 
  left_join(tc_attributability) %>% 
  group_by(cca_name) %>% 
  reframe(average_ci_attributability = mean(percent_due_to_class_ci))


## 5d. combine -------------------------------------------------------------




# median home value
# attirbutability (% of CI tax rate due to classification)
# MV mix, res and CI


combined <- median_res_value %>% 
  left_join(attr) %>% 
  left_join(mv_mix) %>%
  unnest_wider(pct_mv, names_sep = "pct")

names(combined) <- str_remove(names(combined), "mvpct")


writexl::write_xlsx(combined, "outputs/one offs/cook_cca_analysis.xlsx")