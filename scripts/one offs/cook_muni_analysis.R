library(sf)
library(tidyverse)
library(tidycensus)
library(here)

##################### request
# 2023 tax year
  # Cook
  # all munis that are only in Cook
  # median home value
  # attirbutability (% of CI tax rate due to classification)
  # MV mix, res and CI
#########################


# 1. find munis only in cook ----------------------------------------------

muni_area_df <- cmapgeo::municipality_sf %>% 
  select(geoid_place, municipality) %>% 
  mutate(muni_area = as.numeric(st_area(.))) %>% 
  st_transform(crs = cmapgeo::cmap_crs)

cook_shape <- cmapgeo::county_sf %>% 
  filter(county == "Cook") %>% 
  select(county) %>% 
  st_transform(crs = cmapgeo::cmap_crs)
  
muni_cook_intersection <- st_intersection(muni_area_df, cook_shape) %>% 
  mutate(intersection_area = as.numeric(st_area(.))) %>% 
  group_by(geoid_place) %>% 
  reframe(total_cook_area = sum(intersection_area)) %>% 
  left_join(muni_area_df) %>% 
  mutate(pct_in_cook = round(total_cook_area/muni_area, 2)) %>% 
  filter(pct_in_cook > 0.95) %>% 
  select(geo)

#2. match tax codes to munis ------------

tax_codes_with_dist <- readxl::read_excel("outputs\\2_dists_by_taxcode_proc_cook_2023.xlsx") %>% 
  mutate(muni = str_to_title(Municipality_1)) %>% 
  select(tax_code, muni) %>% 
  filter(!is.na(muni)) %>% 
  mutate(muni = case_when(
    str_detect(muni, "Hazelcrest") ~ str_replace(muni, "Hazelcrest", "Hazel Crest"),
    str_detect(muni, "Hts") ~ str_replace(muni, "Hts", "Heights"),
    str_detect(muni, "Lagrange") ~ str_replace(muni, "Lagrange", "La Grange"),
    muni == "Cicero Twp" ~ "Cicero", #township and muni are coextensive
    muni == "Forestview" ~ "Forest View", 
    muni == "Lynnwood" ~ "Lynwood", 
    muni == "Mccook" ~ "McCook", 
    muni == "Mt Prospect" ~ "Mount Prospect", 
    muni == "North Lake" ~ "Northlake", 
    muni == "Indian Head" ~ "Indian Head Park", 
    T ~ muni
  ))
  

# 3. join pins  -----------------------------------------------------------

load(here("internal", "pins.RData"))

cook_pins <- pins$cook

rm(pins)

#this should have 0 rows
# qa_all_munis <- muni_cook_intersection %>% 
#   filter(!municipality %in% tax_codes_with_dist$muni)


#  old --  median home value -------------------------------------------------------

# #find the Census var -- with help -- https://censusreporter.org/topics/housing/
# vars_acs <- load_variables(2023, "acs1") #B25077_001
# 
# median_income_pull <- get_acs(
#   geography = "place",
#   variables = "B25077_001",
#   year = 2023,
#   survey = "acs5", #need to use 5-year as many munis don't hit one year pop floor of 65k
#   state = "il"
# )
# 
# cook_muni_with_med_value <- 

#assign each parcel to a muni

  

