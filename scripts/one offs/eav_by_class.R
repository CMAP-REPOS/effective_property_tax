

# load packages
library(tidyverse)
library(janitor)
library(here)
library(openxlsx)

analysis_year <- 2023

## 1. Load required resources --------------------------------------------------

# property class summaries
load(here("internal", "classes.RData"))

# pins with EAV/MVs, tax codes, and property classes
load(here("internal", "pins.RData"))

cook_pins_class <- pins$cook %>% 
  left_join(classes$cook) %>% 
  filter(eav > 0,
         category != "Exempt/Railroad") %>% 
  group_by(category) %>% 
  reframe(total_eav = sum(eav)) %>% 
  ungroup() %>% 
  mutate(percent_eav = round((total_eav*100)/sum(total_eav)))

write.xlsx(cook_pins_class, "outputs//eav_by_class_cook_23.xlsx")
