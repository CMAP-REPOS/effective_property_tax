library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(here)

options(scipen = 999)

counties <- c("dupage", "kane", "kendall", "lake", "mchenry", "will","cook")
# 
# tbl28_raw <- read.xlsx(here("resources", "Y2018Tbl28.xlsx"), sheet = "Table28Data") %>% 
#   set_names(~tolower(str_replace_all(.,"\\.","_")))
# 
# tbl28 <- as_tibble(tbl28_raw) %>% 
#   # convert county to factor for splitting later, keeping only counties in the region
#   mutate(primary_county = factor(tolower(primary_county), levels = counties)) %>% 
#   filter(!is.na(primary_county)) %>% 
#   # clean up table
#   arrange(primary_county) %>% 
#   select(1:5, 
#          ext_tot = total_extension_nossa,
#          ext_res = residential_extension_new,
#          ext_com = commercial_extension_new,
#          ext_ind = industrial_extension_new,
#          ends_with("_new"), # this keeps only extensions in the format that ends with "_new"
#          -total_farm_extension_new) %>% # but drop total farm extension, because farm A and farm B duplicate this
#   mutate(
#     # sum these non R/C/I extensions
#     ext_other = rowSums(select(.,ends_with("_new")), na.rm = TRUE),
#     # verity that sub extensions sum to total extension
#     ext_tot2 = ext_res + ext_com + ext_ind + ext_other) %>% 
#   # align names with other tables and drop unnecessary columns
#   select(tax_district = district_id, 
#          tax_district_name = district_name,
#          tax_district_type = type_code,
#          starts_with("ext_"),
#          primary_county) %>% 
#   # split into list of dfs, dropping unnecessary columns
#   split(., .$primary_county, drop = TRUE) %>% 
#   map(select, -primary_county, -ext_tot2)

tbl28_raw_idor <-read_excel("C:/Users/abahls/Downloads/y2018tbl28_raw.xlsx") |> clean_names()

tbl27_raw <-read_excel("C:/Users/abahls/Downloads/y2018tbl27.xlsx") |> clean_names() 

tbl27_ssa <- tbl27_raw |> 
  filter(str_detect(tolower(fund_name),"ssa") | str_detect(tolower(fund_name),"special service area"))

table(tbl27_ssa$fund_name)

tbl_27_dist <- tbl27_ssa |> 
  group_by(district_id) |> 
  summarize(total_ssa_extension = sum(fund_extension))

tbl_28_test_mod <- tbl28_raw_idor |> 
  left_join(tbl_27_dist) |> 
  mutate(total_ssa_extension = ifelse(is.na(total_ssa_extension),0,total_ssa_extension),
         mod_extension = total_extension - total_ssa_extension,
         changed = ifelse(mod_extension == total_extension,0,1)) |> 
  select(district_id, total_extension, total_ssa_extension, mod_extension, changed)

names(tbl_28_test_mod) <- str_c(names(tbl_28_test_mod),"_test")

tbl_28_test_mod <- tbl_28_test_mod |> rename(district_id = district_id_test)


tbl_28_cmap_mod <- read_excel("C:/Users/abahls/OneDrive - Chicago Metropolitan Agency for Planning/Documents/github_repos/effective_property_tax/resources/Y2018Tbl28.xlsx",
                              sheet = "Table28Data") |> clean_names()

tbl_28_cmap_mod_join <- tbl_28_cmap_mod |> 
  select(district_id, primary_county, total_extension, total_extension_no_ssa, special_service_area) |> 
  mutate(diff_total = ifelse(total_extension!=total_extension_no_ssa,1,0)) |> 
  left_join(tbl_28_test_mod) |> 
  filter(primary_county %in% toupper(names(cmapgeo::county_fips_codes$cmap)))

on_both <- tbl_28_cmap_mod_join |> filter(diff_total == 1 & changed_test == 1) |> 
  mutate(ssa_diff = round(special_service_area - total_ssa_extension_test),
         ext_diff = total_extension_no_ssa - mod_extension_test) |> 
  arrange(desc(ssa_diff))

helper <- on_both |> select(district_id, total_ssa_extension_test, special_service_area, ssa_diff, primary_county)

helper2 <- tbl27_raw |> 
  select(district_id, fund_name, fund_extension)|> 
  left_join(helper) |> 
  filter(!is.na(total_ssa_extension_test)) |> 
  arrange(desc(ssa_diff), fund_name)

writexl::write_xlsx(helper2,"tbl28_help/differences.xlsx")


just_cmap <- tbl_28_cmap_mod_join |> filter(diff_total == 1 & changed_test == 0)

just_test <- tbl_28_cmap_mod_join |> filter(diff_total == 0 & changed_test == 1)

humph <- tbl27_raw |> filter(district_id == "0220222400090")
















tbl27_ssa_etc_grouped <- tbl27_ssa_etc |> 
  group_by(district_id) |> 
  summarize(total_ssa_etc_ext = sum(fund_extension))




tbl28_proc <- read_excel("C:/Users/abahls/OneDrive - Chicago Metropolitan Agency for Planning/Documents/github_repos/effective_property_tax/resources/Y2018Tbl28.xlsx",
                         sheet = "Table28Data") |> clean_names()

names(tbl28_proc) <- str_c(names(tbl28_proc),"_proc")

diff_28 <- tbl28_raw_idor |> 
  left_join(tbl28_proc, by = c("district_id" = "district_id_proc"))

changes <- diff_28 |> 
  filter(tolower(primary_county_proc) %in% counties) |> 
  left_join(tbl27_ssa_etc_grouped) |> 
  mutate(diff = case_when(
    total_extension != total_extension_no_ssa_proc ~ 1,
    T ~ 0
  ))

test1 <- changes |> 
  filter(diff == 0 & total_ssa_etc_ext!= 0) |> 
  select(starts_with("district"), total_extension, total_extension_no_ssa_proc, total_ssa_etc_ext)

# changes <- diff_28 |> 
#   filter(total_extension != total_extension_no_ssa_proc,
#          tolower(primary_county) %in% counties) |> 
#   select(district_id, district_name, total_extension, total_extension_no_ssa_proc, primary_county) |> 
#   mutate(diff = total_extension - total_extension_no_ssa_proc)




merged <- changes |> 
  left_join(tbl27_raw)
