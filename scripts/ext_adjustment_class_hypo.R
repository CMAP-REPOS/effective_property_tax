
#step 1 -- get the eav share by district
options(scipen = 999)

mod_pins_table <- pins$cook %>% 
  left_join(classes$cook) %>% 
  mutate(calc_mv = case_when(
    assessment_rate == 0 ~ 0,
    T ~ eav/assessment_rate
  ),
  new_eav = calc_mv*(1/3)
  ) %>% 
  select(pin, class, tax_code, new_eav, category)

cook_tc_dist_long <- dists_by_taxcode_proc$cook %>% 
  pivot_longer(-tax_code) %>% 
  rename(tax_district_name = value) %>% 
  filter(!is.na(tax_district_name))

cook_tc_dist_long_pins <- cook_tc_dist_long %>% 
  left_join(mod_pins_table) %>% 
  mutate(eav = case_when(
    class == "000" ~ 0,
    T ~ new_eav
  )) 

cook_eav_proc <- cook_tc_dist_long_pins %>% 
  group_by(category, tax_district_name) %>% 
  reframe(total_eav_dist_class = sum(eav)) %>% 
  group_by(tax_district_name) %>% 
  mutate(total_eav_dist = sum(total_eav_dist_class)) %>% 
  ungroup() %>% 
  mutate(percent_eav = total_eav_dist_class/total_eav_dist) %>% 
  select(category, tax_district_name, percent_eav) 

cook_eav_proc_wide <- cook_eav_proc %>% 
  pivot_wider(tax_district_name, names_from = "category", values_from = "percent_eav") %>% 
  clean_names() %>% 
  replace(is.na(.), 0)

new_ext <- final_extensions$cook %>% 
  left_join(cook_eav_proc_wide) %>% 
  mutate(
    new_ext_res = ext_tot*residential,
    new_ext_com = ext_tot*commercial,
    new_ext_ind = ext_tot*industrial,
    new_ext_other = ext_tot*(exempt_railroad + farm_open_space + vacant)
  ) %>% 
  select(tax_district_name, ext_tot, ext_res = new_ext_res, ext_com = new_ext_com,
         ext_ind = new_ext_ind, ext_other = new_ext_other)

final_extensions$cook <- new_ext %>% mutate(ext_src = "computed_modified")
