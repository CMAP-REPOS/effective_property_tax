library(tidyverse)
library(readxl)
library(rio)


# cook --------------------------------------------------------------------

rates_cook_20 <- read_excel("outputs/3_effective_rates_cook.xlsx")

rates_cook_21 <- read_excel("C:\\Users\\abahls\\Downloads\\21_3_effective_rates_cook.xlsx") |> 
  rename(eff_rate_res_21 = eff_rate_res, eff_rate_ci_21 = eff_rate_ci)

join_cook <- rates_cook_20 |> 
  left_join(rates_cook_21) |> 
  mutate(res_diff = (eff_rate_res - eff_rate_res_21)*100,
         ci_diff = (eff_rate_ci - eff_rate_ci_21)*100)

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "raw/ptaxsim-2023.0.0.db_20241125")

cook_pins_20 <- DBI::dbGetQuery(ptaxsim_db_conn, paste0("select pin, class, tax_code_num, av_clerk from pin where year = ", 2020)) |> 
  rename_with(tolower) |> 
  select(pin,
         class,
         tax_code = tax_code_num,
         eav = av_clerk
  ) |> 
  mutate(class = ifelse(class == "0", "000", class),
         class = ifelse(class == "192", "190",class))


cook_pins_21 <- DBI::dbGetQuery(ptaxsim_db_conn, paste0("select pin, class, tax_code_num, av_clerk from pin where year = ", 2021)) |> 
  rename_with(tolower) |> 
  select(pin,
         class,
         tax_code = tax_code_num,
         eav = av_clerk
  ) |> 
  mutate(class = ifelse(class == "0", "000", class),
         class = ifelse(class == "192", "190",class))


# dupage ------------------------------------------------------------------



rates_dupage_20 <- read_excel("outputs/3_effective_rates_dupage.xlsx")

rates_dupage_21 <- read_excel("C:\\Users\\abahls\\Downloads\\21_3_effective_rates_dupage.xlsx") |> 
  rename(eff_rate_res_21 = eff_rate_res, eff_rate_ci_21 = eff_rate_ci)

join_dupage <- rates_dupage_20 |> 
  left_join(rates_dupage_21) |> 
  mutate(res_diff = (eff_rate_res - eff_rate_res_21)*100,
         ci_diff = (eff_rate_ci - eff_rate_ci_21)*100)



test2018 <- sf::read_sf("V:\\Cadastral_and_Land_Planning\\AssessorData\\AssessorData_Kane.gdb",
            file = "AssessorData_Kane_2018")

