library(tidyverse)
library(readxl)
library(rio)


# cook --------------------------------------------------------------------

rates_cook_22 <- read_excel("outputs/3_effective_rates_cook_2022.xlsx")

rates_cook_21 <- read_excel("C:\\Users\\abahls\\Downloads\\3_effective_rates_cook_2021.xlsx") |> 
  rename(eff_rate_res_21 = eff_rate_res, eff_rate_ci_21 = eff_rate_ci)

join_cook <- rates_cook_22 |> 
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



rates_cook_22 <- read_excel("outputs/3_effective_rates_will_2023.xlsx")

rates_cook_21 <- read_excel("C:\\Users\\abahls\\Downloads\\3_effective_rates_will_2021.xlsx") |> 
  rename(eff_rate_res_21 = eff_rate_res, eff_rate_ci_21 = eff_rate_ci)

join_cook <- rates_cook_22 |> 
  left_join(rates_cook_21) |> 
  mutate(res_diff = (eff_rate_res - eff_rate_res_21)*100,
         ci_diff = (eff_rate_ci - eff_rate_ci_21)*100)

test2018 <- sf::read_sf("V:\\Cadastral_and_Land_Planning\\AssessorData\\AssessorData_Kane.gdb",
            file = "AssessorData_Kane_2018")

