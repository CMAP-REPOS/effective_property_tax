library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(here)

options(scipen = 999)

#this year, Chicago's home equity assurance districts are categorized as "TORT JUDGEMENTS, LIAB & GEN INS" and we want to include them
#should confirm each year by cross checking the assurance district extensions in the Cook Agency Extension Report 

# https://tax.illinois.gov/research/taxstats/propertytaxstatistics.html
table_27 <- here("raw", "2021Table27Revised.xlsx") |> 
  read.xlsx() |> 
  clean_names()|> 
  filter(fund_name == toupper("Special Service Area") |
         fund_name == "TORT JUDGEMENTS, LIAB & GEN INS" &  district_id == "0160162400014") |> #chicago home equity districts
  filter(fund_extension > 0) |> 
  group_by(district_id) |> 
  summarize(ssa_extension = sum(fund_extension))

table_28 <- here("raw", "y2021tbl28.xlsx") |> 
  read.xlsx(startRow = 4) |> 
  clean_names() |> 
  left_join(table_27) |> 
  mutate(extension_no_ssa = case_when(
    !is.na(ssa_extension) ~ extension - ssa_extension,
    T ~ extension
  ))

write_rds(table_28, "resources/Table28_Processed.rda")
