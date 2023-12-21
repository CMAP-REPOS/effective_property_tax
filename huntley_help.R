hfpd_raw <- dists_by_taxcode_raw$mchenry |> filter(tax_district_name == "HUNTLEY FIRE DIST")

hfpd <- dists_by_taxcode_proc$mchenry |> 
  filter(Fire_Protection_District == "HUNTLEY FPD") 

fpd_pins <- pins$mchenry |> 
  filter(tax_code %in% hfpd$tax_code)

mch_commer_class <- classes$mchenry |> filter(category == "Commercial")

fpd_pins_comm <- fpd_pins |> 
  filter(class %in% mch_commer_class$class)

sum(fpd_pins_comm$eav)


fpd_eav_by_type <- fpd_pins |> 
  left_join(classes$mchenry) |> 
  group_by(category) |> 
  summarize(total_eav = sum(eav)) |> 
  janitor::adorn_totals(where = "row")



library(xlsx)
write.xlsx(dists_by_taxcode_raw$mchenry, file="huntley_fpd.xlsx", sheetName="McHenry -- all Tax Codes with districts", rowNames=FALSE)
write.xlsx(dists_by_taxcode_proc$mchenry, file="huntley_fpd.xlsx", sheetName="McHenry -- Tax Codes present in Pin data with districts", rowNames=FALSE)
write.xlsx(hfpd_raw, file="huntley_fpd.xlsx", sheetName="Huntley FPD -- All tax codes", rowNames=FALSE)
write.xlsx(hfpd, file="huntley_fpd.xlsx", sheetName="Huntley FPD -- Tax Codes present in PIN data", rowNames=FALSE)
write.xlsx(classes$mchenry, file="huntley_fpd.xlsx", sheetName="McHenry -- All Property Classes", rowNames=FALSE)
write.xlsx(mch_commer_class, file="huntley_fpd.xlsx", sheetName="McHenry -- Commericial Property Classes", rowNames=FALSE)
write.xlsx(pins$mchenry, file="huntley_fpd.xlsx", sheetName="McHenry -- All Pins", rowNames=FALSE)
write.xlsx(fpd_pins, file="huntley_fpd.xlsx", sheetName="Huntley FPD -- all pins in tax codes with Huntley FPD ", rowNames=FALSE)
write.xlsx(fpd_pins_comm, file="huntley_fpd.xlsx", sheetName="Huntley FPD -- commercial pins in tax codes with Huntley FPD ", rowNames=FALSE)
