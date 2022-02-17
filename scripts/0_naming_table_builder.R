
## Naming table ----------------------------------------------------------------

# The naming table is an excel file CMAP has maintained for years identifying taxing
# districts un the region by their various names, so that data from various 
# sources can be matched.

# Both scripts 2 and 3 rely heavily on the excel-based naming table for naming
# adjustments. This script loads up a function that interprets the naming table
# from Excel into a list of counties for matching with other data.

# This script, when sourced, creates a function to rebuild the naming table in R
# from the source excel, and runs the function once.


# build the function
build_naming_table <- function(){
  raw <- read.xlsx(here("resources", "NamingTable.xlsx"),
                                sheet = "naming") %>% 
    # Confirm field names in naming table
    select(county = County, tax_district_name = Name, 
           IDOR_name = IDOR.Name, IDOR_code = IDOR.Code,
           district_type = Type.of.District, other_name = Other.Name) %>% 
    # naming table was originally created to match imports exactly, including with
    # extra spaces. This is now unnecessary and problematic, as all extra spaces 
    # have been removed from input files. Clean up naming table to match.
    mutate(across(everything(), str_squish))
  
  
  # split naming table into list of tables by county
  processed <- raw %>% 
    mutate(county = as_factor(tolower(county))) %>% 
    split(., .$county) %>% 
    map(select, -county)
  
  
  # confirm list is named and ordered correctly:
  if(!(identical(
    names(processed), 
    c("cook", "dupage", "kane", "kendall", "lake", "mchenry", "will")))){
    stop("Naming table build does not result in an alphabetically-organized list of counties",
         call. = FALSE)
  }
  
  # add this to an object in the GLOBAL ENVIRONMENT
  naming_table <<- processed
  
  invisible(NULL)
}


build_naming_table()
