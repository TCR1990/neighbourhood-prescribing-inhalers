#### 00.3_get_BNF_info_from_OP.R ####
"

Calls the OpenPrescribing.net API to get full drug details (including full bnf_name and is_generic field).

Requires:
  ./data-source/BNF_codes_combined_pre_checks.csv (generated by 00.0_source_data.Rmd)
  
Outputs
  ./data/BNF_codes_measures.csv

NOTES:
- Final line clears all variables in the workspace except pre-exsiting variables and 'BNF_codes_measures'.

"

#### Import libraries ####
library(tidyverse)
library(httr)
library(jsonlite)

#### Workspace setup ####
# log existing variables
existing_vars = ls()

# get directory of current script and set it as working directory
filedir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(filedir)
getwd()

# create data folder if not yet existent
if (!dir.exists("./data-source")) {
  dir.create("./data-source")
}

# create data folder if not yet existent
if (!dir.exists("./data")) {
  dir.create("./data")
}

#### Function definitions ####
rbind.all.columns <- function(x, y) {
  # binds all columns   
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

api_get_bnf_info <- function(bnf_code) {
  # Call OpenPrescribing API to get full info from BNF code
  
  # Build URL
  url = paste("https://openprescribing.net/api/1.0/bnf_code/?q=",
              bnf_code,
              "&exact=true&format=json",
              sep = "")
  
  # Count number of attempts
  attempts = 1
  
  # Call the API and act depending on return code
  while(try(api_table_JSON <- GET(url))$status != 200) { # Return code 200 = OK
    
    # Return code 429 = 'Too Many Requests'
    if(api_table_JSON$status == 429){
      cat(paste("\nAPI return code ", api_table_JSON$status, " - 'Too Many Requests' error after ", attempts, " attempt(s). Retrying... ", sep = ""))
      attempts = attempts + 1    
      Sys.sleep(1) # Try again in 1 s
    }
    
    # Return code 404 = 'Page Not Found'
    if(api_table_JSON$status == 404){
      cat(paste("\nAPI return code ", api_table_JSON$status, ": 'Page Not Found'. Moving to next..."))
      break}
    
    # Return code not recognized
    if(api_table_JSON$status %in% c(404, 200, 429) == FALSE){
      cat(paste("\nAPI return code ", api_table_JSON$status, ": No handling for this return. Moving to next..."))
      break}
  }
  
  # Check if any data was received, and if so decode from JSON  
  api_table = fromJSON(rawToChar(api_table_JSON$content))
  if(length(api_table) > 1){
    return(api_table)
  } else{
    return(0)
  }
}

#### Read in data file containing codes to be checked ####
BNF_codes_combined_pre_checks <- read.csv("./data-source/BNF_codes_combined_pre_checks.csv")

#### Loop through BNF codes and retrieve info from OpenPrescribing API ####
num=0
for (product_code in BNF_codes_combined_pre_checks$bnf_code) {
  
  # if (exists("api_table")){rm("api_table")}
  num = num+1
  
  # Print what is going on
  cat(paste("\n\n", product_code, ' NUMBER: ', num, ' of ', length(BNF_codes_combined_pre_checks$bnf_code), sep = ""))
  
  # Check code in API. DF returned if successful, 0 if not data from API.
  api_table = api_get_bnf_info(product_code)
  
  # Check if data returned
  if(length(api_table) != 1){
    
    # Append to existing output table if exists
    if (exists("product_table")) {
      cat("\nReceived, binding.")
      product_table <- rbind.all.columns(product_table, api_table)
    }else{
      # First result becomes table
      product_table <- api_table
      cat("\nFirst entry received.")
    }
  }else{
    # No data received, clean up and move to next
    rm("api_table")
    cat(paste("\nNo data received for this code from API."))
    next
  }
}

#### Merge and sort dataframe for output ####
# Remove duplicates and rename columns
product_table <- product_table %>% unique()
colnames(product_table)[colnames(product_table) == "id"] = "bnf_code"
colnames(product_table)[colnames(product_table) == "name"] = "bnf_name"

# Combine with original table
BNF_codes_measures <- full_join(product_table, BNF_codes_combined_pre_checks, by="bnf_code") %>%
  mutate(bnf_name.y = ifelse(is.na(bnf_name.x), bnf_name.y, bnf_name.x),
         is_generic = toupper(is_generic)) %>%
  select(-bnf_name.x) %>%
  rename(bnf_name = bnf_name.y) %>%
  # Filter codes which are flagged to not use
  filter(!grepl("*DO NOT USE*", bnf_name)) %>%
  arrange(bnf_name)

#### Save and clear up workspace ####
# Save DF as .csv
write_csv(BNF_codes_measures, file = "./data/BNF_codes_measures.csv")

# remove all variables except pre-existing variables and "OP_codes"
rm(list=setdiff(ls(), c(existing_vars, "BNF_codes_measures")))
