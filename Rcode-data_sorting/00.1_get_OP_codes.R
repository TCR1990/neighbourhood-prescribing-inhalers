#### 00.1_get_OP_codes.R####
"

Extracts text from web pages on OpenPrescribing.net that provide BNF drug codes 
used in inhaler measures.

Downloads and proccesses data, then saves in .csv file OP_codes.csv.

Leaves 'OP_codes' dataframe in the workspace.

NOTE: final line clears all variables in the workspace except pre-exsiting variables and 'OP_codes'.

"
#### Import libraries ####
library(htm2txt)
library(stringr)
library(dplyr)
library(tidyverse) 

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

#### Function definitions ####
scrape_OP_for_codes <- function(url){
## Function for screen scraping Open Prescribing URL and extracting BNF codes
  
  ## Scrape text from the URL
  text <- gettxt(url, encoding = "bytes")
  
  # Coerce to UTF-8 coding so str_match() function can read
  Encoding(text) <- "UTF-8"
  
  ## Extract Numerator
  NumeratorStr <- str_match(text, "Numerator SQL\\s*(.*?)\\s*GROUP BY month")[[1]]
  NumeratorStr2 <- str_match(NumeratorStr, "IN \\(\\s*(.*?)\\s*\\) GROUP BY month")[[2]]
  NumeratorStr3 <- gsub('\\"', "", NumeratorStr2)
  NumeratorList <- as.list(strsplit(NumeratorStr3, ', ')[1])[[1]]
  
  ## Extract Denominator
  DenominatorStr <- str_match(text, "Denominator SQL\\s*(.*?)\\s*GROUP BY month")[[1]]
  DenominatorStr2 <- str_match(DenominatorStr, "IN \\(\\s*(.*?)\\s*\\) GROUP BY month")[[2]]
  DenominatorStr3 <- gsub('\\"', "", DenominatorStr2)
  DenominatorList <- as.list(strsplit(DenominatorStr3, ', ')[1])[[1]]
  
  ## Return list of lists 
  return(list(NumeratorList, DenominatorList))
}

#### Screen scraping from Open Prescribing website to get inhaler codes
# Open Prescribing measures references
# https://openprescribing.net/measure/environmental_inhalers/
# https://openprescribing.net/measure/icsdose/
# https://openprescribing.net/measure/saba/


# Pages containing BNF codes for the measures linked above (click "View technical details for this measure.")
env_url <- 'https://openprescribing.net/measure/environmental_inhalers/definition/'
icsdose_url <- 'https://openprescribing.net/measure/icsdose/definition/'
saba_url <- 'https://openprescribing.net/measure/saba/definition/'

# environmental_inhalers
env_lists <- scrape_OP_for_codes(env_url) # Call screen scraping function
mdi <- env_lists[[1]] # Numerator
mdi_dpi <- env_lists[[2]] # Denominator

# icsdose
ics_lists <- scrape_OP_for_codes(icsdose_url) # Call screen scraping function
high_dose_ics <- ics_lists[[1]] # Numerator
all_ics <- ics_lists[[2]] # Denominator

# saba
saba_lists <- scrape_OP_for_codes(saba_url) # Call screen scraping function
saba <- saba_lists[[1]] # Numerator
saba_ics <- saba_lists[[2]] # Denominator


#### Sort and create dataframe ####
# Remove duplicates, compile into single dataframe, add column labels
OP_codes <- data.frame(bnf_code = unique(c(mdi_dpi, all_ics, saba_ics))) %>%
  mutate(mdi = ifelse(bnf_code %in% mdi, TRUE, FALSE),
         mdi_dpi = ifelse(bnf_code %in% mdi_dpi, TRUE, FALSE),
         high_dose_ics_products = ifelse(bnf_code %in% high_dose_ics, TRUE, FALSE),
         all_ics_products = ifelse(bnf_code %in% all_ics, TRUE, FALSE),
         saba_inhaler_products = ifelse(bnf_code %in% saba, TRUE, FALSE),
         all_saba_ics_products = ifelse(bnf_code %in% saba_ics, TRUE, FALSE))


#### Save and clear up workspace ####
# Save DF as .csv
write_csv(OP_codes, file = './data-source/OP_codes.csv')

# remove all variables except pre-existing variables and "OP_codes"
rm(list=setdiff(ls(), c(existing_vars, "OP_codes")))