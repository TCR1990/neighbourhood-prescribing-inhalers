#### 01.2_get_NHS_CCG_patient_numbers.R ####
"

Download CCG code and number of patients from the NHS. 
Sort into table which includes ccg_id and ccg columns.

Requires:
  Internet connection
  
Outputs
  ./data-source/NHS_ccg_all_years.csv

NOTES:
  - Final line clears all variables in the workspace except pre-exsiting variables and 'NHS_ccg_all_years'.



Code retreives CCG codes and corresponding number of patients from NHS digital.

2015 July:
Link:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/july-2015
FILE:
Numbers of Patients Registered at a GP Practice - July 2015: CCG


2016 April:
Link:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2016
FILE:
Numbers of Patients Registered at a GP Practice - April 2016: CCG


2017 April:
Link:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2017
File:
Patients Registered at a GP Practice - April 2017: 5 year age groups (Commissioning Regions-Regions-CCGs-GP practice)


2018 April:
Link:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/patients-registered-at-a-gp-practice-april-2018-special-topic---registered-patients-compared-to-the-projected-resident-population-in-england
File:
Patients Registered at a GP Practice - April 2018: 5 year age groups (Commissioning Regions-Regions-STPs-CCGs-GP practice)


***NEW SECTION***
2019 June:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/june-2019
File:
Patients Registered at a GP Practice – June 2019: 5 year age groups (Commissioning Regions-Regions-STPs-CCGs-GP practice)
***NEW SECTION***


April 2020:
Link:
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2020
File:
Patients Registered at a GP Practice – April 2020: 5-year age groups (Commissioning Regions-STPs-CCGs-PCNs-GP practice)


"
#### Import libraries ####
library(tidyverse)

#### Workspace setup ####
# log existing variables
existing_vars = ls()

# get directory of current script and set it as working directory
filedir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(filedir)
getwd()

# create data folder if not yet existent
if (!dir.exists("./data")) {
  dir.create("./data")
}

# create source data folder if not yet existent
if (!dir.exists("./data-source")) {
  dir.create("./data-source")
}


# Create a dataframe which includes URLs and dates
ccg_codes_df <- data.frame(
  date = c("2015-07",
           "2016-04",
           "2017-04",
           "2018-04", 
           "2019-04",
           "2020-04"),
  url = c(
    # 2015 
    "https://files.digital.nhs.uk/publicationimport/pub17xxx/pub17927/ccg-reg-patients.csv",
    
    # 2016
    "https://files.digital.nhs.uk/publicationimport/pub20xxx/pub20480/ccg-reg-patients.csv",
    
    # 2017
    "https://files.digital.nhs.uk/excel/s/1/gp-reg-pat-prac-quin-age.csv",
    
    # 2018
    "https://files.digital.nhs.uk/A5/ACF934/gp-reg-pat-prac-quin-age.csv",
    
    # 2019
    "https://files.digital.nhs.uk/D0/F67AE6/gp-reg-pat-prac-quin-age.csv",
    
    # 2020
    "https://files.digital.nhs.uk/98/764116/gp-reg-pat-prac-quin-age.csv"))


# Desired columns. 
# LHS = colnames in datafiles for 2015 and 2016
# RHS = colnames in  datafiles for 2017-2020
cols_ccg_codes <- c("ONS_CCG_CODE", "ONS_CODE")
cols_ccg_ids <- c("CCG_CODE", "ORG_CODE")
cols_n_pats <- c("TOTAL_ALL", "ALL_ALL")

# Loop through each link collecting data and appending to NHS_ccg_all dataframe
for (row_n in 1:dim(ccg_codes_df)[1]){
  
  # Read in data for current year
  NHS_ccg_current <- read.csv(url(ccg_codes_df$url[row_n]))
  
  # Years for 2015 and 2016 are in simpler format so do not require this section
  if(row_n > 2){
    NHS_ccg_current <- NHS_ccg_current %>% 
      filter(ORG_TYPE == "CCG") %>%
      pivot_wider(id_cols=c(ORG_CODE, ONS_CODE),
                  names_from=c(SEX, AGE_GROUP_5),
                  values_from = NUMBER_OF_PATIENTS)
  }
  
  # Rename the columns and add the date
  NHS_ccg_current <- NHS_ccg_current %>%
    select(colnames(NHS_ccg_current)[colnames(NHS_ccg_current) %in% 
                                       c(cols_ccg_codes, cols_ccg_ids, cols_n_pats)]) %>%
    mutate(date = ccg_codes_df$date[row_n]) %>%
    rename(ccg_code = any_of(cols_ccg_codes),
           ccg_id = any_of(cols_ccg_ids),
           number_of_patients = any_of(cols_n_pats))
  
  # Join to the main df (or create if first)
  if (exists("NHS_ccg_all_years")) {
    NHS_ccg_all_years <- rbind(NHS_ccg_all_years, NHS_ccg_current)        
  } else {
    NHS_ccg_all_years <- NHS_ccg_current
  } 
  
}

# This is now saved later after joining with ccg codes
file_dir <- "./data-source/NHS_ccg_all_years.csv"
write_csv(NHS_ccg_all_years, file = file_dir)

# Remove all variables except pre-existing variables and "NHS_ccg_all_years"
rm(list=setdiff(ls(), c(existing_vars, "NHS_ccg_all_years")))
