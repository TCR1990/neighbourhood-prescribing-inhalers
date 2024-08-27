#### 01.4_get_LSOA_patient_data.R ####
"

Make year-by-year data frame for number of patients registered per LSOA at a 
GP practice. Sort into usable output.

Requires:
  Internet connection
  
Outputs
  ./data/OP_patients_all_years.csv

NOTES:

Data from NHS Digital:

https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2015
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2016
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2017
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/patients-registered-at-a-gp-practice-april-2018-special-topic---registered-patients-compared-to-the-projected-resident-population-in-england
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2019
https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2020

The files to download are the ones with names similar to:
Patients registered at a GP Practice: LSOA (all persons-male-female)


Data for 2017-2020 are downloaded, unzipped, and unpacked programatically.

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

# create patient data folder if not yet existent
if (!dir.exists("./data-source/OP_patients")) {
  dir.create("./data-source/OP_patients")
}

start_time <- Sys.time()

# Prepare data frame for links corresponding to different years
patient_df <- data.frame(
  date = c("2015", "2016", "2017", "2018", "2019", "2020"),
  url = c(
    "https://files.digital.nhs.uk/publicationimport/pub17xxx/pub17356/gp-reg-patients-lsoa-alt-tall.csv",
    "https://files.digital.nhs.uk/publicationimport/pub20xxx/pub20480/lsoa-alt-format-tall.csv",
    "https://files.digital.nhs.uk/publicationimport/pub23xxx/pub23475/gp-reg-pat-prac-lsoa-all-females-males.zip",
    "https://files.digital.nhs.uk/62/638799/gp-reg-pat-prac-lsoa-all-females-males.zip",
    "https://files.digital.nhs.uk/16/740C9E/gp-reg-pat-prac-lsoa-male-female-apr-19.zip",
    "https://files.digital.nhs.uk/93/714E7D/gp-reg-pat-prac-lsoa-male-female-Apr-20.zip"),
  file_name = c(
    "gp-reg-patients-lsoa-alt-tall.csv",
    "lsoa-alt-format-tall.csv",
    "gp-reg-pat-prac-lsoa-all-females-males.zip",
    "gp-reg-pat-prac-lsoa-all-females-males.zip",
    "gp-reg-pat-prac-lsoa-male-female-apr-19.zip",
    "gp-reg-pat-prac-lsoa-male-female-Apr-20.zip"))

# Column name variations - to be used later for "count" column
cols_n_pats <- c("All.Patients", "NUMBER_OF_PATIENTS", "Number.of.Patients")

# Loop through each year downloading files and adding to output table
for(row_n in 1:dim(patient_df)[1]){

  if(row_n < 3){
    # Read directly as csv from URL
    OP_patients_current <- read.csv(url(patient_df$url[row_n]))
  } else {
    
    # Specify dir and filename params
    zip_file <- patient_df$file_name[row_n]
    url <- patient_df$url[row_n]
    zip_dir <- paste0("./data-source/OP_patients/", patient_df$file_name[row_n])
    out_dir <- strsplit(zip_dir, ".zip")[[1]]
    
    # Download, unzip, then delete
    download.file(url, zip_dir, mode = "wb")
    unzipped_files <- unzip(zip_dir, exdir = out_dir)
    file.remove(zip_dir)
    
    # Identify target file and read csv
    OP_patients_current <- read.csv(unzipped_files[str_detect(unzipped_files, 'all.csv')])
  }
  
  # Rename columns and select relevant data
  OP_patients_current <- OP_patients_current %>%
    rename(practice_id = PRACTICE_CODE,
           lsoa_id = LSOA_CODE,
           count = any_of(cols_n_pats)) %>%
    select(practice_id, lsoa_id, count) %>%
    mutate(year = patient_df$date[row_n])
  
  # Join to the main df (or create if first)
  if (exists("OP_patients_all_years")) {
    OP_patients_all_years <- rbind(OP_patients_current, OP_patients_all_years)
  } else {
    OP_patients_all_years <- OP_patients_current
  } 
}

# Sort data
OP_patients_all_years <- OP_patients_all_years %>%
  arrange(year, practice_id)

# Save the output
write_csv(OP_patients_all_years, file = "./data/OP_patients_all_years.csv")

# Remove all variables except pre-existing variables and "OP_patients_all_years"
rm(list=setdiff(ls(), c(existing_vars, "OP_patients_all_years")))