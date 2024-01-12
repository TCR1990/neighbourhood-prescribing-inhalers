#### 01.3_get_QOF_data.R ####
"

Download QOF data for total list size and number of asthma and COPD patients 
registered at a GP practice level from NHS Digital. 

Requires:
  Internet connection
  
Outputs
  ./data/QOF_all.R

NOTES:

All data from: 
https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data

https://files.digital.nhs.uk/publicationimport/pub22xxx/pub22266/qof-1516-prev-ach-exc-resp-prac-v2.xlsx
https://files.digital.nhs.uk/publication/c/m/qof-1617-prev-ach-exc-resp-prac.xlsx
https://files.digital.nhs.uk/F1/84569E/qof-1718-prev-ach-exc-resp-prac.xlsx
https://files.digital.nhs.uk/C0/C44CBA/qof-1819-prev-ach-exc-resp-prac.xlsx
https://files.digital.nhs.uk/8B/A9F537/qof-1920-prev-ach-pca-resp-prac.xlsx
https://files.digital.nhs.uk/BA/81F63C/qof-2021-prev-prac-v2.xlsx


To find it go to the page for each year (for example)
https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/2020-21

Find the link that says says prevalance at GP practice level, for all except last it will also mention respiratory. 

"

#### Import libraries ####
library(tidyverse)
library(RCurl)
library(readxl)

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

# create data folder if not yet existent
if (!dir.exists("./data-source/QOF")) {
  dir.create("./data-source/QOF")
}

start_time <- Sys.time()

#### Function definitions ####
big_download <- function(url, file){
  # function to deal with big downloads, showing progress bar
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}


find.n.data.rows <- function(workbook_path, worksheet, col_title_row_n){
  # Find the number of rows containing data following column titles
  range_str <- paste0('A', col_title_row_n, ':A50000')
  
  first_col <- read_excel(workbook_path, sheet = worksheet, range = range_str, col_names = TRUE)
  
  # Returns once encounters empty row
  for(row_n in 1:dim(first_col)[1]){
    if(is.na(first_col[row_n,])){return(row_n)}
  }
}


extract.data <- function(workbook_path, worksheet, col_title_row_n){
  # Extract data from a downloaded QOF file
  
  # 1. Count number of data rows beneath column titles (until NA)
  n_data_rows <- find.n.data.rows(workbook_path, worksheet, col_title_row_n)
  
  # 2. Count number of data columns (first match of both 'List Size' and 'Register') 
  # Get all column titles
  all_col_names <- names(read_xlsx(workbook_path, sheet = worksheet, col_names = TRUE, 
                                   skip = col_title_row_n-1, n_max = 0, .name_repair = "unique"))
  
  # Find last data column number
  last_column_no <- max(
    grep("*List.Size*", all_col_names, ignore.case = TRUE)[1],
    grep("*Register*", all_col_names, ignore.case = TRUE)[1])

  
  # 3. Specify range to import in format "R1C2:R2C5"
  range_str <- paste0('R', col_title_row_n, 'C1:R', (col_title_row_n+n_data_rows-1), 'C', last_column_no)
  
  # 4. Import data 
  imported_data <- read_excel(workbook_path, sheet = worksheet, col_names= TRUE, range = range_str, col_types = "text")
  
  # 5. Remove redundant data
  imported_data <- imported_data %>%
    select(grep("*CCG.code*|*CCG.ODS.code*|*CCG.name*|*Practice.code*|*Practice.name*|*List.Size*|*Register*", 
                names(imported_data), ignore.case = TRUE))
  
  # 6. Rename columns for output
  # Column names desired for output
  output_col_names <- c("ccg_id", "ccg_name", "practice_id", "practice_name", "patients_registered")
  
  if(worksheet == "AST"){
    output_col_names <- c(output_col_names, "asthma")
  }else{
    output_col_names <- c(output_col_names, "COPD")
  }
  
  # Rename columns
  names(imported_data)[grep("*CCG.code*|*CCG.ODS.code*|*CCG.name*|*Practice.code*|*Practice.name*|*List.Size*|*Register*", 
                            names(imported_data), ignore.case = TRUE)] <- output_col_names
  
  # 7. Add QOF_date and year
  imported_data <- imported_data %>%
    mutate(QOF_date = QOF_df$date[row_n],
           year = paste("20", word(QOF_date, 2, sep = "-"), sep=""))
  
  return(imported_data)
}


# Publications also feature the previous year... This is the one we take so file names may APPEAR out of sync
QOF_df <- data.frame(
  date = c("2014-15", "2015-16","2016-17","2017-18","2018-19","2019-20"),
  url = c(
    "https://files.digital.nhs.uk/publicationimport/pub22xxx/pub22266/qof-1516-prev-ach-exc-resp-prac-v2.xlsx",
    "https://files.digital.nhs.uk/publication/c/m/qof-1617-prev-ach-exc-resp-prac.xlsx",
    "https://files.digital.nhs.uk/F1/84569E/qof-1718-prev-ach-exc-resp-prac.xlsx",
    "https://files.digital.nhs.uk/C0/C44CBA/qof-1819-prev-ach-exc-resp-prac.xlsx",
    "https://files.digital.nhs.uk/8B/A9F537/qof-1920-prev-ach-pca-resp-prac.xlsx",
    "https://files.digital.nhs.uk/BA/81F63C/qof-2021-prev-prac-v2.xlsx"),
  file_name = c(
    "qof-1516-prev-ach-exc-resp-prac.xlsx",
    "qof-1617-prev-ach-exc-resp-prac.xlsx",
    "qof-1718-prev-ach-exc-resp-prac.xlsx",
    "qof-1819-prev-ach-exc-resp-prac.xlsx",
    "qof-1920-prev-ach-pca-resp-prac.xlsx",
    "qof-2021-prev-prac-v2.xlsx"),
  col_title_row = c(10, 10, 10, 14, 14, 14))

# Specify an output folder
folder <- "./data-source/QOF/"

# Loop through downloading files, extracting data from worksheets, and sorting
for (row_n in 1:dim(QOF_df)[1]){
  
  # Download data from the current QOF review
  file <- paste(QOF_df$file_name[row_n])
  url <- QOF_df$url[row_n]
  path_file <- paste(folder, file, sep="")
  big_download(url, path_file)
  
  # Data tables start on different rows in downloads from different years
  col_title_row = QOF_df$col_title_row[row_n]

  # Collect asthma data from "AST" sheet
  asthma_sheet <- extract.data(path_file, "AST", col_title_row)
  
  # Collect asthma data from "COPD" sheet
  COPD_sheet <- extract.data(path_file, "COPD", col_title_row)
  
  # Merge into same table
  QOF_data <- merge(asthma_sheet, COPD_sheet)
  
  # Join to the main df (or create if first)
  if (exists("QOF_all")) {
    QOF_all <- rbind(QOF_data, QOF_all)
  } else {
    QOF_all <- QOF_data
  }
}

# Some final tidying up and sorting
QOF_all <- QOF_all %>%
  mutate(year = as.character(year),
         patients_registered = as.numeric(patients_registered),
         asthma = as.numeric(asthma),
         COPD = as.numeric(COPD)) %>%
  select(ccg_id, ccg_name, practice_id, practice_name, patients_registered, QOF_date, asthma, COPD, year) %>%
  arrange(year)

# Save the output
write_csv(QOF_all, file = "./data/QOF_all.csv")

# Remove all variables except pre-existing variables and "QOF_all"
rm(list=setdiff(ls(), c(existing_vars, "QOF_all")))