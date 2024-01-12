#### 00.2_get_NHSBSA_codes.R ####
"

Downloads files from the NHSBSA respiratory dashboard that provide BNF drug codes 
used in inhaler measures.

NHS respiratory dashboard:
https://www.nhsbsa.nhs.uk/epact2/dashboards-and-specifications/respiratory-dashboard

Target file (click 'Appendix 2 (Excel: 91KB)':
https://www.nhsbsa.nhs.uk/sites/default/files/2019-05/Respiratory%20Dashboard%20-%20Appendix%202.xlsx

Downloads and stores source csv files, imports and proccesses data, then stores ouput in new .csv file NHSBSA_codes.csv.

Leaves 'NHSBSA_codes' dataframe in the workspace.

NOTES:
- Final line clears all variables in the workspace except pre-exsiting variables and 'NHSBSA_codes'.
- BNF code lists not used in current analysis are NOT saved in ouput files.

"
#### Import libraries ####
library(dplyr)
library(tidyverse) 
library(readxl)
library(mgsub)

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
check.bnf.codes <- function(datatocheck){
  # count and print number of TRUE/FALSE in each list. Useful for checks.
  for (j in 3:length(datatocheck)){
    cat("\n",
        colnames(datatocheck)[j], 
        "\n    True:  ",
        sum(datatocheck[j] == TRUE),
        "\n    False: ",
        sum(datatocheck[j] == FALSE),
        "\n    Total: ",
        (sum(datatocheck[j] == TRUE) + sum(datatocheck[j] == FALSE)), 
        fill = TRUE,
        sep="")
  }  
}


#### Download source data file from dashboard ####
url <- "https://www.nhsbsa.nhs.uk/sites/default/files/2019-05/Respiratory%20Dashboard%20-%20Appendix%202.xlsx"
file <- "./data-source/Respiratory Dashboard - Appendix 2.xlsx"
download.file(url, file, mode = "wb")

#### Loop through worksheets in downloaded extracting and sorting relevant data ####
for (ws_n in 1:length(excel_sheets(file))){
  
  # Extract column names from current worksheet
  column_names <- names(read_xlsx(file, 
                                     sheet = ws_n, 
                                     col_names= TRUE, 
                                     skip = 4))
  
  # Remove junk columns (those that contain "...*")
  column_names <- column_names[!grepl("*\\.\\.\\.*", column_names, ignore.case = TRUE)]
  
  # Extract data from current worksheet
  worksheet_data <- read_xlsx(file, 
                         sheet = ws_n, 
                         col_names= TRUE, 
                         col_types = "text",
                         skip = 5)
  
  # Remove columns that only contain NA
  worksheet_data <- worksheet_data[, colSums(is.na(worksheet_data)) != nrow(worksheet_data)]
  
  # Extract BNF codes and names for each code list and name columns
  for (i in 1:length(column_names)*2:2){

    # Extract BNF code and BNF names for first list
    current_df <- worksheet_data[c(i,i-1)]
    
    # Drop NA
    current_df <- drop_na(current_df)
    
    # Add column to indicate inclusion in list
    current_df$new_col <- TRUE
    
    # Tidy up column name to describe current code list
    colnames(current_df) <- c("bnf_code", "bnf_name", tolower(mgsub(column_names[i/2],pattern=c(" ","/"),replacement=c("_","_"))))
    
    # Add to main dataframe (or create if none existent)
    if (exists("NHSBSA_codes")){
      NHSBSA_codes <- full_join(current_df, NHSBSA_codes)
    }else{
      NHSBSA_codes <- current_df
    }
  }
}

# Replace NA with 'FALSE' to indicate that drug not included in list
NHSBSA_codes[is.na(NHSBSA_codes)] <- FALSE

# TODO: Remove checks to clean up code?
# Print summary of number of codes per list for check
cat("\n\n\n\nBNF CODES FROM ALL RESPIRATORY MEASURES \n")
check.bnf.codes(NHSBSA_codes)

# Lists to be used in current analysis
used_lists <-
  c("high_dose_ics_products",  # icsdose
    "all_ics_products",        # icsdose
    "laba_products",           # laba_ics
    "saba_inhaler_products"    # saba
  )

#### Remove lists not required for current analysis ####
NHSBSA_codes <- NHSBSA_codes %>% 
  select(bnf_code, bnf_name, all_of(used_lists)) %>%
  # Remove codes not in any list
  filter_all(any_vars(. == TRUE))

# TODO: Remove checks to clean up code?
# To check against download or previous table
cat("\n\n\n\nBNF CODES FROM SELECTED MEASURES \n")
check.bnf.codes(NHSBSA_codes)

#### Save and clear up workspace ####
# Save DF as .csv
write_csv(NHSBSA_codes, file = './data-source/NHSBSA_codes.csv')

# remove all variables except pre-existing variables and "OP_codes"
rm(list=setdiff(ls(), c(existing_vars, "NHSBSA_codes")))