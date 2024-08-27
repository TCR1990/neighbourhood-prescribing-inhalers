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

# Respiratory measures from Open Prescribing: https://openprescribing.net/measure/?tags=respiratory

# https://openprescribing.net/measure/environmental_inhalers/
mdi <- read.csv("./data-provided/OP_measures/environmental_inhalers.csv")$MDI
mdi <- mdi[!mdi == ""] # Numerator
mdi_dpi <- read.csv("./data-provided/OP_measures/environmental_inhalers.csv")$MDI_DPI # Denominator

# https://openprescribing.net/measure/icsdose/
high_dose_ics <- read.csv("./data-provided/OP_measures/icsdose.csv")$high_dose_ics
high_dose_ics <- high_dose_ics[!high_dose_ics == ""] # Numerator
all_ics <- read.csv("./data-provided/OP_measures/icsdose.csv")$all_ics # Denominator

# https://openprescribing.net/measure/saba/
saba <- read.csv("./data-provided/OP_measures/saba.csv")$saba
saba <- saba[!saba == ""] # Numerator
saba_ics <- read.csv("./data-provided/OP_measures/saba.csv")$saba_ics # Denominator

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