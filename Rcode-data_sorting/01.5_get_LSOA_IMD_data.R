#### 01.5_get_LSOA_IMD_data.R ####
"

Download IMD data. Sort table for output which includes columns for:
lsoa_id, IMD_decile_rank, IMD_rank, IMD_score, and IMD_P_rank.

Requires:
  Internet connection
  
Outputs
  ./data-source/OP_patients_all_years.csv

NOTES:

Data from:

https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices

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
if (!dir.exists("./data-source/IMD")) {
  dir.create("./data-source/IMD")
}

# Download and save data
url <- "https://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices"
IMD <- read.csv(url(url))

# Sort data ready for use
IMD_all <- IMD %>%
  filter(Indices.of.Deprivation == "a. Index of Multiple Deprivation (IMD)") %>%
  pivot_wider(id_cols = FeatureCode, values_from = Value, names_from = Measurement) %>%
  rename(lsoa_id = FeatureCode,
         IMD_rank = Rank,
         IMD_score = Score,
         IMD_decile_rank = "Decile ") %>%
  mutate(IMD_P_rank = ntile(IMD_score, 100)) # calculate percentile rank

# Save the source data
write_csv(IMD, file = "./data-source/IMD/imd2019lsoa.csv")

# Save the sorted data
write_csv(IMD_all, file = "./data/IMD_all.csv")

# Remove all variables except pre-existing variables and "IMD_all"
rm(list=setdiff(ls(), c(existing_vars, "IMD_all")))