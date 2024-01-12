#### 01.1_get_ONS_CCG_shapes.R ####
"

Download CCG data from the ONS. 
Sort into table which includes ONS code and ccg name columns.

Requires:
  Internet connection
  
Outputs
  ./data-source/ONS_ccg_all_years.Rdata

NOTES:
- Final line clears all variables in the workspace except pre-exsiting variables and 'ONS_ccg_all_years'.

In 2019-2020, CCG changed to STP:
https://digital.nhs.uk/services/organisation-data-service/archive/change-summary-2020-stp-reconfiguration

Data are sourced from the ONS:
https://geoportal.statistics.gov.uk/search?q=Clinical%20Commissioning%20Groups&sort=name

2015: 
https://geoportal.statistics.gov.uk/datasets/ccg-july-2015-ultra-generalised-clipped-boundaries-in-england/

2016:
https://geoportal.statistics.gov.uk/datasets/ccg-apr-2016-ultra-generalised-clipped-boundaries-in-england/

2017:
https://geoportal.statistics.gov.uk/datasets/ccg-apr-2017-ultra-generalised-clipped-boundaries-in-england-v4/

2018:
https://geoportal.statistics.gov.uk/datasets/ccg-apr-2018-ultra-generalised-clipped-boundaries-in-england/

2019:
https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2019-boundaries-en-buc/explore

2020:
https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-ultra-generalised-boundaries-en

Details for a specific CCG code can be checked on Open Prescribing:
https://openprescribing.net/api/1.0/org_code/?exact=true&q=70F


Downloading files / retreiving JSON data

Data for boundaries are retreived directly from the API as JSON data.
The corresponding JSON for each link above can be found manually by opening the link then:

1. On the left panel, click 'View full details'
2. On the right panel, expand 'View API Resources' tab and click 'Open in API Explorer'
3. Text copied from the 'Query URL' text box

They are for the lowest resolution available (ultrageneralised) to save time spent in unnecessary details in  downloading, loading, and rendering.


"
#### Import libraries ####
library(sf)
library(tidyverse) 

# library(tidyverse)
# library(DBI)
# library(httr)
# library(jsonlite)

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
boundaries_df <- data.frame(
  date = c("2015-07","2016-04","2017-04","2018-04", "2019-04", "2020-04"),
  url = c(
    
    #2015
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CCG_July_2015_UGCB_in_England_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    
    #2016    
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CCG_Apr_2016_UGCB_in_England_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    
    #2017    
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CCG_Apr_2017_UGCB_in_England_V4_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    
    #2018
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CCG_Apr_2018_UGCB_in_England_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    
    #2019
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BUC_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    
    #2020
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2020_Ultra_Generalised_Boundaries_EN_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"))


# Loop through calling the API, downloading data, sorting, and adding to dataframe to output
for (row_n in 1:dim(boundaries_df)[1]){

  #Read the Json file
  shp_data_all <- read_sf(boundaries_df$url[row_n])
  
  # Identify columns in target file
  cols_ccg_info <- c(grep("ccg..cd", colnames(shp_data_all), ignore.case=TRUE), 
                     grep("ccg..nm", colnames(shp_data_all), ignore.case=TRUE))
  
  # Choose columns for CCG name, CCG code, and geometry
  shp_data <- shp_data_all %>% select(all_of(cols_ccg_info), "geometry")
  
  # Rename for output
  colnames(shp_data)[grep("ccg..cd", colnames(shp_data), ignore.case=TRUE)] <- "ccg_code"
  colnames(shp_data)[grep("ccg..nm", colnames(shp_data), ignore.case=TRUE)] <- "ccg_name"
  colnames(shp_data)[colnames(shp_data) == "geometry"] <- "geom"
  
  # Add date column
  shp_data <- shp_data %>% mutate(date = boundaries_df$date[row_n])
  
  # Set the geometry column
  st_geometry(shp_data) <- "geom"
  
  # Add to output file or create if does not yet exist
  if (exists("ONS_ccg_all_years")) {
    ONS_ccg_all_years <- rbind(ONS_ccg_all_years, shp_data)        
  } else {
    ONS_ccg_all_years <- shp_data
  }
}

# This is now saved later after joining with ccg codes
file <- "./data-source/ONS_ccg_all_years.RData"
save(list = c("ONS_ccg_all_years"), file = file)

# Remove all variables except pre-existing variables and "ONS_ccg_all_years"
rm(list=setdiff(ls(), c(existing_vars, "ONS_ccg_all_years")))
