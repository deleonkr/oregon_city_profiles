library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Population by Age by Town
# Created by Kristine de Leon
# On 01/25/2023
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_pop <- dir(path_to_raw, recursive=T, pattern = "age.csv") 

pop_df <- read.csv(paste0(path_to_raw, "/", raw_pop), stringsAsFactors = F, header = T, check.names = F)

#Merge in FIPS
#town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
#town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
town_fips <- read.csv(file.path(getOption("common_path"), "Geography", "town_fips.csv"))
fips <- (town_fips_dp$data[[1]])

pop_df$FIPS <- gsub("^", "0", pop_df$FIPS)

pop_fips <- merge(pop_df, fips, by = "FIPS", all=T)

#Clean up race/ethnicity column
pop_fips$`Race/Ethnicity` <- gsub(" Black", " - Black", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" Native Hawaiian", " - Native Hawaiian", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" Asian", " - Asian", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" Two or More", " - Two or More", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" American Indian", " - American Indian", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" White Alone", " - White Alone", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub(" Some Other", " - Some Other", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub("4 Hispanic", "4 - Hispanic", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub("7 Hispanic", "7 - Hispanic", pop_fips$`Race/Ethnicity`)
pop_fips$`Race/Ethnicity` <- gsub("9 Hispanic", "9 - Hispanic", pop_fips$`Race/Ethnicity`)

#convert to wide
pop_fips <- pop_fips %>% 
select(-MOE)

pop_fips_wide <- spread(pop_fips, `Race/Ethnicity`, Pop)


pop_fips_wide <- pop_fips_wide %>% 
  select(
"Year", "Town", 
"0 to 4 - American Indian and Alaska Native Alone", 
"0 to 4 - Asian Alone", 
"0 to 4 - Black or African American Alone", 
"0 to 4 - Hispanic or Latino", 
"0 to 4 - Native Hawaiian and Other Pacific Islander", 
"0 to 4 - Some Other Race Alone", 
"0 to 4 - Two or More Races", 
"0 to 4 - White Alone", 
"0 to 4 - White Alone Not Hispanic or Latino", 
"5 to 9 - American Indian and Alaska Native Alone", 
"5 to 9 - Asian Alone", 
"5 to 9 - Black or African American Alone", 
"5 to 9 - Hispanic or Latino", 
"5 to 9 - Native Hawaiian and Other Pacific Islander",
"5 to 9 - Some Other Race Alone", 
"5 to 9 - Two or More Races", 
"5 to 9 - White Alone", 
"5 to 9 - White Alone Not Hispanic or Latino", 
"10 to 14 - American Indian and Alaska Native Alone", 
"10 to 14 - Asian Alone" , 
"10 to 14 - Black or African American Alone" , 
"10 to 14 - Hispanic or Latino", 
"10 to 14 - Native Hawaiian and Other Pacific Islander", 
"10 to 14 - Some Other Race Alone", 
"10 to 14 - Two or More Races" , 
"10 to 14 - White Alone", 
"10 to 14 - White Alone Not Hispanic or Latino", 
"15 to 17 - American Indian and Alaska Native Alone", 
"15 to 17 - Asian Alone", 
"15 to 17 - Black or African American Alone", 
"15 to 17 - Hispanic or Latino", 
"15 to 17 - Native Hawaiian and Other Pacific Islander", 
"15 to 17 - Some Other Race Alone", 
"15 to 17 - Two or More Races", 
"15 to 17 - White Alone", 
"15 to 17 - White Alone Not Hispanic or Latino", 
"18 and 19 - American Indian and Alaska Native Alone", 
"18 and 19 - Asian Alone", 
"18 and 19 - Black or African American Alone", 
"18 and 19 - Hispanic or Latino" , 
"18 and 19 - Native Hawaiian and Other Pacific Islander", 
"18 and 19 - Some Other Race Alone", 
"18 and 19 - Two or More Races", 
"18 and 19 - White Alone", 
"18 and 19 - White Alone Not Hispanic or Latino") %>% 
  arrange(Town)

pop_fips_wide$Year <- "2017-2021"

pop_fips_wide <- pop_fips_wide[!is.na(pop_fips_wide$Town),]


write.table(
  pop_fips_wide,
  file.path(getwd(), "data", "population_by_age_by_race_2021.csv"),
  sep = ",",
  row.names = F
)