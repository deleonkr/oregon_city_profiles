library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Population by Age by Town
# Created by Kristine de Leon
# On 01/25/2023
#
##################################################################

#Get state data
geography=geo.make(state=41)
yearlist=c(2011:2021)
span = 5
col.names="pretty" 
key="111def7f87569fd16d4c3b790ee027834ca18c92"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")


state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years << workaround due to 2016 data not bringing in all columns by default
    #grabs different number of columns based on race
    if (race == "All") {
      variable =list()      
      for (k in seq_along(1:49)) {
       number = number=paste0("B01001", tbl, "_", sprintf("%03d",k))
       variable = c(variable, number)
       k=k+1
      }
    } else {
      variable =list()      
      for (k in seq_along(1:31)) {
       number = number=paste0("B01001", tbl, "_", sprintf("%03d",k))
       variable = c(variable, number)
       k=k+1
      }
    }
    variable <- as.character(variable)    
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    Sys.sleep(5)
    total <- data[, 1]
    acs.colnames(total) <- "Number:Total:Total"
    geo <- data@geography
    year <- data@endyear
    year <- paste(year-4, year, sep="-")
    print(paste("Processing: ", year, race))     
    #print(paste("Processing: ", year, race))
    
#acs.lookup(2015, span = 5, dataset = "acs", table.number="B01001", case.sensitive = T)


    if (race == "All") {
        # Total:0 to 4 years, 3,27
        total.0to4 <- acsSum(data, c(3, 27), "Number:Total:0 to 4 years")
        percent.total.0to4 <- divide.acs(total.0to4, total, method = "proportion")
        acs.colnames(percent.total.0to4) <- "Percent:Total:0 to 4 years"
        # Total:5 to 9 years, 4,28
        total.5to9 <- acsSum(data, c(4, 28), "Number:Total:5 to 9 years")
        percent.total.5to9 <- divide.acs(total.5to9, total, method = "proportion")
        acs.colnames(percent.total.5to9) <- "Percent:Total:5 to 9 years"
        # Total:10 to 14 years, 5,29
        total.10to14 <- acsSum(data, c(5, 29), "Number:Total:10 to 14 years")
        percent.total.10to14 <- divide.acs(total.10to14, total, method = "proportion")
        acs.colnames(percent.total.10to14) <- "Percent:Total:10 to 14 years"
        # Total:15 to 19 years, 6:7,30:31
        total.15to19 <- acsSum(data, c(6:7, 30:31), "Number:Total:15 to 19 years")
        percent.total.15to19 <- divide.acs(total.15to19, total, method = "proportion")
        acs.colnames(percent.total.15to19) <- "Percent:Total:15 to 19 years"
        # Total:20 to 24 years, 8:10,32:34
        total.20to24 <- acsSum(data, c(8:10, 32:34), "Number:Total:20 to 24 years")
        percent.total.20to24 <- divide.acs(total.20to24, total, method = "proportion")
        acs.colnames(percent.total.20to24) <- "Percent:Total:20 to 24 years"
        # Total:25 to 29 years, 11,35
        total.25to29 <- acsSum(data, c(11, 35), "Number:Total:25 to 29 years")
        percent.total.25to29 <- divide.acs(total.25to29, total, method = "proportion")
        acs.colnames(percent.total.25to29) <- "Percent:Total:25 to 29 years"
        # Total:30 to 34 years, 12,36
        total.30to34 <- acsSum(data, c(12, 36), "Number:Total:30 to 34 years")
        percent.total.30to34 <- divide.acs(total.30to34, total, method = "proportion")
        acs.colnames(percent.total.30to34) <- "Percent:Total:30 to 34 years"
        # Total:35 to 44 years, 13:14,37:38
        total.35to44 <- acsSum(data, c(13:14, 37:38), "Number:Total:35 to 44 years")
        percent.total.35to44 <- divide.acs(total.35to44, total, method = "proportion")
        acs.colnames(percent.total.35to44) <- "Percent:Total:35 to 44 years"
        # Total:45 to 54 years, 15:16, 39:40
        total.45to54 <- acsSum(data, c(15:16, 39:40), "Number:Total:45 to 54 years")
        percent.total.45to54 <- divide.acs(total.45to54, total, method = "proportion")
        acs.colnames(percent.total.45to54) <- "Percent:Total:45 to 54 years"
        # Total:55 to 64 years, 17:19,41:43
        total.55to64 <- acsSum(data, c(17:19, 41:43), "Number:Total:55 to 64 years")
        percent.total.55to64 <- divide.acs(total.55to64, total, method = "proportion")
        acs.colnames(percent.total.55to64) <- "Percent:Total:55 to 64 years"
        # Total:65 to 74 years, 20:22, 44:46
        total.65to74 <- acsSum(data, c(20:22, 44:46), "Number:Total:65 to 74 years")
        percent.total.65to74 <- divide.acs(total.65to74, total, method = "proportion")
        acs.colnames(percent.total.65to74) <- "Percent:Total:65 to 74 years"
        # Total:75 to 84 years, 23:24, 47:48
        total.75to84 <- acsSum(data, c(23:24, 47:48), "Number:Total:75 to 84 years")
        percent.total.75to84 <- divide.acs(total.75to84, total, method = "proportion")
        acs.colnames(percent.total.75to84) <- "Percent:Total:75 to 84 years"
        # Total:85 years and over, 25,49
        total.85over <- acsSum(data, c(25, 49), "Number:Total:85 years and over")
        percent.total.85over <- divide.acs(total.85over, total, method = "proportion")
        acs.colnames(percent.total.85over) <- "Percent:Total:85 years and over"

        # Gender = Male
        # Male:Total, 2
        male.total <- data[, 2]
        acs.colnames(total) <- "Number:Male:Total"
        percent.male.total <- divide.acs(male.total, total, method = "proportion")
        acs.colnames(percent.male.total) <- "Percent:Male:Total"
        # male:0 to 4 years, 3
        male.0to4 <- data[, 3]
        acs.colnames(male.0to4) <- "Number:Male:0 to 4 years"
        percent.male.0to4 <- divide.acs(male.0to4, total, method = "proportion")
        acs.colnames(percent.male.0to4) <- "Percent:Male:0 to 4 years"
        # male:5 to 9 years, 4
        male.5to9 <- data[, 4]
        acs.colnames(male.5to9) <- "Number:Male:5 to 9 years"
        percent.male.5to9 <- divide.acs(male.5to9, total, method = "proportion")
        acs.colnames(percent.male.5to9) <- "Percent:Male:5 to 9 years"
        # male:10 to 14 years, 5
        male.10to14 <- data[, 5]
        acs.colnames(male.10to14) <- "Number:Male:10 to 14 years"
        percent.male.10to14 <- divide.acs(male.10to14, total, method = "proportion")
        acs.colnames(percent.male.10to14) <- "Percent:Male:10 to 14 years"
        # male:15 to 19 years, 6:7
        male.15to19 <- acsSum(data, c(6:7), "Number:Male:15 to 19 years")
        percent.male.15to19 <- divide.acs(male.15to19, total, method = "proportion")
        acs.colnames(percent.male.15to19) <- "Percent:Male:15 to 19 years"
        # male:20 to 24 years, 8:10
        male.20to24 <- acsSum(data, c(8:10), "Number:Male:20 to 24 years")
        percent.male.20to24 <- divide.acs(male.20to24, total, method = "proportion")
        acs.colnames(percent.male.20to24) <- "Percent:Male:20 to 24 years"
        # male:25 to 29 years, 11
        male.25to29 <- data[, 11]
        acs.colnames(male.25to29) <- "Number:Male:25 to 29 years"
        percent.male.25to29 <- divide.acs(male.25to29, total, method = "proportion")
        acs.colnames(percent.male.25to29) <- "Percent:Male:25 to 29 years"
        # male:30 to 34 years, 12
        male.30to34 <- data[, 12]
        acs.colnames(male.30to34) <- "Number:Male:30 to 34 years"
        percent.male.30to34 <- divide.acs(male.30to34, total, method = "proportion")
        acs.colnames(percent.male.30to34) <- "Percent:Male:30 to 34 years"
        # male:35 to 44 years, 13:14
        male.35to44 <- acsSum(data, c(13:14), "Number:Male:35 to 44 years")
        percent.male.35to44 <- divide.acs(male.35to44, total, method = "proportion")
        acs.colnames(percent.male.35to44) <- "Percent:Male:35 to 44 years"
        # male:45 to 54 years, 15:16
        male.45to54 <- acsSum(data, c(15:16), "Number:Male:45 to 54 years")
        percent.male.45to54 <- divide.acs(male.45to54, total, method = "proportion")
        acs.colnames(percent.male.45to54) <- "Percent:Male:45 to 54 years"
        # male:55 to 64 years, 17:19
        male.55to64 <- acsSum(data, c(17:19), "Number:Male:55 to 64 years")
        percent.male.55to64 <- divide.acs(male.55to64, total, method = "proportion")
        acs.colnames(percent.male.55to64) <- "Percent:Male:55 to 64 years"
        # male:65 to 74 years, 20:22
        male.65to74 <- acsSum(data, c(20:22), "Number:Male:65 to 74 years")
        percent.male.65to74 <- divide.acs(male.65to74, total, method = "proportion")
        acs.colnames(percent.male.65to74) <- "Percent:Male:65 to 74 years"
        # male:75 to 84 years, 23:24
        male.75to84 <- acsSum(data, c(23:24), "Number:Male:75 to 84 years")
        percent.male.75to84 <- divide.acs(male.75to84, total, method = "proportion")
        acs.colnames(percent.male.75to84) <- "Percent:Male:75 to 84 years"
        # male:85 years and over, 25
        male.85over <- data[, 25]
        acs.colnames(male.85over) <- "Number:Male:85 years and over"
        percent.male.85over <- divide.acs(male.85over, total, method = "proportion")
        acs.colnames(percent.male.85over) <- "Percent:Male:85 years and over"

        # Gender = Female
        # Female:Total, 26
        female.total <- data[, 26]
        acs.colnames(total) <- "Number:Female:Total"
        percent.female.total <- divide.acs(female.total, total, method = "proportion")
        acs.colnames(percent.female.total) <- "Percent:Female:Total"
        # Female:0 to 4 years, 27
        female.0to4 <- data[, 27]
        acs.colnames(female.0to4) <- "Number:Female:0 to 4 years"
        percent.female.0to4 <- divide.acs(female.0to4, total, method = "proportion")
        acs.colnames(percent.female.0to4) <- "Percent:Female:0 to 4 years"
        # Female:5 to 9 years, 28
        female.5to9 <- data[, 28]
        acs.colnames(female.5to9) <- "Number:Female:5 to 9 years"
        percent.female.5to9 <- divide.acs(female.5to9, total, method = "proportion")
        acs.colnames(percent.female.5to9) <- "Percent:Female:5 to 9 years"
        # Female:10 to 14 years, 29
        female.10to14 <- data[, 29]
        acs.colnames(female.10to14) <- "Number:Female:10 to 14 years"
        percent.female.10to14 <- divide.acs(female.10to14, total, method = "proportion")
        acs.colnames(percent.female.10to14) <- "Percent:Female:10 to 14 years"
        # Female:15 to 19 years, 30:31
        female.15to19 <- acsSum(data, c(30:31), "Number:Female:15 to 19 years")
        percent.female.15to19 <- divide.acs(female.15to19, total, method = "proportion")
        acs.colnames(percent.female.15to19) <- "Percent:Female:15 to 19 years"
        # Female:20 to 24 years, 32:34
        female.20to24 <- acsSum(data, c(32:34), "Number:Female:20 to 24 years")
        percent.female.20to24 <- divide.acs(female.20to24, total, method = "proportion")
        acs.colnames(percent.female.20to24) <- "Percent:Female:20 to 24 years"
        # Female:25 to 29 years, 35
        female.25to29 <- data[, 35]
        acs.colnames(female.25to29) <- "Number:Female:25 to 29 years"
        percent.female.25to29 <- divide.acs(female.25to29, total, method = "proportion")
        acs.colnames(percent.female.25to29) <- "Percent:Female:25 to 29 years"
        # Female:30 to 34 years, 36
        female.30to34 <- data[, 36]
        acs.colnames(female.30to34) <- "Number:Female:30 to 34 years"
        percent.female.30to34 <- divide.acs(female.30to34, total, method = "proportion")
        acs.colnames(percent.female.30to34) <- "Percent:Female:30 to 34 years"
        # Female:35 to 44 years, 37:38
        female.35to44 <- acsSum(data, c(37:38), "Number:Female:35 to 44 years")
        percent.female.35to44 <- divide.acs(female.35to44, total, method = "proportion")
        acs.colnames(percent.female.35to44) <- "Percent:Female:35 to 44 years"
        # Female:45 to 54 years,  39:40
        female.45to54 <- acsSum(data, c(39:40), "Number:Female:45 to 54 years")
        percent.female.45to54 <- divide.acs(female.45to54, total, method = "proportion")
        acs.colnames(percent.female.45to54) <- "Percent:Female:45 to 54 years"
        # Female:55 to 64 years, 41:43
        female.55to64 <- acsSum(data, c(41:43), "Number:Female:55 to 64 years")
        percent.female.55to64 <- divide.acs(female.55to64, total, method = "proportion")
        acs.colnames(percent.female.55to64) <- "Percent:Female:55 to 64 years"
        # Female:65 to 74 years,  44:46
        female.65to74 <- acsSum(data, c(44:46), "Number:Female:65 to 74 years")
        percent.female.65to74 <- divide.acs(female.65to74, total, method = "proportion")
        acs.colnames(percent.female.65to74) <- "Percent:Female:65 to 74 years"
        # Female:75 to 84 years,  47:48
        female.75to84 <- acsSum(data, c(47:48), "Number:Female:75 to 84 years")
        percent.female.75to84 <- divide.acs(female.75to84, total, method = "proportion")
        acs.colnames(percent.female.75to84) <- "Percent:Female:75 to 84 years"
        # Female:85 years and over, 49
        female.85over <- data[, 49]
        acs.colnames(female.85over) <- "Number:Female:85 years and over"
        percent.female.85over <- divide.acs(female.85over, total, method = "proportion")
        acs.colnames(percent.female.85over) <- "Percent:Female:85 years and over"
    } else {
        # Total:0 to 4 years, 3,18
        total.0to4 <- acsSum(data, c(3, 18), "Number:Total:0 to 4 years")
        percent.total.0to4 <- divide.acs(total.0to4, total, method = "proportion")
        acs.colnames(percent.total.0to4) <- "Percent:Total:0 to 4 years"
        # Total:5 to 9 years, 4,19
        total.5to9 <- acsSum(data, c(4, 19), "Number:Total:5 to 9 years")
        percent.total.5to9 <- divide.acs(total.5to9, total, method = "proportion")
        acs.colnames(percent.total.5to9) <- "Percent:Total:5 to 9 years"
        # Total:10 to 14 years, 5,20
        total.10to14 <- acsSum(data, c(5, 20), "Number:Total:10 to 14 years")
        percent.total.10to14 <- divide.acs(total.10to14, total, method = "proportion")
        acs.colnames(percent.total.10to14) <- "Percent:Total:10 to 14 years"
        # Total:15 to 19 years, 6:7,21:22
        total.15to19 <- acsSum(data, c(6:7, 21:22), "Number:Total:15 to 19 years")
        percent.total.15to19 <- divide.acs(total.15to19, total, method = "proportion")
        acs.colnames(percent.total.15to19) <- "Percent:Total:15 to 19 years"
        # Total:20 to 24 years, 8,23
        total.20to24 <- acsSum(data, c(8, 23), "Number:Total:20 to 24 years")
        percent.total.20to24 <- divide.acs(total.20to24, total, method = "proportion")
        acs.colnames(percent.total.20to24) <- "Percent:Total:20 to 24 years"
        # Total:25 to 29 years, 9,24
        total.25to29 <- acsSum(data, c(9, 24), "Number:Total:25 to 29 years")
        percent.total.25to29 <- divide.acs(total.25to29, total, method = "proportion")
        acs.colnames(percent.total.25to29) <- "Percent:Total:25 to 29 years"
        # Total:30 to 34 years, 10,25
        total.30to34 <- acsSum(data, c(10, 25), "Number:Total:30 to 34 years")
        percent.total.30to34 <- divide.acs(total.30to34, total, method = "proportion")
        acs.colnames(percent.total.30to34) <- "Percent:Total:30 to 34 years"
        # Total:35 to 44 years, 11,26
        total.35to44 <- acsSum(data, c(11, 26), "Number:Total:35 to 44 years")
        percent.total.35to44 <- divide.acs(total.35to44, total, method = "proportion")
        acs.colnames(percent.total.35to44) <- "Percent:Total:35 to 44 years"
        # Total:45 to 54 years, 12, 27
        total.45to54 <- acsSum(data, c(12, 27), "Number:Total:45 to 54 years")
        percent.total.45to54 <- divide.acs(total.45to54, total, method = "proportion")
        acs.colnames(percent.total.45to54) <- "Percent:Total:45 to 54 years"
        # Total:55 to 64 years, 13,28
        total.55to64 <- acsSum(data, c(13, 28), "Number:Total:55 to 64 years")
        percent.total.55to64 <- divide.acs(total.55to64, total, method = "proportion")
        acs.colnames(percent.total.55to64) <- "Percent:Total:55 to 64 years"
        # Total:65 to 74 years, 14, 29
        total.65to74 <- acsSum(data, c(14, 29), "Number:Total:65 to 74 years")
        percent.total.65to74 <- divide.acs(total.65to74, total, method = "proportion")
        acs.colnames(percent.total.65to74) <- "Percent:Total:65 to 74 years"
        # Total:75 to 84 years, 15, 30
        total.75to84 <- acsSum(data, c(15, 30), "Number:Total:75 to 84 years")
        percent.total.75to84 <- divide.acs(total.75to84, total, method = "proportion")
        acs.colnames(percent.total.75to84) <- "Percent:Total:75 to 84 years"
        # Total:85 years and over, 16,31
        total.85over <- acsSum(data, c(16, 31), "Number:Total:85 years and over")
        percent.total.85over <- divide.acs(total.85over, total, method = "proportion")
        acs.colnames(percent.total.85over) <- "Percent:Total:85 years and over"

        # Gender = Male
        # Male:Total, 2
        male.total <- data[, 2]
        acs.colnames(total) <- "Number:Male:Total"
        percent.male.total <- divide.acs(male.total, total, method = "proportion")
        acs.colnames(percent.male.total) <- "Percent:Male:Total"
        # male:0 to 4 years, 3
        male.0to4 <- data[, 3]
        acs.colnames(male.0to4) <- "Number:Male:0 to 4 years"
        percent.male.0to4 <- divide.acs(male.0to4, total, method = "proportion")
        acs.colnames(percent.male.0to4) <- "Percent:Male:0 to 4 years"
        # male:5 to 9 years, 4
        male.5to9 <- data[, 4]
        acs.colnames(male.5to9) <- "Number:Male:5 to 9 years"
        percent.male.5to9 <- divide.acs(male.5to9, total, method = "proportion")
        acs.colnames(percent.male.5to9) <- "Percent:Male:5 to 9 years"
        # male:10 to 14 years, 5
        male.10to14 <- data[, 5]
        acs.colnames(male.10to14) <- "Number:Male:10 to 14 years"
        percent.male.10to14 <- divide.acs(male.10to14, total, method = "proportion")
        acs.colnames(percent.male.10to14) <- "Percent:Male:10 to 14 years"
        # male:15 to 19 years, 6:7
        male.15to19 <- acsSum(data, c(6:7), "Number:Male:15 to 19 years")
        percent.male.15to19 <- divide.acs(male.15to19, total, method = "proportion")
        acs.colnames(percent.male.15to19) <- "Percent:Male:15 to 19 years"
        # male:20 to 24 years, 8
        male.20to24 <- data[, 8]
        acs.colnames(male.20to24) <- "Number:Male:20 to 24 years"
        percent.male.20to24 <- divide.acs(male.20to24, total, method = "proportion")
        acs.colnames(percent.male.20to24) <- "Percent:Male:20 to 24 years"
        # male:25 to 29 years, 9
        male.25to29 <- data[, 9]
        acs.colnames(male.25to29) <- "Number:Male:25 to 29 years"
        percent.male.25to29 <- divide.acs(male.25to29, total, method = "proportion")
        acs.colnames(percent.male.25to29) <- "Percent:Male:25 to 29 years"
        # male:30 to 34 years, 10
        male.30to34 <- data[, 10]
        acs.colnames(male.30to34) <- "Number:Male:30 to 34 years"
        percent.male.30to34 <- divide.acs(male.30to34, total, method = "proportion")
        acs.colnames(percent.male.30to34) <- "Percent:Male:30 to 34 years"
        # male:35 to 44 years, 11
        male.35to44 <- data[, 11]
        acs.colnames(male.35to44) <- "Number:Male:35 to 44 years"
        percent.male.35to44 <- divide.acs(male.35to44, total, method = "proportion")
        acs.colnames(percent.male.35to44) <- "Percent:Male:35 to 44 years"
        # male:45 to 54 years, 12
        male.45to54 <- data[, 12]
        acs.colnames(male.45to54) <- "Number:Male:45 to 54 years"
        percent.male.45to54 <- divide.acs(male.45to54, total, method = "proportion")
        acs.colnames(percent.male.45to54) <- "Percent:Male:45 to 54 years"
        # male:55 to 64 years, 13
        male.55to64 <- data[, 13]
        acs.colnames(male.55to64) <- "Number:Male:55 to 64 years"
        percent.male.55to64 <- divide.acs(male.55to64, total, method = "proportion")
        acs.colnames(percent.male.55to64) <- "Percent:Male:55 to 64 years"
        # male:65 to 74 years, 14
        male.65to74 <- data[, 14]
        acs.colnames(male.65to74) <- "Number:Male:65 to 74 years"
        percent.male.65to74 <- divide.acs(male.65to74, total, method = "proportion")
        acs.colnames(percent.male.65to74) <- "Percent:Male:65 to 74 years"
        # male:75 to 84 years, 15
        male.75to84 <- data[, 15]
        acs.colnames(male.75to84) <- "Number:Male:75 to 84 years"
        percent.male.75to84 <- divide.acs(male.75to84, total, method = "proportion")
        acs.colnames(percent.male.75to84) <- "Percent:Male:75 to 84 years"
        # male:85 years and over, 16
        male.85over <- data[, 16]
        acs.colnames(male.85over) <- "Number:Male:85 years and over"
        percent.male.85over <- divide.acs(male.85over, total, method = "proportion")
        acs.colnames(percent.male.85over) <- "Percent:Male:85 years and over"

        # Gender = Female
        # Female:Total, 17
        female.total <- data[, 17]
        acs.colnames(total) <- "Number:Female:Total"
        percent.female.total <- divide.acs(female.total, total, method = "proportion")
        acs.colnames(percent.female.total) <- "Percent:Female:Total"
        # Female:0 to 4 years, 18
        female.0to4 <- data[, 18]
        acs.colnames(female.0to4) <- "Number:Female:0 to 4 years"
        percent.female.0to4 <- divide.acs(female.0to4, total, method = "proportion")
        acs.colnames(percent.female.0to4) <- "Percent:Female:0 to 4 years"
        # Female:5 to 9 years, 19
        female.5to9 <- data[, 19]
        acs.colnames(female.5to9) <- "Number:Female:5 to 9 years"
        percent.female.5to9 <- divide.acs(female.5to9, total, method = "proportion")
        acs.colnames(percent.female.5to9) <- "Percent:Female:5 to 9 years"
        # Female:10 to 14 years, 20
        female.10to14 <- data[, 20]
        acs.colnames(female.10to14) <- "Number:Female:10 to 14 years"
        percent.female.10to14 <- divide.acs(female.10to14, total, method = "proportion")
        acs.colnames(percent.female.10to14) <- "Percent:Female:10 to 14 years"
        # Female:15 to 19 years, 21:22
        female.15to19 <- acsSum(data, c(21:22), "Number:Female:15 to 19 years")
        percent.female.15to19 <- divide.acs(female.15to19, total, method = "proportion")
        acs.colnames(percent.female.15to19) <- "Percent:Female:15 to 19 years"
        # Female:20 to 24 years, 23
        female.20to24 <- data[, 23]
        acs.colnames(female.20to24) <- "Number:Female:20 to 24 years"
        percent.female.20to24 <- divide.acs(female.20to24, total, method = "proportion")
        acs.colnames(percent.female.20to24) <- "Percent:Female:20 to 24 years"
        # Female:25 to 29 years, 24
        female.25to29 <- data[, 24]
        acs.colnames(female.25to29) <- "Number:Female:25 to 29 years"
        percent.female.25to29 <- divide.acs(female.25to29, total, method = "proportion")
        acs.colnames(percent.female.25to29) <- "Percent:Female:25 to 29 years"
        # Female:30 to 34 years, 25
        female.30to34 <- data[, 25]
        acs.colnames(female.30to34) <- "Number:Female:30 to 34 years"
        percent.female.30to34 <- divide.acs(female.30to34, total, method = "proportion")
        acs.colnames(percent.female.30to34) <- "Percent:Female:30 to 34 years"
        # Female:35 to 44 years, 26
        female.35to44 <- data[, 26]
        acs.colnames(female.35to44) <- "Number:Female:35 to 44 years"
        percent.female.35to44 <- divide.acs(female.35to44, total, method = "proportion")
        acs.colnames(percent.female.35to44) <- "Percent:Female:35 to 44 years"
        # Female:45 to 54 years,  27
        female.45to54 <- data[, 27]
        acs.colnames(female.45to54) <- "Number:Female:45 to 54 years"
        percent.female.45to54 <- divide.acs(female.45to54, total, method = "proportion")
        acs.colnames(percent.female.45to54) <- "Percent:Female:45 to 54 years"
        # Female:55 to 64 years, 28
        female.55to64 <- data[, 28]
        acs.colnames(female.55to64) <- "Number:Female:55 to 64 years"
        percent.female.55to64 <- divide.acs(female.55to64, total, method = "proportion")
        acs.colnames(percent.female.55to64) <- "Percent:Female:55 to 64 years"
        # Female:65 to 74 years,  29
        female.65to74 <- data[, 29]
        acs.colnames(female.65to74) <- "Number:Female:65 to 74 years"
        percent.female.65to74 <- divide.acs(female.65to74, total, method = "proportion")
        acs.colnames(percent.female.65to74) <- "Percent:Female:65 to 74 years"
        # Female:75 to 84 years,  30
        female.75to84 <- data[, 30]
        acs.colnames(female.75to84) <- "Number:Female:75 to 84 years"
        percent.female.75to84 <- divide.acs(female.75to84, total, method = "proportion")
        acs.colnames(percent.female.75to84) <- "Percent:Female:75 to 84 years"
        # Female:85 years and over, 31
        female.85over <- data[, 31]
        acs.colnames(female.85over) <- "Number:Female:85 years and over"
        percent.female.85over <- divide.acs(female.85over, total, method = "proportion")
        acs.colnames(percent.female.85over) <- "Percent:Female:85 years and over"
    }
    
# merge in fips
    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
            geo,
            estimate(total),
            estimate(total.0to4),
            estimate(total.5to9),
            estimate(total.10to14),
            estimate(total.15to19),
            estimate(total.20to24),
            estimate(total.25to29),
            estimate(total.30to34),
            estimate(total.35to44),
            estimate(total.45to54),
            estimate(total.55to64),
            estimate(total.65to74),
            estimate(total.75to84),
            estimate(total.85over),
            estimate(male.total),
            estimate(male.0to4),
            estimate(male.5to9),
            estimate(male.10to14),
            estimate(male.15to19),
            estimate(male.20to24),
            estimate(male.25to29),
            estimate(male.30to34),
            estimate(male.35to44),
            estimate(male.45to54),
            estimate(male.55to64),
            estimate(male.65to74),
            estimate(male.75to84),
            estimate(male.85over),
            estimate(female.total),
            estimate(female.0to4),
            estimate(female.5to9),
            estimate(female.10to14),
            estimate(female.15to19),
            estimate(female.20to24),
            estimate(female.25to29),
            estimate(female.30to34),
            estimate(female.35to44),
            estimate(female.45to54),
            estimate(female.55to64),
            estimate(female.65to74),
            estimate(female.75to84),
            estimate(female.85over),
            year,
            race,
            "Number",
            "Population"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(total.0to4) * 1.645,
            standard.error(total.5to9) * 1.645,
            standard.error(total.10to14) * 1.645,
            standard.error(total.15to19) * 1.645,
            standard.error(total.20to24) * 1.645,
            standard.error(total.25to29) * 1.645,
            standard.error(total.30to34) * 1.645,
            standard.error(total.35to44) * 1.645,
            standard.error(total.45to54) * 1.645,
            standard.error(total.55to64) * 1.645,
            standard.error(total.65to74) * 1.645,
            standard.error(total.75to84) * 1.645,
            standard.error(total.85over) * 1.645,
            standard.error(male.total) * 1.645,
            standard.error(male.0to4) * 1.645,
            standard.error(male.5to9) * 1.645,
            standard.error(male.10to14) * 1.645,
            standard.error(male.15to19) * 1.645,
            standard.error(male.20to24) * 1.645,
            standard.error(male.25to29) * 1.645,
            standard.error(male.30to34) * 1.645,
            standard.error(male.35to44) * 1.645,
            standard.error(male.45to54) * 1.645,
            standard.error(male.55to64) * 1.645,
            standard.error(male.65to74) * 1.645,
            standard.error(male.75to84) * 1.645,
            standard.error(male.85over) * 1.645,
            standard.error(female.total) * 1.645,
            standard.error(female.0to4) * 1.645,
            standard.error(female.5to9) * 1.645,
            standard.error(female.10to14) * 1.645,
            standard.error(female.15to19) * 1.645,
            standard.error(female.20to24) * 1.645,
            standard.error(female.25to29) * 1.645,
            standard.error(female.30to34) * 1.645,
            standard.error(female.35to44) * 1.645,
            standard.error(female.45to54) * 1.645,
            standard.error(female.55to64) * 1.645,
            standard.error(female.65to74) * 1.645,
            standard.error(female.75to84) * 1.645,
            standard.error(female.85over) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "Town", "FIPS",
            "Number:Total:Total",
            "Number:Total:0 to 4 years",
            "Number:Total:5 to 9 years",
            "Number:Total:10 to 14 years",
            "Number:Total:15 to 19 years",
            "Number:Total:20 to 24 years",
            "Number:Total:25 to 29 years",
            "Number:Total:30 to 34 years",
            "Number:Total:35 to 44 years",
            "Number:Total:45 to 54 years",
            "Number:Total:55 to 64 years",
            "Number:Total:65 to 74 years",
            "Number:Total:75 to 84 years",
            "Number:Total:85 years and over",
            "Number:Male:Total",
            "Number:Male:0 to 4 years",
            "Number:Male:5 to 9 years",
            "Number:Male:10 to 14 years",
            "Number:Male:15 to 19 years",
            "Number:Male:20 to 24 years",
            "Number:Male:25 to 29 years",
            "Number:Male:30 to 34 years",
            "Number:Male:35 to 44 years",
            "Number:Male:45 to 54 years",
            "Number:Male:55 to 64 years",
            "Number:Male:65 to 74 years",
            "Number:Male:75 to 84 years",
            "Number:Male:85 years and over",
            "Number:Female:Total",
            "Number:Female:0 to 4 years",
            "Number:Female:5 to 9 years",
            "Number:Female:10 to 14 years",
            "Number:Female:15 to 19 years",
            "Number:Female:20 to 24 years",
            "Number:Female:25 to 29 years",
            "Number:Female:30 to 34 years",
            "Number:Female:35 to 44 years",
            "Number:Female:45 to 54 years",
            "Number:Female:55 to 64 years",
            "Number:Female:65 to 74 years",
            "Number:Female:75 to 84 years",
            "Number:Female:85 years and over",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars = c("Town", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name = "Population",
            variable.factor = F,
            value.name = "Value",
            value.factor = F
         )

    percentEstimates <- data.table(
            geo,
            # estimate(percent.total),
            estimate(percent.total.0to4),
            estimate(percent.total.5to9),
            estimate(percent.total.10to14),
            estimate(percent.total.15to19),
            estimate(percent.total.20to24),
            estimate(percent.total.25to29),
            estimate(percent.total.30to34),
            estimate(percent.total.35to44),
            estimate(percent.total.45to54),
            estimate(percent.total.55to64),
            estimate(percent.total.65to74),
            estimate(percent.total.75to84),
            estimate(percent.total.85over),
            estimate(percent.male.total),
            estimate(percent.male.0to4),
            estimate(percent.male.5to9),
            estimate(percent.male.10to14),
            estimate(percent.male.15to19),
            estimate(percent.male.20to24),
            estimate(percent.male.25to29),
            estimate(percent.male.30to34),
            estimate(percent.male.35to44),
            estimate(percent.male.45to54),
            estimate(percent.male.55to64),
            estimate(percent.male.65to74),
            estimate(percent.male.75to84),
            estimate(percent.male.85over),
            estimate(percent.female.total),
            estimate(percent.female.0to4),
            estimate(percent.female.5to9),
            estimate(percent.female.10to14),
            estimate(percent.female.15to19),
            estimate(percent.female.20to24),
            estimate(percent.female.25to29),
            estimate(percent.female.30to34),
            estimate(percent.female.35to44),
            estimate(percent.female.45to54),
            estimate(percent.female.55to64),
            estimate(percent.female.65to74),
            estimate(percent.female.75to84),
            estimate(percent.female.85over),
            year,
            race,
            "percent",
            "Population"
        )
    percentMOES <- data.table(
            geo,
            # standard.error(percent.total) * 1.645,
            standard.error(percent.total.0to4) * 1.645,
            standard.error(percent.total.5to9) * 1.645,
            standard.error(percent.total.10to14) * 1.645,
            standard.error(percent.total.15to19) * 1.645,
            standard.error(percent.total.20to24) * 1.645,
            standard.error(percent.total.25to29) * 1.645,
            standard.error(percent.total.30to34) * 1.645,
            standard.error(percent.total.35to44) * 1.645,
            standard.error(percent.total.45to54) * 1.645,
            standard.error(percent.total.55to64) * 1.645,
            standard.error(percent.total.65to74) * 1.645,
            standard.error(percent.total.75to84) * 1.645,
            standard.error(percent.total.85over) * 1.645,
            standard.error(percent.male.total) * 1.645,
            standard.error(percent.male.0to4) * 1.645,
            standard.error(percent.male.5to9) * 1.645,
            standard.error(percent.male.10to14) * 1.645,
            standard.error(percent.male.15to19) * 1.645,
            standard.error(percent.male.20to24) * 1.645,
            standard.error(percent.male.25to29) * 1.645,
            standard.error(percent.male.30to34) * 1.645,
            standard.error(percent.male.35to44) * 1.645,
            standard.error(percent.male.45to54) * 1.645,
            standard.error(percent.male.55to64) * 1.645,
            standard.error(percent.male.65to74) * 1.645,
            standard.error(percent.male.75to84) * 1.645,
            standard.error(percent.male.85over) * 1.645,
            standard.error(percent.female.total) * 1.645,
            standard.error(percent.female.0to4) * 1.645,
            standard.error(percent.female.5to9) * 1.645,
            standard.error(percent.female.10to14) * 1.645,
            standard.error(percent.female.15to19) * 1.645,
            standard.error(percent.female.20to24) * 1.645,
            standard.error(percent.female.25to29) * 1.645,
            standard.error(percent.female.30to34) * 1.645,
            standard.error(percent.female.35to44) * 1.645,
            standard.error(percent.female.45to54) * 1.645,
            standard.error(percent.female.55to64) * 1.645,
            standard.error(percent.female.65to74) * 1.645,
            standard.error(percent.female.75to84) * 1.645,
            standard.error(percent.female.85over) * 1.645,
            year,
            race,
            "percent",
            "Margins of Error"
        )
    percentNames <- c(
            "Town", "FIPS",
            # "Percent:Total:Total",
            "Percent:Total:0 to 4 years",
            "Percent:Total:5 to 9 years",
            "Percent:Total:10 to 14 years",
            "Percent:Total:15 to 19 years",
            "Percent:Total:20 to 24 years",
            "Percent:Total:25 to 29 years",
            "Percent:Total:30 to 34 years",
            "Percent:Total:35 to 44 years",
            "Percent:Total:45 to 54 years",
            "Percent:Total:55 to 64 years",
            "Percent:Total:65 to 74 years",
            "Percent:Total:75 to 84 years",
            "Percent:Total:85 years and over",
            "Percent:Male:Total",
            "Percent:Male:0 to 4 years",
            "Percent:Male:5 to 9 years",
            "Percent:Male:10 to 14 years",
            "Percent:Male:15 to 19 years",
            "Percent:Male:20 to 24 years",
            "Percent:Male:25 to 29 years",
            "Percent:Male:30 to 34 years",
            "Percent:Male:35 to 44 years",
            "Percent:Male:45 to 54 years",
            "Percent:Male:55 to 64 years",
            "Percent:Male:65 to 74 years",
            "Percent:Male:75 to 84 years",
            "Percent:Male:85 years and over",
            "Percent:Female:Total",
            "Percent:Female:0 to 4 years",
            "Percent:Female:5 to 9 years",
            "Percent:Female:10 to 14 years",
            "Percent:Female:15 to 19 years",
            "Percent:Female:20 to 24 years",
            "Percent:Female:25 to 29 years",
            "Percent:Female:30 to 34 years",
            "Percent:Female:35 to 44 years",
            "Percent:Female:45 to 54 years",
            "Percent:Female:55 to 64 years",
            "Percent:Female:65 to 74 years",
            "Percent:Female:75 to 84 years",
            "Percent:Female:85 years and over",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars = c("Town", "FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name = "Population",
            variable.factor = F,
            value.name = "Value",
            value.factor = F
         )

    inter_data <- rbind(inter_data, numbersData.melt, percentsData.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get town data
geography=geo.make(state=41, county="*", place ="*")

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    #needed to grab all columns for all years
    #grabs different number of columns based on race
    if (race == "All") {
      variable =list()      
      for (k in seq_along(1:49)) {
       number = number=paste0("B01001", tbl, "_", sprintf("%03d",k))
       variable = c(variable, number)
       k=k+1
      }
    } else {
      variable =list()      
      for (k in seq_along(1:31)) {
       number = number=paste0("B01001", tbl, "_", sprintf("%03d",k))
       variable = c(variable, number)
       k=k+1
      }
    }
    variable <- as.character(variable)    
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                      variable = variable, key=key)
    Sys.sleep(5)
    total <- data[, 1]
    acs.colnames(total) <- "Number:Total:Total"
    geo <- data@geography
    geo$county <- sprintf("%02d", geo$county)
    geo$county <- gsub("^", "041", geo$county)
    geo$FIPS <- paste0(geo$county, geo$place)
    geo$state <- NULL
    geo$NAME <- NULL
    geo$place <- NULL
    geo$county <- NULL     
    year <- data@endyear
    year <- paste(year-4, year, sep="-")
    print(paste("Processing: ", year, race))     
        if (race == "All") {
        # Total:0 to 4 years, 3,27
        total.0to4 <- acsSum(data, c(3, 27), "Number:Total:0 to 4 years")
        percent.total.0to4 <- divide.acs(total.0to4, total, method = "proportion")
        acs.colnames(percent.total.0to4) <- "Percent:Total:0 to 4 years"
        # Total:5 to 9 years, 4,28
        total.5to9 <- acsSum(data, c(4, 28), "Number:Total:5 to 9 years")
        percent.total.5to9 <- divide.acs(total.5to9, total, method = "proportion")
        acs.colnames(percent.total.5to9) <- "Percent:Total:5 to 9 years"
        # Total:10 to 14 years, 5,29
        total.10to14 <- acsSum(data, c(5, 29), "Number:Total:10 to 14 years")
        percent.total.10to14 <- divide.acs(total.10to14, total, method = "proportion")
        acs.colnames(percent.total.10to14) <- "Percent:Total:10 to 14 years"
        # Total:15 to 19 years, 6:7,30:31
        total.15to19 <- acsSum(data, c(6:7, 30:31), "Number:Total:15 to 19 years")
        percent.total.15to19 <- divide.acs(total.15to19, total, method = "proportion")
        acs.colnames(percent.total.15to19) <- "Percent:Total:15 to 19 years"
        # Total:20 to 24 years, 8:10,32:34
        total.20to24 <- acsSum(data, c(8:10, 32:34), "Number:Total:20 to 24 years")
        percent.total.20to24 <- divide.acs(total.20to24, total, method = "proportion")
        acs.colnames(percent.total.20to24) <- "Percent:Total:20 to 24 years"
        # Total:25 to 29 years, 11,35
        total.25to29 <- acsSum(data, c(11, 35), "Number:Total:25 to 29 years")
        percent.total.25to29 <- divide.acs(total.25to29, total, method = "proportion")
        acs.colnames(percent.total.25to29) <- "Percent:Total:25 to 29 years"
        # Total:30 to 34 years, 12,36
        total.30to34 <- acsSum(data, c(12, 36), "Number:Total:30 to 34 years")
        percent.total.30to34 <- divide.acs(total.30to34, total, method = "proportion")
        acs.colnames(percent.total.30to34) <- "Percent:Total:30 to 34 years"
        # Total:35 to 44 years, 13:14,37:38
        total.35to44 <- acsSum(data, c(13:14, 37:38), "Number:Total:35 to 44 years")
        percent.total.35to44 <- divide.acs(total.35to44, total, method = "proportion")
        acs.colnames(percent.total.35to44) <- "Percent:Total:35 to 44 years"
        # Total:45 to 54 years, 15:16, 39:40
        total.45to54 <- acsSum(data, c(15:16, 39:40), "Number:Total:45 to 54 years")
        percent.total.45to54 <- divide.acs(total.45to54, total, method = "proportion")
        acs.colnames(percent.total.45to54) <- "Percent:Total:45 to 54 years"
        # Total:55 to 64 years, 17:19,41:43
        total.55to64 <- acsSum(data, c(17:19, 41:43), "Number:Total:55 to 64 years")
        percent.total.55to64 <- divide.acs(total.55to64, total, method = "proportion")
        acs.colnames(percent.total.55to64) <- "Percent:Total:55 to 64 years"
        # Total:65 to 74 years, 20:22, 44:46
        total.65to74 <- acsSum(data, c(20:22, 44:46), "Number:Total:65 to 74 years")
        percent.total.65to74 <- divide.acs(total.65to74, total, method = "proportion")
        acs.colnames(percent.total.65to74) <- "Percent:Total:65 to 74 years"
        # Total:75 to 84 years, 23:24, 47:48
        total.75to84 <- acsSum(data, c(23:24, 47:48), "Number:Total:75 to 84 years")
        percent.total.75to84 <- divide.acs(total.75to84, total, method = "proportion")
        acs.colnames(percent.total.75to84) <- "Percent:Total:75 to 84 years"
        # Total:85 years and over, 25,49
        total.85over <- acsSum(data, c(25, 49), "Number:Total:85 years and over")
        percent.total.85over <- divide.acs(total.85over, total, method = "proportion")
        acs.colnames(percent.total.85over) <- "Percent:Total:85 years and over"

        # Gender = Male
        # Male:Total, 2
        male.total <- data[, 2]
        acs.colnames(total) <- "Number:Male:Total"
        percent.male.total <- divide.acs(male.total, total, method = "proportion")
        acs.colnames(percent.male.total) <- "Percent:Male:Total"
        # male:0 to 4 years, 3
        male.0to4 <- data[, 3]
        acs.colnames(male.0to4) <- "Number:Male:0 to 4 years"
        percent.male.0to4 <- divide.acs(male.0to4, total, method = "proportion")
        acs.colnames(percent.male.0to4) <- "Percent:Male:0 to 4 years"
        # male:5 to 9 years, 4
        male.5to9 <- data[, 4]
        acs.colnames(male.5to9) <- "Number:Male:5 to 9 years"
        percent.male.5to9 <- divide.acs(male.5to9, total, method = "proportion")
        acs.colnames(percent.male.5to9) <- "Percent:Male:5 to 9 years"
        # male:10 to 14 years, 5
        male.10to14 <- data[, 5]
        acs.colnames(male.10to14) <- "Number:Male:10 to 14 years"
        percent.male.10to14 <- divide.acs(male.10to14, total, method = "proportion")
        acs.colnames(percent.male.10to14) <- "Percent:Male:10 to 14 years"
        # male:15 to 19 years, 6:7
        male.15to19 <- acsSum(data, c(6:7), "Number:Male:15 to 19 years")
        percent.male.15to19 <- divide.acs(male.15to19, total, method = "proportion")
        acs.colnames(percent.male.15to19) <- "Percent:Male:15 to 19 years"
        # male:20 to 24 years, 8:10
        male.20to24 <- acsSum(data, c(8:10), "Number:Male:20 to 24 years")
        percent.male.20to24 <- divide.acs(male.20to24, total, method = "proportion")
        acs.colnames(percent.male.20to24) <- "Percent:Male:20 to 24 years"
        # male:25 to 29 years, 11
        male.25to29 <- data[, 11]
        acs.colnames(male.25to29) <- "Number:Male:25 to 29 years"
        percent.male.25to29 <- divide.acs(male.25to29, total, method = "proportion")
        acs.colnames(percent.male.25to29) <- "Percent:Male:25 to 29 years"
        # male:30 to 34 years, 12
        male.30to34 <- data[, 12]
        acs.colnames(male.30to34) <- "Number:Male:30 to 34 years"
        percent.male.30to34 <- divide.acs(male.30to34, total, method = "proportion")
        acs.colnames(percent.male.30to34) <- "Percent:Male:30 to 34 years"
        # male:35 to 44 years, 13:14
        male.35to44 <- acsSum(data, c(13:14), "Number:Male:35 to 44 years")
        percent.male.35to44 <- divide.acs(male.35to44, total, method = "proportion")
        acs.colnames(percent.male.35to44) <- "Percent:Male:35 to 44 years"
        # male:45 to 54 years, 15:16
        male.45to54 <- acsSum(data, c(15:16), "Number:Male:45 to 54 years")
        percent.male.45to54 <- divide.acs(male.45to54, total, method = "proportion")
        acs.colnames(percent.male.45to54) <- "Percent:Male:45 to 54 years"
        # male:55 to 64 years, 17:19
        male.55to64 <- acsSum(data, c(17:19), "Number:Male:55 to 64 years")
        percent.male.55to64 <- divide.acs(male.55to64, total, method = "proportion")
        acs.colnames(percent.male.55to64) <- "Percent:Male:55 to 64 years"
        # male:65 to 74 years, 20:22
        male.65to74 <- acsSum(data, c(20:22), "Number:Male:65 to 74 years")
        percent.male.65to74 <- divide.acs(male.65to74, total, method = "proportion")
        acs.colnames(percent.male.65to74) <- "Percent:Male:65 to 74 years"
        # male:75 to 84 years, 23:24
        male.75to84 <- acsSum(data, c(23:24), "Number:Male:75 to 84 years")
        percent.male.75to84 <- divide.acs(male.75to84, total, method = "proportion")
        acs.colnames(percent.male.75to84) <- "Percent:Male:75 to 84 years"
        # male:85 years and over, 25
        male.85over <- data[, 25]
        acs.colnames(male.85over) <- "Number:Male:85 years and over"
        percent.male.85over <- divide.acs(male.85over, total, method = "proportion")
        acs.colnames(percent.male.85over) <- "Percent:Male:85 years and over"

        # Gender = Female
        # Female:Total, 26
        female.total <- data[, 26]
        acs.colnames(total) <- "Number:Female:Total"
        percent.female.total <- divide.acs(female.total, total, method = "proportion")
        acs.colnames(percent.female.total) <- "Percent:Female:Total"
        # Female:0 to 4 years, 27
        female.0to4 <- data[, 27]
        acs.colnames(female.0to4) <- "Number:Female:0 to 4 years"
        percent.female.0to4 <- divide.acs(female.0to4, total, method = "proportion")
        acs.colnames(percent.female.0to4) <- "Percent:Female:0 to 4 years"
        # Female:5 to 9 years, 28
        female.5to9 <- data[, 28]
        acs.colnames(female.5to9) <- "Number:Female:5 to 9 years"
        percent.female.5to9 <- divide.acs(female.5to9, total, method = "proportion")
        acs.colnames(percent.female.5to9) <- "Percent:Female:5 to 9 years"
        # Female:10 to 14 years, 29
        female.10to14 <- data[, 29]
        acs.colnames(female.10to14) <- "Number:Female:10 to 14 years"
        percent.female.10to14 <- divide.acs(female.10to14, total, method = "proportion")
        acs.colnames(percent.female.10to14) <- "Percent:Female:10 to 14 years"
        # Female:15 to 19 years, 30:31
        female.15to19 <- acsSum(data, c(30:31), "Number:Female:15 to 19 years")
        percent.female.15to19 <- divide.acs(female.15to19, total, method = "proportion")
        acs.colnames(percent.female.15to19) <- "Percent:Female:15 to 19 years"
        # Female:20 to 24 years, 32:34
        female.20to24 <- acsSum(data, c(32:34), "Number:Female:20 to 24 years")
        percent.female.20to24 <- divide.acs(female.20to24, total, method = "proportion")
        acs.colnames(percent.female.20to24) <- "Percent:Female:20 to 24 years"
        # Female:25 to 29 years, 35
        female.25to29 <- data[, 35]
        acs.colnames(female.25to29) <- "Number:Female:25 to 29 years"
        percent.female.25to29 <- divide.acs(female.25to29, total, method = "proportion")
        acs.colnames(percent.female.25to29) <- "Percent:Female:25 to 29 years"
        # Female:30 to 34 years, 36
        female.30to34 <- data[, 36]
        acs.colnames(female.30to34) <- "Number:Female:30 to 34 years"
        percent.female.30to34 <- divide.acs(female.30to34, total, method = "proportion")
        acs.colnames(percent.female.30to34) <- "Percent:Female:30 to 34 years"
        # Female:35 to 44 years, 37:38
        female.35to44 <- acsSum(data, c(37:38), "Number:Female:35 to 44 years")
        percent.female.35to44 <- divide.acs(female.35to44, total, method = "proportion")
        acs.colnames(percent.female.35to44) <- "Percent:Female:35 to 44 years"
        # Female:45 to 54 years,  39:40
        female.45to54 <- acsSum(data, c(39:40), "Number:Female:45 to 54 years")
        percent.female.45to54 <- divide.acs(female.45to54, total, method = "proportion")
        acs.colnames(percent.female.45to54) <- "Percent:Female:45 to 54 years"
        # Female:55 to 64 years, 41:43
        female.55to64 <- acsSum(data, c(41:43), "Number:Female:55 to 64 years")
        percent.female.55to64 <- divide.acs(female.55to64, total, method = "proportion")
        acs.colnames(percent.female.55to64) <- "Percent:Female:55 to 64 years"
        # Female:65 to 74 years,  44:46
        female.65to74 <- acsSum(data, c(44:46), "Number:Female:65 to 74 years")
        percent.female.65to74 <- divide.acs(female.65to74, total, method = "proportion")
        acs.colnames(percent.female.65to74) <- "Percent:Female:65 to 74 years"
        # Female:75 to 84 years,  47:48
        female.75to84 <- acsSum(data, c(47:48), "Number:Female:75 to 84 years")
        percent.female.75to84 <- divide.acs(female.75to84, total, method = "proportion")
        acs.colnames(percent.female.75to84) <- "Percent:Female:75 to 84 years"
        # Female:85 years and over, 49
        female.85over <- data[, 49]
        acs.colnames(female.85over) <- "Number:Female:85 years and over"
        percent.female.85over <- divide.acs(female.85over, total, method = "proportion")
        acs.colnames(percent.female.85over) <- "Percent:Female:85 years and over"
    } else {
        # Total:0 to 4 years, 3,18
        total.0to4 <- acsSum(data, c(3, 18), "Number:Total:0 to 4 years")
        percent.total.0to4 <- divide.acs(total.0to4, total, method = "proportion")
        acs.colnames(percent.total.0to4) <- "Percent:Total:0 to 4 years"
        # Total:5 to 9 years, 4,19
        total.5to9 <- acsSum(data, c(4, 19), "Number:Total:5 to 9 years")
        percent.total.5to9 <- divide.acs(total.5to9, total, method = "proportion")
        acs.colnames(percent.total.5to9) <- "Percent:Total:5 to 9 years"
        # Total:10 to 14 years, 5,20
        total.10to14 <- acsSum(data, c(5, 20), "Number:Total:10 to 14 years")
        percent.total.10to14 <- divide.acs(total.10to14, total, method = "proportion")
        acs.colnames(percent.total.10to14) <- "Percent:Total:10 to 14 years"
        # Total:15 to 19 years, 6:7,21:22
        total.15to19 <- acsSum(data, c(6:7, 21:22), "Number:Total:15 to 19 years")
        percent.total.15to19 <- divide.acs(total.15to19, total, method = "proportion")
        acs.colnames(percent.total.15to19) <- "Percent:Total:15 to 19 years"
        # Total:20 to 24 years, 8,23
        total.20to24 <- acsSum(data, c(8, 23), "Number:Total:20 to 24 years")
        percent.total.20to24 <- divide.acs(total.20to24, total, method = "proportion")
        acs.colnames(percent.total.20to24) <- "Percent:Total:20 to 24 years"
        # Total:25 to 29 years, 9,24
        total.25to29 <- acsSum(data, c(9, 24), "Number:Total:25 to 29 years")
        percent.total.25to29 <- divide.acs(total.25to29, total, method = "proportion")
        acs.colnames(percent.total.25to29) <- "Percent:Total:25 to 29 years"
        # Total:30 to 34 years, 10,25
        total.30to34 <- acsSum(data, c(10, 25), "Number:Total:30 to 34 years")
        percent.total.30to34 <- divide.acs(total.30to34, total, method = "proportion")
        acs.colnames(percent.total.30to34) <- "Percent:Total:30 to 34 years"
        # Total:35 to 44 years, 11,26
        total.35to44 <- acsSum(data, c(11, 26), "Number:Total:35 to 44 years")
        percent.total.35to44 <- divide.acs(total.35to44, total, method = "proportion")
        acs.colnames(percent.total.35to44) <- "Percent:Total:35 to 44 years"
        # Total:45 to 54 years, 12, 27
        total.45to54 <- acsSum(data, c(12, 27), "Number:Total:45 to 54 years")
        percent.total.45to54 <- divide.acs(total.45to54, total, method = "proportion")
        acs.colnames(percent.total.45to54) <- "Percent:Total:45 to 54 years"
        # Total:55 to 64 years, 13,28
        total.55to64 <- acsSum(data, c(13, 28), "Number:Total:55 to 64 years")
        percent.total.55to64 <- divide.acs(total.55to64, total, method = "proportion")
        acs.colnames(percent.total.55to64) <- "Percent:Total:55 to 64 years"
        # Total:65 to 74 years, 14, 29
        total.65to74 <- acsSum(data, c(14, 29), "Number:Total:65 to 74 years")
        percent.total.65to74 <- divide.acs(total.65to74, total, method = "proportion")
        acs.colnames(percent.total.65to74) <- "Percent:Total:65 to 74 years"
        # Total:75 to 84 years, 15, 30
        total.75to84 <- acsSum(data, c(15, 30), "Number:Total:75 to 84 years")
        percent.total.75to84 <- divide.acs(total.75to84, total, method = "proportion")
        acs.colnames(percent.total.75to84) <- "Percent:Total:75 to 84 years"
        # Total:85 years and over, 16,31
        total.85over <- acsSum(data, c(16, 31), "Number:Total:85 years and over")
        percent.total.85over <- divide.acs(total.85over, total, method = "proportion")
        acs.colnames(percent.total.85over) <- "Percent:Total:85 years and over"

        # Gender = Male
        # Male:Total, 2
        male.total <- data[, 2]
        acs.colnames(total) <- "Number:Male:Total"
        percent.male.total <- divide.acs(male.total, total, method = "proportion")
        acs.colnames(percent.male.total) <- "Percent:Male:Total"
        # male:0 to 4 years, 3
        male.0to4 <- data[, 3]
        acs.colnames(male.0to4) <- "Number:Male:0 to 4 years"
        percent.male.0to4 <- divide.acs(male.0to4, total, method = "proportion")
        acs.colnames(percent.male.0to4) <- "Percent:Male:0 to 4 years"
        # male:5 to 9 years, 4
        male.5to9 <- data[, 4]
        acs.colnames(male.5to9) <- "Number:Male:5 to 9 years"
        percent.male.5to9 <- divide.acs(male.5to9, total, method = "proportion")
        acs.colnames(percent.male.5to9) <- "Percent:Male:5 to 9 years"
        # male:10 to 14 years, 5
        male.10to14 <- data[, 5]
        acs.colnames(male.10to14) <- "Number:Male:10 to 14 years"
        percent.male.10to14 <- divide.acs(male.10to14, total, method = "proportion")
        acs.colnames(percent.male.10to14) <- "Percent:Male:10 to 14 years"
        # male:15 to 19 years, 6:7
        male.15to19 <- acsSum(data, c(6:7), "Number:Male:15 to 19 years")
        percent.male.15to19 <- divide.acs(male.15to19, total, method = "proportion")
        acs.colnames(percent.male.15to19) <- "Percent:Male:15 to 19 years"
        # male:20 to 24 years, 8
        male.20to24 <- data[, 8]
        acs.colnames(male.20to24) <- "Number:Male:20 to 24 years"
        percent.male.20to24 <- divide.acs(male.20to24, total, method = "proportion")
        acs.colnames(percent.male.20to24) <- "Percent:Male:20 to 24 years"
        # male:25 to 29 years, 9
        male.25to29 <- data[, 9]
        acs.colnames(male.25to29) <- "Number:Male:25 to 29 years"
        percent.male.25to29 <- divide.acs(male.25to29, total, method = "proportion")
        acs.colnames(percent.male.25to29) <- "Percent:Male:25 to 29 years"
        # male:30 to 34 years, 10
        male.30to34 <- data[, 10]
        acs.colnames(male.30to34) <- "Number:Male:30 to 34 years"
        percent.male.30to34 <- divide.acs(male.30to34, total, method = "proportion")
        acs.colnames(percent.male.30to34) <- "Percent:Male:30 to 34 years"
        # male:35 to 44 years, 11
        male.35to44 <- data[, 11]
        acs.colnames(male.35to44) <- "Number:Male:35 to 44 years"
        percent.male.35to44 <- divide.acs(male.35to44, total, method = "proportion")
        acs.colnames(percent.male.35to44) <- "Percent:Male:35 to 44 years"
        # male:45 to 54 years, 12
        male.45to54 <- data[, 12]
        acs.colnames(male.45to54) <- "Number:Male:45 to 54 years"
        percent.male.45to54 <- divide.acs(male.45to54, total, method = "proportion")
        acs.colnames(percent.male.45to54) <- "Percent:Male:45 to 54 years"
        # male:55 to 64 years, 13
        male.55to64 <- data[, 13]
        acs.colnames(male.55to64) <- "Number:Male:55 to 64 years"
        percent.male.55to64 <- divide.acs(male.55to64, total, method = "proportion")
        acs.colnames(percent.male.55to64) <- "Percent:Male:55 to 64 years"
        # male:65 to 74 years, 14
        male.65to74 <- data[, 14]
        acs.colnames(male.65to74) <- "Number:Male:65 to 74 years"
        percent.male.65to74 <- divide.acs(male.65to74, total, method = "proportion")
        acs.colnames(percent.male.65to74) <- "Percent:Male:65 to 74 years"
        # male:75 to 84 years, 15
        male.75to84 <- data[, 15]
        acs.colnames(male.75to84) <- "Number:Male:75 to 84 years"
        percent.male.75to84 <- divide.acs(male.75to84, total, method = "proportion")
        acs.colnames(percent.male.75to84) <- "Percent:Male:75 to 84 years"
        # male:85 years and over, 16
        male.85over <- data[, 16]
        acs.colnames(male.85over) <- "Number:Male:85 years and over"
        percent.male.85over <- divide.acs(male.85over, total, method = "proportion")
        acs.colnames(percent.male.85over) <- "Percent:Male:85 years and over"

        # Gender = Female
        # Female:Total, 17
        female.total <- data[, 17]
        acs.colnames(total) <- "Number:Female:Total"
        percent.female.total <- divide.acs(female.total, total, method = "proportion")
        acs.colnames(percent.female.total) <- "Percent:Female:Total"
        # Female:0 to 4 years, 18
        female.0to4 <- data[, 18]
        acs.colnames(female.0to4) <- "Number:Female:0 to 4 years"
        percent.female.0to4 <- divide.acs(female.0to4, total, method = "proportion")
        acs.colnames(percent.female.0to4) <- "Percent:Female:0 to 4 years"
        # Female:5 to 9 years, 19
        female.5to9 <- data[, 19]
        acs.colnames(female.5to9) <- "Number:Female:5 to 9 years"
        percent.female.5to9 <- divide.acs(female.5to9, total, method = "proportion")
        acs.colnames(percent.female.5to9) <- "Percent:Female:5 to 9 years"
        # Female:10 to 14 years, 20
        female.10to14 <- data[, 20]
        acs.colnames(female.10to14) <- "Number:Female:10 to 14 years"
        percent.female.10to14 <- divide.acs(female.10to14, total, method = "proportion")
        acs.colnames(percent.female.10to14) <- "Percent:Female:10 to 14 years"
        # Female:15 to 19 years, 21:22
        female.15to19 <- acsSum(data, c(21:22), "Number:Female:15 to 19 years")
        percent.female.15to19 <- divide.acs(female.15to19, total, method = "proportion")
        acs.colnames(percent.female.15to19) <- "Percent:Female:15 to 19 years"
        # Female:20 to 24 years, 23
        female.20to24 <- data[, 23]
        acs.colnames(female.20to24) <- "Number:Female:20 to 24 years"
        percent.female.20to24 <- divide.acs(female.20to24, total, method = "proportion")
        acs.colnames(percent.female.20to24) <- "Percent:Female:20 to 24 years"
        # Female:25 to 29 years, 24
        female.25to29 <- data[, 24]
        acs.colnames(female.25to29) <- "Number:Female:25 to 29 years"
        percent.female.25to29 <- divide.acs(female.25to29, total, method = "proportion")
        acs.colnames(percent.female.25to29) <- "Percent:Female:25 to 29 years"
        # Female:30 to 34 years, 25
        female.30to34 <- data[, 25]
        acs.colnames(female.30to34) <- "Number:Female:30 to 34 years"
        percent.female.30to34 <- divide.acs(female.30to34, total, method = "proportion")
        acs.colnames(percent.female.30to34) <- "Percent:Female:30 to 34 years"
        # Female:35 to 44 years, 26
        female.35to44 <- data[, 26]
        acs.colnames(female.35to44) <- "Number:Female:35 to 44 years"
        percent.female.35to44 <- divide.acs(female.35to44, total, method = "proportion")
        acs.colnames(percent.female.35to44) <- "Percent:Female:35 to 44 years"
        # Female:45 to 54 years,  27
        female.45to54 <- data[, 27]
        acs.colnames(female.45to54) <- "Number:Female:45 to 54 years"
        percent.female.45to54 <- divide.acs(female.45to54, total, method = "proportion")
        acs.colnames(percent.female.45to54) <- "Percent:Female:45 to 54 years"
        # Female:55 to 64 years, 28
        female.55to64 <- data[, 28]
        acs.colnames(female.55to64) <- "Number:Female:55 to 64 years"
        percent.female.55to64 <- divide.acs(female.55to64, total, method = "proportion")
        acs.colnames(percent.female.55to64) <- "Percent:Female:55 to 64 years"
        # Female:65 to 74 years,  29
        female.65to74 <- data[, 29]
        acs.colnames(female.65to74) <- "Number:Female:65 to 74 years"
        percent.female.65to74 <- divide.acs(female.65to74, total, method = "proportion")
        acs.colnames(percent.female.65to74) <- "Percent:Female:65 to 74 years"
        # Female:75 to 84 years,  30
        female.75to84 <- data[, 30]
        acs.colnames(female.75to84) <- "Number:Female:75 to 84 years"
        percent.female.75to84 <- divide.acs(female.75to84, total, method = "proportion")
        acs.colnames(percent.female.75to84) <- "Percent:Female:75 to 84 years"
        # Female:85 years and over, 31
        female.85over <- data[, 31]
        acs.colnames(female.85over) <- "Number:Female:85 years and over"
        percent.female.85over <- divide.acs(female.85over, total, method = "proportion")
        acs.colnames(percent.female.85over) <- "Percent:Female:85 years and over"
    }
    
# merge in fips
    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
            geo,
            estimate(total),
            estimate(total.0to4),
            estimate(total.5to9),
            estimate(total.10to14),
            estimate(total.15to19),
            estimate(total.20to24),
            estimate(total.25to29),
            estimate(total.30to34),
            estimate(total.35to44),
            estimate(total.45to54),
            estimate(total.55to64),
            estimate(total.65to74),
            estimate(total.75to84),
            estimate(total.85over),
            estimate(male.total),
            estimate(male.0to4),
            estimate(male.5to9),
            estimate(male.10to14),
            estimate(male.15to19),
            estimate(male.20to24),
            estimate(male.25to29),
            estimate(male.30to34),
            estimate(male.35to44),
            estimate(male.45to54),
            estimate(male.55to64),
            estimate(male.65to74),
            estimate(male.75to84),
            estimate(male.85over),
            estimate(female.total),
            estimate(female.0to4),
            estimate(female.5to9),
            estimate(female.10to14),
            estimate(female.15to19),
            estimate(female.20to24),
            estimate(female.25to29),
            estimate(female.30to34),
            estimate(female.35to44),
            estimate(female.45to54),
            estimate(female.55to64),
            estimate(female.65to74),
            estimate(female.75to84),
            estimate(female.85over),
            year,
            race,
            "Number",
            "Population"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(total.0to4) * 1.645,
            standard.error(total.5to9) * 1.645,
            standard.error(total.10to14) * 1.645,
            standard.error(total.15to19) * 1.645,
            standard.error(total.20to24) * 1.645,
            standard.error(total.25to29) * 1.645,
            standard.error(total.30to34) * 1.645,
            standard.error(total.35to44) * 1.645,
            standard.error(total.45to54) * 1.645,
            standard.error(total.55to64) * 1.645,
            standard.error(total.65to74) * 1.645,
            standard.error(total.75to84) * 1.645,
            standard.error(total.85over) * 1.645,
            standard.error(male.total) * 1.645,
            standard.error(male.0to4) * 1.645,
            standard.error(male.5to9) * 1.645,
            standard.error(male.10to14) * 1.645,
            standard.error(male.15to19) * 1.645,
            standard.error(male.20to24) * 1.645,
            standard.error(male.25to29) * 1.645,
            standard.error(male.30to34) * 1.645,
            standard.error(male.35to44) * 1.645,
            standard.error(male.45to54) * 1.645,
            standard.error(male.55to64) * 1.645,
            standard.error(male.65to74) * 1.645,
            standard.error(male.75to84) * 1.645,
            standard.error(male.85over) * 1.645,
            standard.error(female.total) * 1.645,
            standard.error(female.0to4) * 1.645,
            standard.error(female.5to9) * 1.645,
            standard.error(female.10to14) * 1.645,
            standard.error(female.15to19) * 1.645,
            standard.error(female.20to24) * 1.645,
            standard.error(female.25to29) * 1.645,
            standard.error(female.30to34) * 1.645,
            standard.error(female.35to44) * 1.645,
            standard.error(female.45to54) * 1.645,
            standard.error(female.55to64) * 1.645,
            standard.error(female.65to74) * 1.645,
            standard.error(female.75to84) * 1.645,
            standard.error(female.85over) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Number:Total:Total",
            "Number:Total:0 to 4 years",
            "Number:Total:5 to 9 years",
            "Number:Total:10 to 14 years",
            "Number:Total:15 to 19 years",
            "Number:Total:20 to 24 years",
            "Number:Total:25 to 29 years",
            "Number:Total:30 to 34 years",
            "Number:Total:35 to 44 years",
            "Number:Total:45 to 54 years",
            "Number:Total:55 to 64 years",
            "Number:Total:65 to 74 years",
            "Number:Total:75 to 84 years",
            "Number:Total:85 years and over",
            "Number:Male:Total",
            "Number:Male:0 to 4 years",
            "Number:Male:5 to 9 years",
            "Number:Male:10 to 14 years",
            "Number:Male:15 to 19 years",
            "Number:Male:20 to 24 years",
            "Number:Male:25 to 29 years",
            "Number:Male:30 to 34 years",
            "Number:Male:35 to 44 years",
            "Number:Male:45 to 54 years",
            "Number:Male:55 to 64 years",
            "Number:Male:65 to 74 years",
            "Number:Male:75 to 84 years",
            "Number:Male:85 years and over",
            "Number:Female:Total",
            "Number:Female:0 to 4 years",
            "Number:Female:5 to 9 years",
            "Number:Female:10 to 14 years",
            "Number:Female:15 to 19 years",
            "Number:Female:20 to 24 years",
            "Number:Female:25 to 29 years",
            "Number:Female:30 to 34 years",
            "Number:Female:35 to 44 years",
            "Number:Female:45 to 54 years",
            "Number:Female:55 to 64 years",
            "Number:Female:65 to 74 years",
            "Number:Female:75 to 84 years",
            "Number:Female:85 years and over",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars = c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name = "Population",
            variable.factor = F,
            value.name = "Value",
            value.factor = F
         )

    percentEstimates <- data.table(
            geo,
            # estimate(percent.total),
            estimate(percent.total.0to4),
            estimate(percent.total.5to9),
            estimate(percent.total.10to14),
            estimate(percent.total.15to19),
            estimate(percent.total.20to24),
            estimate(percent.total.25to29),
            estimate(percent.total.30to34),
            estimate(percent.total.35to44),
            estimate(percent.total.45to54),
            estimate(percent.total.55to64),
            estimate(percent.total.65to74),
            estimate(percent.total.75to84),
            estimate(percent.total.85over),
            estimate(percent.male.total),
            estimate(percent.male.0to4),
            estimate(percent.male.5to9),
            estimate(percent.male.10to14),
            estimate(percent.male.15to19),
            estimate(percent.male.20to24),
            estimate(percent.male.25to29),
            estimate(percent.male.30to34),
            estimate(percent.male.35to44),
            estimate(percent.male.45to54),
            estimate(percent.male.55to64),
            estimate(percent.male.65to74),
            estimate(percent.male.75to84),
            estimate(percent.male.85over),
            estimate(percent.female.total),
            estimate(percent.female.0to4),
            estimate(percent.female.5to9),
            estimate(percent.female.10to14),
            estimate(percent.female.15to19),
            estimate(percent.female.20to24),
            estimate(percent.female.25to29),
            estimate(percent.female.30to34),
            estimate(percent.female.35to44),
            estimate(percent.female.45to54),
            estimate(percent.female.55to64),
            estimate(percent.female.65to74),
            estimate(percent.female.75to84),
            estimate(percent.female.85over),
            year,
            race,
            "percent",
            "Population"
        )
    percentMOES <- data.table(
            geo,
            # standard.error(percent.total) * 1.645,
            standard.error(percent.total.0to4) * 1.645,
            standard.error(percent.total.5to9) * 1.645,
            standard.error(percent.total.10to14) * 1.645,
            standard.error(percent.total.15to19) * 1.645,
            standard.error(percent.total.20to24) * 1.645,
            standard.error(percent.total.25to29) * 1.645,
            standard.error(percent.total.30to34) * 1.645,
            standard.error(percent.total.35to44) * 1.645,
            standard.error(percent.total.45to54) * 1.645,
            standard.error(percent.total.55to64) * 1.645,
            standard.error(percent.total.65to74) * 1.645,
            standard.error(percent.total.75to84) * 1.645,
            standard.error(percent.total.85over) * 1.645,
            standard.error(percent.male.total) * 1.645,
            standard.error(percent.male.0to4) * 1.645,
            standard.error(percent.male.5to9) * 1.645,
            standard.error(percent.male.10to14) * 1.645,
            standard.error(percent.male.15to19) * 1.645,
            standard.error(percent.male.20to24) * 1.645,
            standard.error(percent.male.25to29) * 1.645,
            standard.error(percent.male.30to34) * 1.645,
            standard.error(percent.male.35to44) * 1.645,
            standard.error(percent.male.45to54) * 1.645,
            standard.error(percent.male.55to64) * 1.645,
            standard.error(percent.male.65to74) * 1.645,
            standard.error(percent.male.75to84) * 1.645,
            standard.error(percent.male.85over) * 1.645,
            standard.error(percent.female.total) * 1.645,
            standard.error(percent.female.0to4) * 1.645,
            standard.error(percent.female.5to9) * 1.645,
            standard.error(percent.female.10to14) * 1.645,
            standard.error(percent.female.15to19) * 1.645,
            standard.error(percent.female.20to24) * 1.645,
            standard.error(percent.female.25to29) * 1.645,
            standard.error(percent.female.30to34) * 1.645,
            standard.error(percent.female.35to44) * 1.645,
            standard.error(percent.female.45to54) * 1.645,
            standard.error(percent.female.55to64) * 1.645,
            standard.error(percent.female.65to74) * 1.645,
            standard.error(percent.female.75to84) * 1.645,
            standard.error(percent.female.85over) * 1.645,
            year,
            race,
            "percent",
            "Margins of Error"
        )
    percentNames <- c(
            "FIPS",
            # "Percent:Total:Total",
            "Percent:Total:0 to 4 years",
            "Percent:Total:5 to 9 years",
            "Percent:Total:10 to 14 years",
            "Percent:Total:15 to 19 years",
            "Percent:Total:20 to 24 years",
            "Percent:Total:25 to 29 years",
            "Percent:Total:30 to 34 years",
            "Percent:Total:35 to 44 years",
            "Percent:Total:45 to 54 years",
            "Percent:Total:55 to 64 years",
            "Percent:Total:65 to 74 years",
            "Percent:Total:75 to 84 years",
            "Percent:Total:85 years and over",
            "Percent:Male:Total",
            "Percent:Male:0 to 4 years",
            "Percent:Male:5 to 9 years",
            "Percent:Male:10 to 14 years",
            "Percent:Male:15 to 19 years",
            "Percent:Male:20 to 24 years",
            "Percent:Male:25 to 29 years",
            "Percent:Male:30 to 34 years",
            "Percent:Male:35 to 44 years",
            "Percent:Male:45 to 54 years",
            "Percent:Male:55 to 64 years",
            "Percent:Male:65 to 74 years",
            "Percent:Male:75 to 84 years",
            "Percent:Male:85 years and over",
            "Percent:Female:Total",
            "Percent:Female:0 to 4 years",
            "Percent:Female:5 to 9 years",
            "Percent:Female:10 to 14 years",
            "Percent:Female:15 to 19 years",
            "Percent:Female:20 to 24 years",
            "Percent:Female:25 to 29 years",
            "Percent:Female:30 to 34 years",
            "Percent:Female:35 to 44 years",
            "Percent:Female:45 to 54 years",
            "Percent:Female:55 to 64 years",
            "Percent:Female:65 to 74 years",
            "Percent:Female:75 to 84 years",
            "Percent:Female:85 years and over",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars = c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name = "Population",
            variable.factor = F,
            value.name = "Value",
            value.factor = F
         )

    inter_data <- rbind(inter_data, numbersData.melt, percentsData.melt)
  }
  town_data <- rbind(town_data, inter_data)
}

#Merge in FIPS
town_fips_dp_URL <- 'towns/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

town_data <- merge(towns, town_data, by="FIPS", all.x=T)

town_data <- town_data[town_data$Town != "Oregon",]

dataset <- rbind(state_data, town_data)

#Final Additions, processing
# Split Measure type and Tenure out of "housing units" column, then drop that column
dataset[,c("Measure Type", "Gender", "Age Cohort"):=do.call(Map, c(f = c, strsplit(`Population`, ":", fixed = T)))]
dataset[,`Population` := NULL]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 2)]


dataset$`Age Cohort` <- factor(dataset$`Age Cohort`, levels = c("Total",             
                     "0 to 4 years", 
                     "5 to 9 years", 
                     "10 to 14 years", 
                     "15 to 19 years", 
                     "20 to 24 years", 
                     "25 to 29 years", 
                     "30 to 34 years", 
                     "35 to 44 years", 
                     "45 to 54 years", 
                     "55 to 64 years", 
                     "65 to 74 years", 
                     "75 to 84 years",
                     "85 years and over"))

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, Gender, `Race/Ethnicity`, `Age Cohort`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Gender, `Race/Ethnicity`, `Age Cohort`, `Measure Type`, desc(Variable))

write.table(
    dataset,
    file.path("data", "population-by-age-town-2021.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)




