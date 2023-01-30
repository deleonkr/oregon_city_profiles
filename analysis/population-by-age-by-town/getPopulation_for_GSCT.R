library(acs)
source('./scripts/acsHelpers.R')

tables <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

# Get geography object for OR and subcounty divisions
pop_by_race <- data.frame(stringsAsFactors = FALSE)
for (i in seq_along(tables)) {
  tbl <- tables[i]
  race <- races[i]
  acsdata <- getACSData(
    getORGeos("town"),
    yearList = 2021:2021,
    table = paste0("B01001", tbl)
  ) 
 
  pops <- data.frame(stringsAsFactors = FALSE)
  for (data in acsdata) {
    year <- data@endyear
    # pop.total <- acsSum(data, 1, "Total")
    # pop.total.f <- acsSum(data, 17, "Total Female")
    pop.total.f.0to4 <- acsSum(data, 18, "Female Under 5 years")
    pop.total.f.5to9 <- acsSum(data, 19, "Female 5 to 9 years")
    pop.total.f.10to14 <- acsSum(data, 20, "Female 10 to 14 years")
    pop.total.f.15to17 <- acsSum(data, 21, "Female 15 to 17 years")
    pop.total.f.18to19 <- acsSum(data, 22, "Female 18 and 19 years")
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        # estimate(pop.total),
        # estimate(pop.total.f), 
        estimate(pop.total.f.0to4),
        estimate(pop.total.f.5to9),
        estimate(pop.total.f.10to14),
        estimate(pop.total.f.15to17),
        estimate(pop.total.f.18to19)

    )
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )
    
# estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD01.Estimate; Total:")] <- paste0("All Genders ", race)
# estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD17.Estimate; Female:")] <- paste0("Female All Ages ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD18.Estimate; Female: - Under 5 years")] <- paste0("0 to 4 ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD19.Estimate; Female: - 5 to 9 years")] <- paste0("5 to 9 ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD20.Estimate; Female: - 10 to 14 years")] <- paste0("10 to 14 ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD21.Estimate; Female: - 15 to 17 years")] <- paste0("15 to 17 ", race)
estimates$`Race/Ethnicity`[which(estimates$`Race/Ethnicity` == "HD01_VD22.Estimate; Female: - 18 and 19 years")] <- paste0("18 and 19 ", race)



    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        # standard.error(pop.total) * 1.645,
        # standard.error(pop.total.f) * 1.645, 
        standard.error(pop.total.f.0to4) * 1.645,
        standard.error(pop.total.f.5to9) * 1.645,
        standard.error(pop.total.f.10to14) * 1.645,
        standard.error(pop.total.f.15to17) * 1.645,
        standard.error(pop.total.f.18to19) * 1.645        
    )

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Race/Ethnicity",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )
    
# moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD01.Estimate; Total:")] <-  paste0("All Genders ", race)
# moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD17.Estimate; Female:")] <-  paste0("Female All Ages ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD18.Estimate; Female: - Under 5 years")] <- paste0("0 to 4 ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD19.Estimate; Female: - 5 to 9 years")] <- paste0("5 to 9 ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD20.Estimate; Female: - 10 to 14 years")] <- paste0("10 to 14 ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD21.Estimate; Female: - 15 to 17 years")] <- paste0("15 to 17 ", race)
moes$`Race/Ethnicity`[which(moes$`Race/Ethnicity` == "HD01_VD22.Estimate; Female: - 18 and 19 years")] <- paste0("18 and 19 ", race)

    setkey(estimates, FIPS, Year, `Race/Ethnicity`)
    setkey(moes, FIPS, Year, `Race/Ethnicity`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "4100100000",]
pop_by_race <- rbind(pops, pop_by_race)
}

# Write to File
write.table(
    pop_by_race,
    file.path(getwd(), "raw", "populations_by_race_by_age.csv"),
    sep = ",",
    row.names = F
)