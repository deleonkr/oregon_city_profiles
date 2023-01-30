library(plyr)
library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Educational Attainment
# Created by Kristine de Leon
# On 01/20/2021
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

orGeos <- getORGeos()
yearList = c(2021)
tn = "B15002"
acsdata <- getACSData(orGeos, yearList=yearList, table = tn)

dataset <- data.table()

for(data in acsdata) {
    #year <- data@endyear
    year <- 2021
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep="-")

    #total populations
    total <- data[,1]
    acs.colnames(total) <- "Number:Total:Total"

    total.total.percent <- divide.acs(total, total, method="proportion", verbose = T)
    acs.colnames(total.total.percent) <- "Percent:Total:Total"

    total.under.hs <- acsSum(data, c(3:10, 20:27), "Number:Total:Less than High School Diploma")
    percent.total.under.hs <- divide.acs(total.under.hs, total, method="proportion", verbose=T)
    acs.colnames(percent.total.under.hs) <- "Percent:Total:Less than High School Diploma"

    total.hs <- acsSum(data, c(11, 28), "Number:Total:High School Diploma, GED, or equivalent")
    percent.total.hs <- divide.acs(total.hs, total, method="proportion", verbose=T)
    acs.colnames(percent.total.hs) <- "Percent:Total:High School Diploma, GED, or equivalent"

    total.some.college <- acsSum(data, c(12:13, 29:30), "Number:Total:Some College")
    percent.total.some.college <- divide.acs(total.some.college, total, method="proportion", verbose=T)
    acs.colnames(percent.total.some.college) <- "Percent:Total:Some College"

    total.associates <- acsSum(data, c(14, 31), "Number:Total:Associate's Degree")
    percent.total.associates <- divide.acs(total.associates, total, method="proportion", verbose=T)
    acs.colnames(percent.total.associates) <- "Percent:Total:Associate's Degree"

    total.bachelors.plus <- acsSum(data, c(15:18, 32:35), "Number:Total:Bachelor's Degree or higher")
    percent.total.bachelors.plus <- divide.acs(total.bachelors.plus, total, method="proportion", verbose=T)
    acs.colnames(percent.total.bachelors.plus) <- "Percent:Total:Bachelor's Degree or higher"

    # Male population
    total.male <- data[,2]
    acs.colnames(total.male) <- "Male:Total"

    total.male.percent <- divide.acs(total.male, total, method="proportion", verbose = T)
    acs.colnames(total.male.percent) <- "Percent:Male:Total"

    male.under.hs <- acsSum(data, c(3:10), "Number:Male:Less than High School Diploma")
    percent.male.under.hs <- divide.acs(male.under.hs, total.male, method="proportion", verbose=T)
    acs.colnames(percent.male.under.hs) <- "Percent:Male:Less than High School Diploma"

    male.hs <- acsSum(data, c(11), "Number:Male:High School Diploma, GED, or equivalent")
    percent.male.hs <- divide.acs(male.hs, total.male, method="proportion", verbose=T)
    acs.colnames(percent.male.hs) <- "Percent:Male:High School Diploma, GED, or equivalent"

    male.some.college <- acsSum(data, c(12:13), "Number:Male:Some College")
    percent.male.some.college <- divide.acs(male.some.college, total.male, method="proportion", verbose=T)
    acs.colnames(percent.male.some.college) <- "Percent:Male:Some College"

    male.associates <- data[,14]
    acs.colnames(male.associates) <- "Number:Male:Associate's Degree"
    percent.male.associates <- divide.acs(male.associates, total.male, method="proportion", verbose=T)
    acs.colnames(percent.male.associates) <- "Percent:Male:Associate's Degree"

    male.bachelors.plus <- acsSum(data, c(15:18), "Number:Male:Bachelor's Degree or higher")
    percent.male.bachelors.plus <- divide.acs(male.bachelors.plus, total.male, method="proportion", verbose=T)
    acs.colnames(percent.male.bachelors.plus) <- "Percent:Male:Bachelor's Degree or higher"

    # Female population
    total.female <- data[,19]
    acs.colnames(total.female) <- "Female:Total"

    total.female.percent <- divide.acs(total.female, total, method="proportion", verbose = T)
    acs.colnames(total.female.percent) <- "Percent:Female:Total"

    female.under.hs <- acsSum(data, c(20:27), "Number:Female:Less than High School Diploma")
    percent.female.under.hs <- divide.acs(female.under.hs, total.female, method="proportion", verbose=T)
    acs.colnames(percent.female.under.hs) <- "Percent:Female:Less than High School Diploma"

    female.hs <- acsSum(data, c(28), "Number:Female:High School Diploma, GED, or equivalent")
    percent.female.hs <- divide.acs(female.hs, total.female, method="proportion", verbose=T)
    acs.colnames(percent.female.hs) <- "Percent:Female:High School Diploma, GED, or equivalent"

    female.some.college <- acsSum(data, c(29:30), "Number:Female:Some College")
    percent.female.some.college <- divide.acs(female.some.college, total.female, method="proportion", verbose=T)
    acs.colnames(percent.female.some.college) <- "Percent:Female:Some College"

    female.associates <- data[,31]
    acs.colnames(female.associates) <- "Number:Female:Associate's Degree"
    percent.female.associates <- divide.acs(female.associates, total.female, method="proportion", verbose=T)
    acs.colnames(percent.female.associates) <- "Percent:Female:Associate's Degree"

    female.bachelors.plus <- acsSum(data, c(32:35), "Number:Female:Bachelor's Degree or higher")
    percent.female.bachelors.plus <- divide.acs(female.bachelors.plus, total.female, method="proportion", verbose=T)
    acs.colnames(percent.female.bachelors.plus) <- "Percent:Female:Bachelor's Degree or higher"

    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
            datafips$fips,
            estimate(total),
            estimate(total.under.hs),
            estimate(total.hs),
            estimate(total.some.college),
            estimate(total.associates),
            estimate(total.bachelors.plus),
            estimate(total.male),
            estimate(male.under.hs),
            estimate(male.hs),
            estimate(male.some.college),
            estimate(male.associates),
            estimate(male.bachelors.plus),
            estimate(total.female),
            estimate(female.under.hs),
            estimate(female.hs),
            estimate(female.some.college),
            estimate(female.associates),
            estimate(female.bachelors.plus),
            year,
            "Number",
            "Educational Attainment"
        )
    numberMOES <- data.table(
            datafips$fips,
            standard.error(total) * 1.645,
            standard.error(total.under.hs) * 1.645,
            standard.error(total.hs) * 1.645,
            standard.error(total.some.college) * 1.645,
            standard.error(total.associates) * 1.645,
            standard.error(total.bachelors.plus) * 1.645,
            standard.error(total.male) * 1.645,
            standard.error(male.under.hs) * 1.645,
            standard.error(male.hs) * 1.645,
            standard.error(male.some.college) * 1.645,
            standard.error(male.associates) * 1.645,
            standard.error(male.bachelors.plus) * 1.645,
            standard.error(total.female) * 1.645,
            standard.error(female.under.hs) * 1.645,
            standard.error(female.hs) * 1.645,
            standard.error(female.some.college) * 1.645,
            standard.error(female.associates) * 1.645,
            standard.error(female.bachelors.plus) * 1.645,
            year,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total:Total",
            "Total:Less than High School Diploma",
            "Total:High School Diploma, GED, or equivalent",
            "Total:Some College",
            "Total:Associate's Degree",
            "Total:Bachelor's Degree or higher",
            "Male:Total",
            "Male:Less than High School Diploma",
            "Male:High School Diploma, GED, or equivalent",
            "Male:Some College",
            "Male:Associate's Degree",
            "Male:Bachelor's Degree or higher",
            "Female:Total",
            "Female:Less than High School Diploma",
            "Female:High School Diploma, GED, or equivalent",
            "Female:Some College",
            "Female:Associate's Degree",
            "Female:Bachelor's Degree or higher",
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Educational Attainment",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    percentEstimates <- data.table(
            datafips$fips,
            estimate(total.total.percent),
            estimate(percent.total.under.hs),
            estimate(percent.total.hs),
            estimate(percent.total.some.college),
            estimate(percent.total.associates),
            estimate(percent.total.bachelors.plus),
            estimate(total.male.percent),
            estimate(percent.male.under.hs),
            estimate(percent.male.hs),
            estimate(percent.male.some.college),
            estimate(percent.male.associates),
            estimate(percent.male.bachelors.plus),
            estimate(total.female.percent),
            estimate(percent.female.under.hs),
            estimate(percent.female.hs),
            estimate(percent.female.some.college),
            estimate(percent.female.associates),
            estimate(percent.female.bachelors.plus),
            year,
            "Percent",
            "Educational Attainment"
        )
    percentMOES <- data.table(
                datafips$fips,
                standard.error(total.total.percent) * 1.645,
                standard.error(percent.total.under.hs) * 1.645,
                standard.error(percent.total.hs) * 1.645,
                standard.error(percent.total.some.college) * 1.645,
                standard.error(percent.total.associates) * 1.645,
                standard.error(percent.total.bachelors.plus) * 1.645,
                standard.error(total.male.percent) * 1.645,
                standard.error(percent.male.under.hs) * 1.645,
                standard.error(percent.male.hs) * 1.645,
                standard.error(percent.male.some.college) * 1.645,
                standard.error(percent.male.associates) * 1.645,
                standard.error(percent.male.bachelors.plus) * 1.645,
                standard.error(total.female.percent) * 1.645,
                standard.error(percent.female.under.hs) * 1.645,
                standard.error(percent.female.hs) * 1.645,
                standard.error(percent.female.some.college) * 1.645,
                standard.error(percent.female.associates) * 1.645,
                standard.error(percent.female.bachelors.plus) * 1.645,
                year,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Total:Total",
            "Total:Less than High School Diploma",
            "Total:High School Diploma, GED, or equivalent",
            "Total:Some College",
            "Total:Associate's Degree",
            "Total:Bachelor's Degree or higher",
            "Male:Total",
            "Male:Less than High School Diploma",
            "Male:High School Diploma, GED, or equivalent",
            "Male:Some College",
            "Male:Associate's Degree",
            "Male:Bachelor's Degree or higher",
            "Female:Total",
            "Female:Less than High School Diploma",
            "Female:High School Diploma, GED, or equivalent",
            "Female:Some College",
            "Female:Associate's Degree",
            "Female:Bachelor's Degree or higher",
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Educational Attainment",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    dataset <- rbind(dataset, numbersData.melt, percentData.melt)
}

#backup <- dataset
#dataset <- backup
#Final Additions, processing
# Split Gender and Educational Attainment out of variable
dataset[,c("Gender", "Educational Attainment"):=do.call(Map, c(f=c, strsplit(`Educational Attainment`, ":", fixed=T)))]
# Round Values according to type/variable
# Numbers
dataset[`Measure Type` == "Number" & Variable == "Margins of Error", Value := round(Value, 0)]
dataset[`Measure Type` == "Number" & Variable == "Educational Attainment", Value := round(Value, 2)]


# Percent
dataset[`Measure Type` == 'Percent' & Variable == "Margins of Error", Value := round(Value*100, 2)]
dataset[`Measure Type` == "Percent" & Variable == "Educational Attainment", Value := round(Value*100, 2)]



# Join town names by FIPS code
town_fips_dp_URL <- 'towns/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- as.data.frame(dataset)

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

dataset <- dataset %>% 
  select(Town, FIPS, Year, Gender, `Educational Attainment`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Gender, `Educational Attainment`, `Measure Type`, Variable)

# write table
write.table(
    dataset,
    file.path("data", "educational_attainment_2021.csv"),
    sep = ",",
    row.names=F,
    na = "-9999"
)