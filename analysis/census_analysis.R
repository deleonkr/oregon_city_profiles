# Wrangling Census data with tidyverse tools

library(tidyverse)
library(tidycensus)
library(sf)
library(ggsflabel)
library(scales)
library(tigris)

install.packages("remotes")
remotes::install_github("walkerke/tidycensus")

# Counties of focus for selecting city to profile
my_counties <- c(
  "Washington",
  "Multnomah",
  "Clackamas"
  )

# get place and county geometries from {tigris} within Oregon
or_places <- tigris::places(state = "OR", cb = TRUE, class = "sf") %>% 
  select(place_fips = GEOID, place_name = NAME)

or_counties <- tigris::counties(state = "OR", cb = TRUE, class = "sf") %>% 
  select(county_fips = GEOID, county_name = NAME)

# spatial join places and counties to get counties that intersect places
# only select the counties of interest
portland_places <- st_join(or_places, or_counties, largest = TRUE) %>% 
  filter(county_name %in% my_counties)
#Warning message: attribute variables are assumed to be spatially constant throughout all geometries 

# make a little map for fun to test out census places in multnomah county 
ggplot(portland_places) +
  geom_sf(data = filter(or_counties, county_name %in% my_counties)) +
  geom_sf(color = "blue") +
  ggsflabel::geom_sf_text_repel(aes(label = place_name), nudge_x = 0.03, nudge_y = -0.02) +
  labs(title = "Census places in Multnomah County, OR") +
  theme_void()


#POPULATION OVER TIME
#iterating over ACS5 years 
pop_years <- 2010:2021
names(pop_years) <- pop_years

or_pop_by_year <- map_dfr(pop_years, ~{
  get_acs(
    geography = "place",
    variables = "B01003_001",
    state = "OR",
    survey = "acs5",
    year = .x
  )
}, .id = "year")

#arrange the results
or_pop_by_year %>% 
  arrange(NAME, variable, year)

#filter for portland metro area
pdx_pop_by_year <- or_pop_by_year %>% 
    filter(GEOID %in% portland_places$place_fips)

#write both oregon and pdx metro population results to csv
write.csv(or_pop_by_year, "or_pop_by_year.csv", row.names=FALSE)
write.csv(pdx_pop_by_year, "pdx_pop_by_year.csv", row.names=FALSE)

#COMPARING POPULATION FROM 2016 TO 2021

#years of interest
years <- lst(2016, 2021)
# which census variables?
my_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001"
  )

# loop over list of years and get 5 year acs estimates for all places in OR
or_pop_years <- map_dfr(
  years,
  ~ get_acs(
      geography = "place",
      variables = my_vars,
      state = "OR",
      year = .x,
      survey = "acs5",
      geometry = FALSE
      ),
  .id = "year"  # when combining results, add id var (name of list item)
  ) %>%
  select(-moe) %>%  # shhhh
  arrange(variable, NAME) %>% 
  print()

# reshape and calculate percent change in population
or_pop_16_21 <- or_pop_years %>% 
  spread(year, estimate, sep = "_") %>% 
  mutate(
    year_2016 = if_else(
      variable == "total_pop",
      year_2016
      ),
    change = year_2021 - year_2016,
    pct_change = change / year_2016 * 100
    )

# which places had the largest percent increase in population?
or_pop_change_16_21 <- or_pop_16_21 %>% 
  filter(variable == "total_pop") %>% 
  arrange(desc(pct_change))

# which places had the largest percent increase in median income?
or_income_change_16_21 <- or_pop_16_21 %>% 
  filter(variable == "median_income") %>% 
  arrange(desc(pct_change))

#filter for portland metro area for both total_pop and median_income
pdx_pop_change_16_21 <- or_pop_change_16_21 %>% 
    filter(GEOID %in% portland_places$place_fips)

pdx_income_change_16_21 <- or_income_change_16_21 %>% 
    filter(GEOID %in% portland_places$place_fips)

#save to csv for both population and income data
write.csv(pdx_pop_change_16_21, "pdx_pop_change_16_21.csv", row.names=FALSE)
write.csv(pdx_income_change_16_21, "pdx_pop_change_16_21", row.names=FALSE)


# RACE/ETHNICITY 

# get variables of interest for looking at race/ethnicity
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012",
  Two_races = "B02001_008"
)

# loop over list of years and get 5 year acs estimates for all places in oregon for 2016 & 2021
oregon_race_compare <- map_dfr(
  years,
  ~ get_acs(
      geography = "place",
      variables = race_vars,
      state = "OR",
      year = .x,
      survey = "acs5",
      summary_var = "B03002_001",
      geometry = FALSE
      ),
  .id = "year"  # when combining results, add id var (name of list item)
  ) %>%
  select(-moe) %>%  # shhhh
  arrange(variable, NAME) %>% 
  print()

#filter data for just portland metro
pdx_race_compare <- oregon_race_compare %>% 
  filter(GEOID %in% portland_places$place_fips)

#get percent changes of race/ethnicity
pdx_race_compare_pct<- pdx_race_compare %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(year, GEOID, NAME, variable, estimate, summary_est, percent)

#save results to csv
write.csv(pdx_race_compare_pct, "pdx_race_compare_pct.csv", row.names=FALSE)

#clean race comparison data
pdx_race_compare_clean <- subset(pdx_race_compare_pct, select = -c(estimate, summary_est))

#reshape data to separate year 2016 and 2021 into different columns
# reshape and calculate percent change in income
pdx_race_pct_points <- pdx_race_compare_clean %>% 
  spread(year, percent, sep = "_") %>% 
  mutate(
    year_2016 = if_else(
      variable == "White",
      round(year_2016 * 1, 2),  
      year_2016
      ),
    pct_points_change = year_2021 - year_2016
    )
#save results to csv
write.csv(pdx_race_pct_points, "pdx_race_pct_points.csv", row.names=FALSE)


#arrange the results
or_pop_by_year %>% 
  arrange(NAME, variable, year)

##FINDINGS
#Cedar Hills CDP had the largest increase in nonwhite people from 2016 to 2021
#Durham city had a population growth of 46% from 2016 to 2021 and it's the second fastest diversifying city in the metro area
#Durham also has the 8th largest percentage change in median income from 2016 to 2021
#Gaston city is the one city with the largest median income percent change


#AGE GROUPS

#get acs5 table data for age ranges for year 2021
oregon_places_age <- get_acs(
  geography = "place",
  state = "OR",
  table = "B01001",
  survey = "acs5",
  summary_var = "B01001_001",
  year = 2021
)

#filter data for just portland metro
pdx_ages_21<- oregon_places_age %>% 
  filter(GEOID %in% portland_places$place_fips)

#get age groups percent
pdx_age_groups21_pct <- pdx_ages_21 %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)

#write to csv
write.csv(pdx_age_groups21_pct, "pdx_age_groups21_pct.csv", row.names=FALSE)



#HOUSING

#make a table of variables for analyzing housing data
housing_vars <- c(
    median_monthly_cost = "B25105_001E",
    median_housing_value = "B25077_001",
    avg_household_size = "B25010_001E",
    housing_units_mort = "B25027_002E",
    housing_units_no_mort = "B25027_010E",
    housing_units_status = "B25027_001E",
)

# median_real_estate_taxes = "B25010_001E"
# get 2021 acs5 data for housing variables of interest
or_housing_acs21 <- get_acs(
  geography = "place",
  variables = housing_vars,
  state = "OR",
  survey = "acs5",
  year = 2021
)

# filter housing within portland metro
pdx_housing_acs21 <- or_housing_acs21 %>% 
  filter(GEOID %in% portland_places$place_fips)



# EDUCATION DATA

# make a table of education variables of interest
#  Given that we only need a few variables (representing estimates of populations age 25+ who have levels of education, by sex), 
# we’ll request those variables directly rather than the entire B15002 table.
# note that we have to get male and female numbers for each one
ed_vars <- c(
    no_school_f = "B15002_020", 
    hs_no_diploma_f = "B15002_027", 
    hs_grad_f = "B15002_028",
    some_college_f = "B15002_030", 
    associates_f = "B15002_031",
    bachelors_f = "B15002_032",
    masters_f = "B15002_033",
    doctorate_f = "B15002_035",
    professional_f = "B15002_034",
    no_school_m = "B15002_003", 
    hs_no_diploma_m = "B15002_010", 
    hs_grad_m = "B15002_011",
    some_college_m = "B15002_013", 
    associates_m = "B15002_014",
    bachelors_m = "B15002_015",
    masters_m = "B15002_016",
    doctorate_m = "B15002_018",
    professional_m = "B15002_017",
    total = "B15002_001")

or_ed_21 <- get_acs(
  geography = "place",
  state = "OR",
  variables = ed_vars,
  survey = "acs5",
  year = 2021
)

#filter for pdx metro area only
pdx_ed_21 <- or_ed_21 %>% 
  filter(GEOID %in% portland_places$place_fips)

#write to csv
write.csv(pdx_ed_21, "pdx_ed_21.csv", row.names=FALSE)


# INCOME LEVELS

#oregon household incomes for 2021
or_hh_income21 <- get_acs(
  geography = "place",
  table = "B19001",
  state = "OR",
  survey = "acs5",
  year = 2021
)

# filter income data for only places within portland
pdx_hh_income21 <- or_hh_income21 %>% 
  filter(GEOID %in% portland_places$place_fips)

# split income groups
pdx_hh_income21_recode <- pdx_hh_income21 %>%
  filter(variable != "B19001_001") %>%
  mutate(incgroup = case_when(
     variable < "B19001_008" ~ "below35k", 
    variable < "B19001_013" ~ "bw35kand75k", 
    variable < "B19001_014" ~ "bw75kand100k", 
    TRUE ~ "above100k"
  )) 

#summarize income groups
pdx_income_groups21 <- pdx_hh_income21_recode %>%
  group_by(GEOID, incgroup) %>%
  summarize(estimate = sum(estimate))

#write to csv for visualization
write.csv(pdx_income_groups21, "pdx_income_groups21.csv", row.names=FALSE) 
