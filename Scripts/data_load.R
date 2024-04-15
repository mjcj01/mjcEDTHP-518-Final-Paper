library(tidyverse)
library(tidycensus)
library(sf)
library(readxl)
### educationdata is an NCES CCD API
library(educationdata)

### Loading in 2024-25 budget proposal data
shapiro_adequacy_investment_2425 <- read_csv("Data//adequacy_investment.csv") %>%
  drop_na(school_district) %>%
  mutate(school_district = str_to_upper(gsub("  ", " ", school_district)),
         school_district = gsub("SAINT", "ST.", school_district),
         school_district = gsub("MT", "MOUNT", school_district),
         school_district = gsub("CHELTENHAM TOWNSHIP ", "CHELTENHAM ", school_district),
         school_district = gsub("OWEN J", "OWEN J.", school_district))

shapiro_proposed_bef_funding_2425 <- read_csv("Data//proposed_bef_funding.csv") %>%
  drop_na(school_district) %>%
  mutate(school_district = str_to_upper(gsub("  ", " ", school_district)),
         school_district = gsub("SAINT", "ST.", school_district),
         school_district = gsub("MT", "MOUNT", school_district),
         school_district = gsub("CHELTENHAM TOWNSHIP ", "CHELTENHAM ", school_district),
         school_district = gsub("OWEN J", "OWEN J.", school_district))

### Loading in school district data from PASDA

school_districts <- st_read("Data//Pennsylvania School District Shapefile//PaSchoolDistricts2024_03.shp") %>%
  rename(AUN = "AUN_SCHDIS")

### Loading in state revenue tables
annual_state_rev <- rbind(read_csv("Data/State Revenue/state_rev_1415.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2014),
      read_csv("Data/State Revenue/state_rev_1516.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2015),
      read_csv("Data/State Revenue/state_rev_1617.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2016),
      read_csv("Data/State Revenue/state_rev_1718.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2017),
      read_csv("Data/State Revenue/state_rev_1819.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2018),
      read_csv("Data/State Revenue/state_rev_1920.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2019),
      read_csv("Data/State Revenue/state_rev_2021.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2020),
      read_csv("Data/State Revenue/state_rev_2122.csv") %>%
        select(AUN, school_district, county, total_state_rev, bef) %>%
        mutate(year = 2021)) %>%
  mutate(bef = parse_number(bef),
         total_state_rev = parse_number(total_state_rev)) %>%
  filter(grepl("SD", school_district))

### Loading in local revenue data
annual_local_rev <- rbind(read_csv("Data/Local Revenue/local_rev_2021.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2014),
                          read_csv("Data/Local Revenue/local_rev_1920.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2015),
                          read_csv("Data/Local Revenue/local_rev_1819.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2016),
                          read_csv("Data/Local Revenue/local_rev_1718.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2017),
                          read_csv("Data/Local Revenue/local_rev_1617.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2018),
                          read_csv("Data/Local Revenue/local_rev_1516.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2019),
                          read_csv("Data/Local Revenue/local_rev_1415.csv", locale = locale(encoding = "latin1")) %>%
                            select(AUN, school_district, total_local_rev, real_estate_revenue) %>%
                            mutate(year = 2020)) %>%
  mutate(total_local_rev = parse_number(total_local_rev),
         real_estate_revenue = parse_number(real_estate_revenue)) %>%
  filter(grepl("SD", school_district)) %>%
  select(-school_district)

### Loading attendance data
annual_attendance <- rbind(read_excel("Data/Attendance Data/Selected Data 2014-15.xlsx") %>%
                             mutate(year = 2014),
                           read_excel("Data/Attendance Data/Selected Data 2015-16.xlsx") %>%
                             mutate(year = 2015),
                           read_excel("Data/Attendance Data/Selected Data 2016-17.xlsx") %>%
                             mutate(year = 2016),
                           read_excel("Data/Attendance Data/Selected Data 2017-18.xlsx") %>%
                             mutate(year = 2017),
                           read_excel("Data/Attendance Data/Selected Data 2018-19.xlsx") %>%
                             mutate(year = 2018),
                           read_excel("Data/Attendance Data/Selected Data 2019-20.xlsx") %>%
                             mutate(year = 2019),
                           read_excel("Data/Attendance Data/Selected Data 2020-21.xlsx") %>%
                             mutate(year = 2020)) %>%
  drop_na(school_district)

### Loading urban school codes
urban_school_codes <- read_csv("Data//urban_codes.csv") %>%
  rename(school_district = "LEA NAME") %>%
  mutate("code" = ifelse(grepl("City", `Urban-centric Locale [District]`), "City", 
                  ifelse(grepl("Suburb", `Urban-centric Locale [District]`), "Suburb",
                  ifelse(grepl("Town", `Urban-centric Locale [District]`), "Town",
                  ifelse(grepl("Rural", `Urban-centric Locale [District]`), "Rural", "NULL")))))
  
attendance_2022 <- read_excel("Data//Attendance Data//Finances SelectedData 2021-2022.xlsx") %>%
  drop_na(school_district)

### Crosswalk between AUN and NCES IDs provided by Matthew Kelly
nces_aun_crosswalk <- read_csv("Data//nces_aun_crosswalk.csv")

### Pulling NCES CCD enrollment data by race on all Pennsylvania school districts
nces_ccd <- get_education_data(level = "school-districts",
                               source = "ccd",
                               topic = "enrollment",
                               subtopic = "race",
                               filters = list(year = c(2014:2020),
                                              fips = 42,
                                              grade = 99)) %>%
  merge(.,
        data.frame("race_cat" = c("White", "Black", "Hispanic", "Asian", "American Indian or Alaskan Native",
                                  "Native Hawaiian or other Pacific Islander", "Two or more races",
                                  "Nonresident Alien", "Unknown", "Other", "Total"),
                   "race" = c(1:9, 20, 99)),
        by = "race") %>% 
  filter(grade == 99) %>%
  merge(.,
        nces_aun_crosswalk,
        by.x = c("leaid"), by.y = c("nces_id"))

community_data <- read_csv("Data//proposed_income_data.csv") %>%
  drop_na(AUN)

### Combining all data by AUN codes
data_merge <- annual_state_rev %>%
  merge(., shapiro_adequacy_investment_2425 %>% select(-school_district, -county), by = "AUN") %>%
  merge(., shapiro_proposed_bef_funding_2425 %>% select(-school_district, -county), by = "AUN") %>%
  merge(., annual_attendance %>% select(-school_district, -county) %>% drop_na(wadm), by = c("AUN", "year")) %>%
  merge(., urban_school_codes, by = "AUN") %>%
  merge(., school_districts, by = "AUN") %>%
  merge(., nces_ccd, by = c("AUN", "year")) %>%
  merge(., annual_local_rev, by = c("AUN", "year")) %>%
  merge(., community_data, by = "AUN") %>%
  filter(school_district.y != "BRYN ATHYN SD")
