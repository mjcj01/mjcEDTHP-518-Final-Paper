library(tidyverse)
library(tidycensus)
library(sf)

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

### Loading in school district data from the U.S. Census
school_district_demographics_22 <- get_acs(geography = "school district (unified)",
                                        state = "PA",
                                        year = 2022,
                                        variables = "B01001_001",
                                        geometry = TRUE,
                                        output = "wide") %>%
  filter(NAME != "School District Not Defined, Pennsylvania") %>%
  mutate(NAME = str_to_upper(gsub("School District, Pennsylvania", "SD", NAME)),
         NAME = gsub("SOUTH BUTLER COUNTY SD", "KNOCH SD", NAME),
         NAME = gsub("BLAIRSVILLE-SALTSBURG SD", "RIVER VALLEY SD", NAME)) %>% 
  rename(school_district = "NAME")

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

urban_school_codes <- read_csv("Data//urban_codes.csv") %>%
  rename(school_district = "LEA NAME") %>%
  mutate("code" = ifelse(grepl("City", `Urban-centric Locale [District]`), "City", 
                  ifelse(grepl("Suburb", `Urban-centric Locale [District]`), "Suburb",
                  ifelse(grepl("Town", `Urban-centric Locale [District]`), "Town",
                  ifelse(grepl("Rural", `Urban-centric Locale [District]`), "Rural", "NULL")))))
  

annual_state_rev %>%
  group_by(year) %>%
  drop_na(bef) %>%
  summarise(med = sum(bef)) %>%
  merge(.,
        data_frame("year" = c(2014:2021),
                   "rate" = c(1.6, 0.1, 1.3, 2.1, 2.4, 1.8, 1.2, 4.7)),
        by = "year") %>%
  ggplot(data = ., aes(x = year, y = med)) + 
  geom_line()

data_merge <- annual_state_rev %>%
  merge(., shapiro_adequacy_investment_2425 %>% select(-school_district), by = "AUN") %>%
  merge(., shapiro_proposed_bef_funding_2425 %>% select(-school_district), by = "AUN") %>%
  merge(., urban_school_codes, by = "AUN") %>%
  select(AUN, school_district.y, county.x, total_state_rev, bef, year, weighted_students,
         adequacy_target, adequacy_gap, state_share_ag, total_proposed_bef, base_bef_2324,
         proposed_formula_dist, adequacy_investment, est_bef, code) %>%
  merge(., school_district_demographics_22, by.x = "school_district.y", by.y = "school_district")


