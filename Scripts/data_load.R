library(tidyverse)
library(tidycensus)

### Loading in 2024-25 budget proposal data
adequacy_investment <- read_csv("Data//adequacy_investment.csv") %>%
  drop_na(school_district)
proposed_bef_funding <- read_csv("Data//proposed_bef_funding.csv") %>%
  drop_na(school_district)

### Loading in school district data from the U.S. Census
school_district_demographics <- get_acs(geography = "school district (unified)",
                                        state = "PA",
                                        year = 2022,
                                        variables = "B01001_001",
                                        geometry = FALSE,
                                        output = "wide") %>%
  filter(NAME != "School District Not Defined, Pennsylvania")
