library(tidyverse)
library(sf)

merge(school_districts, shapiro_adequacy_investment_2425, by = "AUN") %>%
  merge(., annual_attendance %>% filter(year == 2020) %>% select(-school_district), by = "AUN") %>%
  merge(., urban_school_codes %>% select(-school_district), by = "AUN") %>%
  select(AUN, weighted_students, total_adequacy_investment, exp_per_adm, code, geometry) %>%
  mutate("investment_per_ws" = total_adequacy_investment / weighted_students,
         "difference" = ((investment_per_ws + exp_per_adm) - exp_per_adm) / exp_per_adm) %>%
  select(-total_adequacy_investment) %>%
  st_write(., "Data//Shapefiles for ArcGIS//Adequacy Investment Increase//ade_increase.shp", append = FALSE)

merge(school_districts, shapiro_adequacy_investment_2425, by = "AUN") %>%
  merge(., annual_attendance %>% filter(year == 2020) %>% select(-school_district), by = "AUN") %>%
  merge(., urban_school_codes %>% select(-school_district), by = "AUN") %>%
  select(AUN, weighted_students, total_adequacy_investment, exp_per_adm, code, geometry) %>%
  mutate("investment_per_ws" = total_adequacy_investment / weighted_students,
         "difference" = ((investment_per_ws + exp_per_adm) - exp_per_adm) / exp_per_adm) %>%
  select(-total_adequacy_investment) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  merge(., spending_and_community_df %>% select(spending_and_community, AUN), by = "AUN") %>%
  st_write(., "Data//Shapefiles for ArcGIS//Investment Shapefile//investment_shp.shp", append = FALSE)
