library(tidyverse)
library(spdep)

investment_per_ws <- shapiro_adequacy_investment_2425 %>%
  merge(., school_district_demographics_22, by.x = "school_district", by.y = "school_district") %>%
  merge(., annual_attendance %>% filter(year == 2020) %>% select(-school_district), by = "AUN") %>%
  merge(., urban_school_codes %>% select(-school_district), by = "AUN") %>%
  #filter(year == 2020) %>%
  select(AUN, school_district, geometry, weighted_students, aie_per_wadm,
         total_adequacy_investment, exp_per_adm, code) %>%
  unique() %>%
  mutate("investment_per_ws" = total_adequacy_investment / weighted_students,
         "difference" = ((investment_per_ws + exp_per_adm) - exp_per_adm) / exp_per_adm) %>%
  #drop_na(aie_per_wadm) %>%
  st_as_sf()

nb_school_districts <- poly2nb(investment_per_ws, queen = TRUE)
nbw_school_districts <- nb2listw(nb_school_districts, style = "W")

moran.test(investment_per_ws$difference, nbw_school_districts)

school_district_ids <- order(investment_per_ws$AUN)
investment_per_ws_localmoran <- localmoran(investment_per_ws$aie_per_wadm, nbw_school_districts)

printCoefmat(data.frame(investment_per_ws_localmoran[school_district_ids,],
                        row.names = investment_per_ws$AUN[school_district_ids],
                        check.names = FALSE)) %>%
  rownames_to_column() %>%
  merge(., investment_per_ws, by.x = "rowname", by.y = "AUN") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = Ii)) +
  scale_fill_steps()

ggplot(investment_per_ws) +
  geom_sf(aes(fill = difference)) +
  guides(fill = guide_legend(title = " ")) +
  labs(title = "Adequacy Investment per Weighted Student") +
  theme_void() 

ggplot(investment_per_ws %>% filter(school_district != "BRYN ATHYN SD"),
       aes(x = exp_per_adm, y = investment_per_ws, color = code)) +
  geom_point()

ggplot(investment_per_ws, aes(x = investment_per_ws, fill = code)) +
  geom_histogram() +
  labs(x = "Adequacy Investment per Weighted Student",
       y = "Count") +
  guides(fill = guide_legend(title = "Urban Code")) +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$"))
