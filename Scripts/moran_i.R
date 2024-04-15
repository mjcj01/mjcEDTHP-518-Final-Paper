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
investment_per_ws_localmoran <- localmoran(investment_per_ws$difference, nbw_school_districts)

printCoefmat(data.frame(investment_per_ws_localmoran[school_district_ids,],
                        row.names = investment_per_ws$AUN[school_district_ids],
                        check.names = FALSE)) %>%
  rownames_to_column() %>%
  merge(., investment_per_ws, by.x = "rowname", by.y = "AUN") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = Ii)) +
  scale_fill_stepsn(colors = c("blue", "green"))

ggplot(investment_per_ws) +
  geom_sf(aes(fill = difference)) +
  guides(fill = guide_legend(title = " ")) +
  labs(title = "% Increase in per Student Expenditures") +
  scale_fill_continuous(labels = scales::label_percent()) +
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



spending_and_community_df <- data_merge %>%
  filter(year == 2020) %>%
  select(AUN, total_local_rev, poverty_0_99, poverty_100_184, local_effort_per_house, median_house_income,
         aie_per_wadm,exp_per_adm, geometry) %>%
  unique() %>%
  mutate(median_house_income = as.numeric(median_house_income)) %>%
  mutate("spending_and_community" = ifelse(median_house_income >= 63714 & exp_per_adm >= 19176.54, "Wealthier & Higher Spending",
                                           ifelse(median_house_income >= 63714 & exp_per_adm < 19176.54, "Wealthier & Lower Spending",
                                                  ifelse(median_house_income < 63714 & exp_per_adm >= 19176.54, "Poorer & Higher Spending",
                                                         ifelse(median_house_income < 63714 & exp_per_adm < 19176.54, "Poorer & Lower Spending", NA)))),
         "community_wealth" = ifelse(median_house_income >= 63714, "Wealthier", "Poorer"),
         "spending" = ifelse(exp_per_adm >= 19176.54, "Higher Spending", "Lower Spending")) %>%
  drop_na(exp_per_adm) %>%
  st_as_sf()

nb_spending_and_community <- poly2nb(spending_and_community_df, queen = TRUE)
nbw_spending_and_community <- nb2listw(nb_spending_and_community, style = "W")

moran.test(spending_and_community_df$median_house_income, nbw_spending_and_community)
moran.test(spending_and_community_df$exp_per_adm, nbw_spending_and_community)
