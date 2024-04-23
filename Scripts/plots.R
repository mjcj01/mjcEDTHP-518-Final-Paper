library(tidyverse)
library(gt)
library(sf)

data_merge %>%
  as.data.frame() %>%
  group_by(code, year) %>%
  drop_na(aie_per_wadm) %>%
  summarise("median" = median(aie_per_wadm)) %>%
  rename(`Urban Code` = "code",
         Year = "year",
         `Median AIE per WADM` = "median") %>%
  ggplot(data = ., aes(x = Year, y = `Median AIE per WADM`, color = `Urban Code`)) +
  geom_point(size = 3) +
  geom_line(linewidth = 2) +
  theme_minimal() +
  labs(title = "Median AIE per WADM From 2014 - 2020") +
  scale_y_continuous(labels = scales::dollar_format(prefix="$"))

data_merge %>% 
  as.data.frame() %>% 
  select(year, total_state_rev, wadm, code) %>% 
  group_by(year, code) %>% 
  summarise(spending = median(total_state_rev) / median(wadm)) %>%
  ggplot(aes(x = year, y = spending, color = code)) + 
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  guides(color = guide_legend(title = "Urban Code")) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
  labs(x = "Year",
       y = "State Revenue Per WADM",
       title = "Median State Revenue per WADM From 2014 - 2020") +
  theme_minimal()

data_merge %>% 
  as.data.frame() %>% 
  select(year, total_local_rev, wadm, code) %>% 
  group_by(year, code) %>% 
  summarise(spending = median(total_local_rev) / median(wadm)) %>%
  ggplot(aes(x = year, y = spending, color = code)) + 
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  guides(color = guide_legend(title = "Urban Code")) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
  labs(x = "Year",
       y = "Local Revenue Per WADM",
       title = "Median Local Revenue per WADM From 2014 - 2020") +
  theme_minimal()

data_merge %>%
  as.data.frame() %>%
  group_by(code, year) %>%
  select(code, year, eq_mills) %>%
  drop_na(eq_mills) %>%
  summarise("median" = median(eq_mills)) %>%
  ggplot(aes(x = year, y = median, color = code)) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Year",
       y = "Median Property Tax Rate (in Mills)",
       title = "Property Tax Rate From 2014 - 2020") +
  guides(color = guide_legend(title = "Urban Code"))

attendance_2022 %>%
  merge(., shapiro_proposed_bef_funding_2425, by = "AUN") %>%
  merge(., urban_school_codes, by = "AUN") %>%
  group_by(code) %>%
  summarise("median_wadm" = median(wadm),
            "median_prop_bef" = median(total_proposed_bef),
            "median_base_bef" = median(base_bef_2324)) %>%
  mutate("Proposed BEF per WADM" = median_prop_bef / median_wadm,
         "2023-24 BEF per WADM" = median_base_bef / median_wadm,
         "% Difference" = paste(round(((`Proposed BEF per WADM` - `2023-24 BEF per WADM`) / `2023-24 BEF per WADM`) * 100,
                                      digits = 2), "%", sep = "")) %>%
  select(code, `Proposed BEF per WADM`, `2023-24 BEF per WADM`, `% Difference`) %>%
  rename(`Urban Code` = "code") %>%
  gt() %>%
  cols_align(align = "center") %>%
  opt_horizontal_padding(scale = 3) %>%
  opt_vertical_padding(scale = 1.25)

attendance_2022 %>%
  merge(., shapiro_proposed_bef_funding_2425, by = "AUN") %>%
  merge(., urban_school_codes, by = "AUN") %>%
  group_by(code) %>%
  summarise("median_wadm" = median(wadm),
            "median_prop_bef" = median(total_proposed_bef),
            "median_base_bef" = median(base_bef_2324)) %>%
  mutate("Proposed BEF per WADM" = median_prop_bef / median_wadm,
         "2023-24 BEF per WADM" = median_base_bef / median_wadm,
         "% Difference" = paste(round(((`Proposed BEF per WADM` - `2023-24 BEF per WADM`) / `2023-24 BEF per WADM`) * 100,
                                      digits = 2), "%", sep = "")) %>%
  select(code, `Proposed BEF per WADM`, `2023-24 BEF per WADM`, `% Difference`) %>%
  mutate("Proposed Increase" = `Proposed BEF per WADM` - `2023-24 BEF per WADM`) %>%
  pivot_longer(cols = c(`2023-24 BEF per WADM`, `Proposed Increase`), names_to = "category", values_to = "amount") %>%
  mutate("category" = factor(category, levels = c("Proposed Increase", "2023-24 BEF per WADM"))) %>%
  ggplot(aes(x =  code, y = amount, fill = category)) +
  geom_col()

ggplot(data = data_merge %>% filter(year == 2019)) +
  geom_sf(aes(fill = eq_mills)) +
  scale_fill_steps()

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, base_bef_2324, year, race_cat, enrollment) %>%
  filter(year == 2020 & race_cat == "Black" |
         year == 2020 & race_cat == "White" |
         year == 2020 & race_cat == "Hispanic" |
         year == 2020 & race_cat == "Asian" |
         year == 2020 & race_cat == "Total") %>%
  pivot_wider(names_from = "race_cat",
              values_from = "enrollment") %>%
  mutate("% White" = White / Total,
         "% Black" = Black / Total,
         "% Hispanic" = Hispanic / Total,
         "% Asian" = Asian / Total,
         "base" = base_bef_2324 / Total) %>%
  pivot_longer(cols = c(`% White`, `% Black`, `% Hispanic`, `% Asian`),
               names_to = "cat",
               values_to = "enrollment") %>%
  ggplot(data = ., aes(x = enrollment, y = base)) +
  geom_point() +
  labs(x = "% of Total Enrollment",
       y = "Equalized Property Tax Rate (in Mills)") +
  facet_wrap(vars(cat)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = "transparent"))

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, base_bef_2324, year, race_cat, enrollment) %>%
  filter(year == 2020 & race_cat == "Black" |
           year == 2020 & race_cat == "White" |
           year == 2020 & race_cat == "Hispanic" |
           year == 2020 & race_cat == "Asian" |
           year == 2020 & race_cat == "Total") %>%
  pivot_wider(names_from = "race_cat",
              values_from = "enrollment") %>%
  mutate("% White" = White / Total,
         "% Black" = Black / Total,
         "% Hispanic" = Hispanic / Total,
         "% Asian" = Asian / Total,
         "base" = base_bef_2324 / Total) %>%
  ggplot(aes(x = `% White`, y = `% Asian`)) +
  geom_point() +
  theme_minimal() +
  labs(x = "% of Students That Are White",
       y = "% of Students That Are Black")

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, total_proposed_bef, base_bef_2324, local_effort_per_house,
         exp_per_adm, eq_mills, year, wadm, race_cat, enrollment) %>%
  filter(year == 2020 & race_cat == "Black" |
           year == 2020 & race_cat == "White" |
           year == 2020 & race_cat == "Hispanic" |
           year == 2020 & race_cat == "Asian" |
           year == 2020 & race_cat == "Total") %>%
  pivot_wider(names_from = "race_cat",
              values_from = "enrollment") %>%
  mutate("% White" = White / Total,
         "% Black" = Black / Total,
         "% Hispanic" = Hispanic / Total,
         "% Asian" = Asian / Total,
         "base" = total_proposed_bef / Total,
         "change" = (total_proposed_bef - base_bef_2324) / base_bef_2324) %>%
  pivot_longer(cols = c(`% White`, `% Black`, `% Hispanic`, `% Asian`),
               names_to = "cat",
               values_to = "enrollment") %>%
  ggplot(data = ., aes(x = enrollment, y = eq_mills)) +
  geom_point(size = 1) +
  labs(x = "% of Total Enrollment",
       y = "Equalized Property Tax Rate (in Mills)") +
  facet_wrap(vars(cat), nrow = 1) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = "transparent"),
        axis.text.x = element_text(size = 5)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::percent) +
  geom_hline(yintercept = 18.55)
  

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, eq_mills, year) %>%
  unique() %>%
  pivot_wider(names_from = "year",
              values_from = "eq_mills") %>%
  select(AUN, school_district.y, `2014`, `2020`) %>%
  mutate("difference" = (`2020` - `2014`) / `2014`) %>%
  filter(!grepl("PHILADELPHIA", school_district.y)) %>%
  ggplot(aes(x = `2014`, y = `2020`)) +
  geom_point()

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, eq_mills, year, geometry) %>%
  unique() %>%
  pivot_wider(names_from = "year",
              values_from = "eq_mills") %>%
  select(AUN, school_district.y, geometry, `2014`, `2020`) %>%
  mutate("difference" = (`2020` - `2014`) / `2014`) %>%
  #filter(!grepl("PHILADELPHIA", school_district.y)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = difference))

data_merge %>%
  filter(year == 2020) %>%
  mutate(local_per_wadm = total_local_rev / wadm) %>%
  drop_na(eq_mills) %>%
  ggplot(aes(x = eq_mills, y = local_per_wadm, color = code)) +
  geom_vline(xintercept = 19.15629, linewidth = 1) +
  geom_hline(yintercept = 7562.362, linewidth = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Local Revenue per WADM in 2020 - 21",
       x = "Equalized Property Tax Rate (in Mills)",
       y = "Local Revenue per WADM") +
  guides(color = guide_legend(title = "Urban Code"))

data_merge %>%
  filter(year == 2020) %>%
  select(AUN, total_local_rev, poverty_0_99, poverty_100_184, local_effort_per_house, median_house_income,
         aie_per_wadm,exp_per_adm, geometry) %>%
  unique() %>%
  mutate(median_house_income = as.numeric(median_house_income)) %>%
  #group_by(local_effort_per_house) %>%
  #summarise(avg_spending = mean(aie_per_wadm)) %>%
  ggplot(aes(x = median_house_income, y = exp_per_adm)) +
  geom_point() +
  geom_vline(xintercept = 63714) +
  geom_hline(yintercept = 19176.54)

data_merge %>%
  filter(year == 2020) %>%
  select(AUN, total_local_rev, poverty_0_99, poverty_100_184, local_effort_per_house, median_house_income,
         aie_per_wadm,exp_per_adm, geometry, wadm) %>%
  unique() %>%
  mutate(median_house_income = as.numeric(median_house_income)) %>%
  mutate("spending_and_community" = ifelse(median_house_income >= 63714 & exp_per_adm >= 19176.54, "Wealthier & Higher Spending",
                                    ifelse(median_house_income >= 63714 & exp_per_adm < 19176.54, "Wealthier & Lower Spending",
                                    ifelse(median_house_income < 63714 & exp_per_adm >= 19176.54, "Poorer & Higher Spending",
                                    ifelse(median_house_income < 63714 & exp_per_adm < 19176.54, "Poorer & Lower Spending", NA)))),
         "community_wealth" = ifelse(median_house_income >= 63714, "Wealthier", "Poorer"),
         "spending" = ifelse(exp_per_adm >= 19176.54, "Higher Spending", "Lower Spending"),
         "local_per_wadm" = total_local_rev / wadm) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = median_house_income)) +
  scale_fill_steps2() +
  guides(fill = guide_legend(title = "Median House Income")) +
  theme_void()

data_merge %>%
  filter(year == 2020) %>%
  select(AUN, total_local_rev, poverty_0_99, poverty_100_184, local_effort_per_house, median_house_income,
         aie_per_wadm,exp_per_adm, geometry, wadm) %>%
  unique() %>%
  mutate(median_house_income = as.numeric(median_house_income)) %>%
  mutate("spending_and_community" = ifelse(median_house_income >= 63714 & exp_per_adm >= 19176.54, "Wealthier & Higher Spending",
                                           ifelse(median_house_income >= 63714 & exp_per_adm < 19176.54, "Wealthier & Lower Spending",
                                                  ifelse(median_house_income < 63714 & exp_per_adm >= 19176.54, "Poorer & Higher Spending",
                                                         ifelse(median_house_income < 63714 & exp_per_adm < 19176.54, "Poorer & Lower Spending", NA)))),
         "community_wealth" = ifelse(median_house_income >= 63714, "Wealthier", "Poorer"),
         "spending" = ifelse(exp_per_adm >= 19176.54, "Higher Spending", "Lower Spending"),
         "local_per_wadm" = total_local_rev / wadm) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = spending_and_community)) +
  #scale_fill_steps() +
  guides(fill = guide_legend(title = "Community Wealth and Spending")) +
  theme_void()

data_merge %>%
  filter(year == 2020) %>%
  select(AUN, code, total_local_rev, poverty_0_99, poverty_100_184, local_effort_per_house, median_house_income, eq_mills,
         aie_per_wadm, exp_per_adm, base_bef_2324, total_adequacy_investment, local_effort_per_house, geometry, wadm) %>%
  unique() %>%
  mutate(median_house_income = as.numeric(median_house_income)) %>%
  mutate("spending_and_community" = ifelse(median_house_income >= 63714 & exp_per_adm >= 19176.54, "Wealthier & Higher Spending",
                                    ifelse(median_house_income >= 63714 & exp_per_adm < 19176.54, "Wealthier & Lower Spending",
                                    ifelse(median_house_income < 63714 & exp_per_adm >= 19176.54, "Poorer & Higher Spending",
                                    ifelse(median_house_income < 63714 & exp_per_adm < 19176.54, "Poorer & Lower Spending", NA)))),
         "community_wealth" = ifelse(median_house_income >= 63714, "Wealthier", "Poorer"),
         "spending" = ifelse(exp_per_adm >= 19176.54, "Higher Spending", "Lower Spending"),
         "local_per_wadm" = total_local_rev / wadm,
         "income_per_mill" = median_house_income / eq_mills) %>%
  ggplot(aes(x = income_per_mill, y = base_bef_2324 / wadm, color = code)) +
  geom_point()
  


         