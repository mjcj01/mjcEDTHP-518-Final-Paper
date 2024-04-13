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
  gt()

ggplot(data = data_merge %>% filter(year == 2019)) +
  geom_sf(aes(fill = eq_mills)) +
  scale_fill_steps()

data_merge %>%
  as.data.frame() %>%
  select(AUN, school_district.y, eq_mills, year, geometry) %>%
  pivot_wider(names_from = "year",
              values_from = "eq_mills") %>%
  select(AUN, school_district.y, geometry, `2014`, `2020`) %>%
  mutate("difference" = (`2020` - `2014`) / `2014`) %>%
  filter(!grepl("PHILADELPHIA", school_district.y)) %>%
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

         