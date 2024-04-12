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
  labs(title = "Median AIE per WADM From 2014 - 2020")

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
  gtsave("Figures and Tables//proposed_changes.rtf")

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
