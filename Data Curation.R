library(tidyverse)
library(magrittr)
library(lubridate)

# Read Weather Data

weather <- read_csv("interpolated_weather_data.csv", show_col_types = F) %>%
  pivot_wider(names_from = "var", values_from = "measurement") %>%
  mutate(Date = mon, mon = month(mon))

# Read in Productivity Data

prod_data_full <- list.files('Productivity per NAICS within region/', full.names = T) %>% 
  lapply(read_csv, show_col_types = FALSE) %>% bind_rows() %>%
  rename_with(~gsub("production_in_division_","", .x)) %>% mutate(
    total = rowSums(pick(c(3:16))),
    across(c(3:16), ~.x/total, .names = "{.col}_proportion")
  ) 

# Get 20 Year Subset of Valid data

prod_data_subset <- prod_data_full %>% filter(
  !if_any(everything(), is.na),
  Date %within% interval("1998-01-01", "2017-12-01")
) 

# Read in Climate Normals

normals <- read_csv("interpolated_climate_normals.csv", show_col_types = F) %>%
  pivot_wider(names_from = "var", values_from = "measurement") %>% 
    rename_with(~paste0(.x, "_normal"), c(3:11)) %>% mutate(mon = month(mon))

# Get Carbon Dioxide Data

co2 <- read_csv("CO2.csv", show_col_types = F) %>% 
  select(-c(2,3)) %>%
  mutate(across(-1, as.numeric)) %>%
  pivot_longer(c(-1), names_to = "month", values_to = "co2_concentration") %>%
  mutate(month = str_remove(month, " \\(parts per million\\)"),
         Date = as.Date(as.yearmon(paste0(month, `Year/Month`), format = "%B%Y"))) %>%
  select(Date, co2_concentration)

# Perform Consecutive Left Joins

full_data <- prod_data_subset %>% 
  left_join(weather, by = c("GeoUID", "Date")) %>%
  left_join(normals, by = c("GeoUID", "mon")) %>% 
  left_join(co2, by = "Date")

# Compute Anomalies

full_data %<>% mutate(
  mean_max_temp_c_diff = mean_max_temp_c - mean_max_temp_c_normal,
  mean_min_temp_c_diff = mean_min_temp_c - mean_min_temp_c_normal,
  mean_temp_c_diff = mean_temp_c - mean_temp_c_normal,
  extr_max_temp_c_diff = extr_max_temp_c - extreme_max_temp_c_normal,
  extr_min_temp_c_diff = extr_min_temp_c - extreme_min_temp_c_normal,
  total_precip_mm_diff = total_precip_mm - total_precip_mm_normal,
  total_rain_mm_diff = total_rain_mm - total_rain_mm_normal,
  total_snow_cm_diff = total_snow_cm - total_snow_cm_normal,
  snow_grnd_last_day_cm_diff = snow_grnd_last_day_cm - snow_grnd_last_day_normal,
)

write_csv(full_data, "full_data.csv")

