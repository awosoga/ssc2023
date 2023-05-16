library(tidyverse)
library(plm)
library(broom)
library(furrr)

data <- read_csv("complete_data.csv", show_col_types = F)

fixed_effects <- function(data) {
  plm(
    productivity ~ 
      mean_max_temp_c_diff + mean_min_temp_c_diff + mean_temp_c_diff + total_precip_mm_diff +
      total_rain_mm_diff + total_snow_cm_diff + snow_grnd_last_day_cm_diff + 
      extr_max_temp_c_diff + extr_min_temp_c_diff
    , data = data,
    index = c("GeoUID", "Date"),
    model = "within") %>% 
    lmtest::coeftest(vcov. = sandwich::vcovHC, type = "HC1")
}

# Economic Productivity Proportions

prop_data <- data %>% select(c(1,2,18, 23:36), ends_with("diff")) %>%
  mutate(GeoUID = as.factor(GeoUID), 
         Date = as.factor(Date)) %>%
  pivot_longer(cols = c(4:17), names_to = "industry", 
               values_to = "productivity", values_drop_na = T) %>%
  group_by(provincename, industry) %>% nest() %>% ungroup()

# Run Independent Models in Parallel and Store Their Results
plan(multisession, workers = as.integer(availableCores()))

prop_results_tidy <- prop_data %>% 
  mutate(fxe = future_map(data, fixed_effects)) %>% 
  mutate(fxe_results = map(fxe, tidy)) %>%
  unnest(cols = c(fxe_results)) %>%
  select(-c(data, fxe))

plan(sequential) 

prop_significant_results <- prop_results_tidy %>% filter(p.value < 0.05) 


