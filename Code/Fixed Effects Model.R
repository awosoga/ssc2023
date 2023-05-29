run_model <- function(numCores = 1) {

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, plm, broom, furrr)

full_data <- read_csv("full_data.csv", show_col_types = F)

fixed_effects <- function(data) {
  plm(
    response ~ 
      mean_max_temp_anomaly + mean_min_temp_anomaly + mean_temp_anomaly + 
      total_precip_anomaly +
      total_rain_anomaly + total_snow_anomaly + snow_grnd_last_day_anomaly + 
      extr_max_temp_anomaly + extr_min_temp_anomaly + co2_anomaly
    , data = data,
    index = c("GeoUID", "Date"),
    model = "within")
}

parameter_estimates <- function(model) {
  (model %>% lmtest::coeftest(vcov. = sandwich::vcovHC, type = "HC1")) %>% tidy
}


# Model 2: Productivity Proportion ~ Climate Anomalies (By Province)

productivity_proportion_data_province <- full_data %>% 
  select(Date, provincename, GeoUID,
        starts_with("x"), ends_with("anomaly")) %>%
  mutate(GeoUID = as.factor(GeoUID), 
         Date = as.factor(Date),
         across(ends_with("anomaly"), scales::rescale)
         ) %>%
  pivot_longer(cols = c(4:17), names_to = "industry", 
               values_to = "response", values_drop_na = T) %>%
  group_by(provincename, industry) %>% nest() %>% ungroup()

if (numCores <= as.integer(availableCores()))
  plan(multisession, workers = numCores)
else
  plan(multisession, workers = as.integer(availableCores()))

productivity_proportion_results_province <- productivity_proportion_data_province %>% 
  mutate(fxe = future_map(data, fixed_effects),
         f_stat = map(fxe, function(model) summary(model)$fstatistic$statistic),
         r_squared = map(fxe, r.squared),
         fxe_results = map(fxe, parameter_estimates)) %>%
  unnest(cols = c(f_stat, r_squared, fxe_results)) %>%
  select(-c(data, fxe))

plan(sequential)

write_csv(productivity_proportion_results_province, 
          "productivity_proportion_results_province_scaled.csv")
}