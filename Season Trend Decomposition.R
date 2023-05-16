library(tidyverse)
library(furrr)
library(doParallel)
library(foreach)
library(lubridate)

# Import Weather Data
full_data <- read_csv("full_data.csv", show_col_types = F)

# Define Function to Compute Time Series
compute_time_series <- function(data) {
  ts(data$measurement, frequency = 12, start = c(1998,1)) %>%
    decompose()
}

# Compute Time Series for each Climate Variable in Parallel
plan(multisession, workers = as.integer(availableCores()))
weather_portion <- full_data[,c(18,38:56)] %>%
  pivot_longer(cols = c(2:20), names_to = "variable", values_to = "measurement") %>%
  group_by(GeoUID, variable) %>%
  nest() %>% ungroup()
time_series <- weather_portion %>%
  mutate(model = future_map(data, compute_time_series)) %>% pull(model)

plan(sequential)


# Register Parallel Backend
registerDoParallel(cores = as.integer(availableCores()))

# Define Component Extraction Function
get_components <- function(i, j) {
  cbind.data.frame( 
    Date = seq(ymd("1998-01-01"), ymd("2017-12-01"), by = 'months'),
    GeoUID = unique(weather_portion$GeoUID)[i], 
    variable = unique(weather_portion$variable)[j], 
    seasonal = (time_series[[i*j]][2] %>% bind_rows() %>% pull() %>% as.numeric()),
    trend = (time_series[[i*j]][3] %>% bind_rows()) %>% pull() %>% as.numeric()),
    random = (time_series[[i*j]][4] %>% bind_rows() %>% pull() %>% as.numeric())
  )
}

# Run Component Extraction Function in Parallel
decomposition <- 
  foreach(i = seq_along(unique(weather_portion$GeoUID)), .combine = 'rbind',
          .packages = c('lubridate', 'dplyr')) %:%
    foreach(j = seq_along(unique(weather_portion$variable)), 
            .combine = 'rbind', .packages = c('lubridate', 'dplyr')) %dopar% {
      get_components(i,j)
    }

# Pivot the Data from Long Format to Panel Format
detrended_data <- decomposition %>% 
  pivot_wider(names_from = "variable", values_from = c(seasonal, trend, random))

complete_data <- cbind(full_data, detrended_data[,-c(1,2)])

# Save Final Data Set
write_csv(complete_data, "complete_data.csv")
