if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, foreach, furrr, doParallel)

# Load Full
full_data <- read_csv("full_data.csv", show_col_types = F)

# Define Function to Compute Time Series
compute_time_series <- function(data) {
  ts(data$measurement, frequency = 12, start = c(1997,1)) %>%
    decompose()
}

# library(vars)
# library(bruceR)
# data(Canada)
# VARselect(Canada)
# vm = VAR(Canada, p=3)
# model_summary(vm)
# granger_causality(vm)
#Compute Time Series for each Climate Anomaly in Parallel
climate_portion <- full_data %>% select(GeoUID, ends_with("anomaly")) %>%
  pivot_longer(-1, names_to = "variable", values_to = "measurement") %>%
  group_by(GeoUID, variable) %>%
  nest() %>% ungroup()
plan(multisession, workers = as.integer(availableCores()))
time_series <- climate_portion %>% 
  filter(GeoUID %in% unique(climate_portion$GeoUID)) %>%
  mutate(model = future_map(data, compute_time_series)) %>% pull(model)
plan(sequential)

# # Compute Time Series for each Productivity Measure in Parallel
# productivity_portion <- full_data %>% select(GeoUID, starts_with("X")) %>%
#   pivot_longer(-1, names_to = "variable", values_to = "measurement") %>%
#   group_by(GeoUID, variable) %>%
#   nest() %>% ungroup() 
# plan(multisession, workers = as.integer(availableCores()))
# time_series <- productivity_portion %>%
#   filter(GeoUID %in% unique(climate_portion$GeoUID)[1:3]) %>%
#   mutate(model = future_map(data, compute_time_series)) %>% pull(model)
# plan(sequential)

# Register Parallel Backend
registerDoParallel(cores = as.integer(availableCores()))

# Define Component Extraction Function
get_components <- function(i, j) {
  cbind.data.frame( 
    Date = seq(ymd("1997-01-01"), ymd("2017-12-01"), by = 'months'),
    GeoUID = unique(climate_portion$GeoUID)[i], 
    variable = unique(climate_portion$variable)[j], 
    seasonal = (time_series[[i*j]][2] %>% bind_rows() %>% pull() %>% as.numeric()),
    trend = (time_series[[i*j]][3] %>% bind_rows()) %>% pull() %>% as.numeric(),
    random = (time_series[[i*j]][4] %>% bind_rows() %>% pull() %>% as.numeric())
  )
}

# Run Component Extraction Function in Parallel
decomposition <- 
  foreach(i = seq_along(unique(climate_portion$GeoUID)), .combine = 'rbind',
          .packages = c('lubridate', 'dplyr')) %:%
    foreach(j = seq_along(unique(climate_portion$variable)), 
            .combine = 'rbind', .packages = c('lubridate', 'dplyr')) %dopar% {
      get_components(i,j)
    }

# Pivot the Data from Long Format to Panel Format
detrended_data <- decomposition %>% 
  pivot_wider(names_from = "variable", values_from = c(seasonal, trend, random))

# Save Final Data Set
write_csv(detrended_data, "climate_data_stl.csv")