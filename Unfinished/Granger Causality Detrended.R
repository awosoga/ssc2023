if (!require("pacman")) install.packages("pacman")
suppressPackageStartupMessages(pacman::p_load(tidyverse, furrr))

# Load Data
productivity_data <- read_csv("productivity_data_stl.csv", show_col_types = F)
climate_data <- read_csv("climate_data_stl.csv", show_col_types = F)
full_data <- cbind(productivity_data, climate_data)

test_df <- full_data %>% select(c(18, starts_with("random"))) %>%
    pivot_longer(!c(GeoUID, ends_with("anomaly")), 
                 names_to = "variable", 
                 values_to = "measurement",
                 values_drop_na = T) %>%
    group_by(GeoUID, variable) %>%
    nest() %>% ungroup()

suppress_output <- function(output){
  sink("/dev/null") #or NUL for windows machines
  tmp = output
  sink()
  return(tmp)
}

compute_granger_causality <- function(data) {
  ts <- ts(data, start = c(1997,1), frequency = 12)
  lags = vars::VARselect(ts)$selection[1] %>% as.integer()
  vm = vars::VAR(ts, p = lags)
  gc = suppress_output(bruceR::granger_causality(vm))
  return(gc$result %>% filter(Causality == "measurement <= ALL") %>% select(p.F, p.Chisq))
}

plan(multisession, workers = as.integer(availableCores()))
results <- test_df %>% mutate(
  p_values = future_map(data, compute_granger_causality)
) %>% unnest(p_values) %>% select(-data) %>% mutate(
  significant_f = if_else(p.F < 0.05, 1, 0),
  significant_chisq = if_else(p.Chisq < 0.05, 1, 0),
  is_prop = ifelse(str_detect(variable, "proportion"), 1,0)
) %>% group_by(is_prop) %>% 
  summarise(f = sum(significant_f), chisq = sum(significant_chisq))

write_csv(results, "results_detrended.csv")
