if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, lubridate)

# Load Anomaly Data
anomaly_data <- read_csv("interpolated_climate_anomalies.csv", show_col_types = F) %>%
  pivot_wider(names_from = "var", values_from = "measurement") %>%
  mutate(Date = mon, mon = month(mon))

# Load Productivity Data and get 21 Year Subset of Valid data
productivity_data <- list.files('Productivity per NAICS within region/', full.names = T) %>% 
  lapply(read_csv, show_col_types = FALSE) %>% bind_rows() %>%
  rename_with(~gsub("production_in_division_","", .x)) %>% mutate(
    total = rowSums(pick((3:16))),
    across(c(3:16), ~.x/total),
    #growth_rate =  xts::diff.xts(log(total+(1e-5)), 1) * 100
  ) %>% filter(
    !if_any(everything(), is.na),
    Date %within% interval("1997-02-01", "2017-12-01")
  )

# NEW: Add CO2 anomalies
link <- "https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/ghg-concentrations/2021/1-carbon-dioxide-concentration-en.csv"
co2 <- read_csv(link, show_col_types = F, skip = 2) %>% 
  select(-c(2,3)) %>%
  mutate(across(-1, as.numeric)) %>%
  pivot_longer(c(-1), names_to = "month", values_to = "co2_concentration") %>%
  mutate(month = str_remove(month, " \\(parts per million\\)"),
         Date = zoo::as.Date(zoo::as.yearmon(paste0(month, `Year/Month`), format = "%B%Y"))) %>%
  select(Date, co2_concentration) 

co2_normal <- co2 %>% filter(Date %within% interval("1971-01-01", "2000-12-01")) %>%
  summarise(normal = mean(co2_concentration, na.rm = T)) %>% pull()

co2_anomaly <- co2 %>% filter(Date %within% interval("1997-02-01", "2017-12-01")) %>%
  mutate(co2_anomaly = co2_concentration - co2_normal) %>% select(-co2_concentration)

# Perform Consecutive Left Joins to Create Full Data Set
full_data <- productivity_data %>% 
  left_join(anomaly_data, by = c("GeoUID", "Date")) %>%
  left_join(co2_anomaly, by = "Date")

write_csv(full_data, "full_data.csv")

