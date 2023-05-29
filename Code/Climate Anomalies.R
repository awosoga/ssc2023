if(!require("weathercan")) 
  install.packages("weathercan", 
                   repos = c("https://ropensci.r-universe.dev", 
                             "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, weathercan)

# Download 21 Years Worth of Monthly Weather Data Readings (1997-2017)
station_info <- stations() %>% 
  filter(normals_1971_2000, 
         interval == "month", 
         !prov %in% c("YT", "NT", "NU"),
         end >= "1997-01-01") %>% pull(station_id)

station_data <- suppressMessages(weather_dl(station_ids = station_info, 
                           start = "1997-01-01", end = "2017-12-01",
                           interval = "month"))

# Download Climate Normals Data
climate_normals <- normals_dl(
  climate_ids = unique(station_data$climate_id), 
  normals_years = "1971-2000", quiet = TRUE, verbose = F) %>% 
  unnest(normals) %>% 
  filter(period != "Year") %>%
  mutate(period = as.character(period),
    month = month(zoo::as.yearmon(paste0(period, "-2000"), format = "%b-%Y")) )%>%
  select(month,
        climate_id,
        mean_temp_normal = temp_daily_average, 
        mean_max_temp_normal = temp_daily_max,
        mean_min_temp_normal = temp_daily_min,
        total_precip_normal = precip,
        snow_grnd_last_day_normal = snow_depth_month_end,
        extreme_max_temp_normal = temp_extreme_max,
        extreme_min_temp_normal = temp_extreme_min,
        total_rain_normal = rain,
        total_snow_normal = snow
        #dir_max_gust = wind_max_gust_dir,
        #spd_max_gust = wind_max_gust
  ) 

# Compute Climate Anomalies as difference between Observed and Normal
climate_anomalies <- station_data %>% 
  select(date, prov, climate_id, lat, lon, 
         extr_max_temp, extr_min_temp,mean_max_temp, mean_min_temp, mean_temp,
         snow_grnd_last_day, total_precip, total_rain, total_snow) %>% 
  mutate(month = month(date)) %>%
  left_join(climate_normals, by = c("climate_id", "month")) %>%
  mutate(
    mean_max_temp_anomaly = mean_max_temp - mean_max_temp_normal,
    mean_min_temp_anomaly = mean_min_temp - mean_min_temp_normal,
    mean_temp_anomaly = mean_temp - mean_temp_normal,
    extr_max_temp_anomaly = extr_max_temp - extreme_max_temp_normal,
    extr_min_temp_anomaly = extr_min_temp - extreme_min_temp_normal,
    total_precip_anomaly = total_precip - total_precip_normal,
    total_rain_anomaly = total_rain - total_rain_normal,
    total_snow_anomaly = total_snow - total_snow_normal,
    snow_grnd_last_day_anomaly = snow_grnd_last_day - snow_grnd_last_day_normal
  )


# Save File
write_csv(climate_anomalies, "climate_anomalies.csv")
