library(weathercan)
library(tidyverse)

# Download Climate Normals Inventory from Government of Canada Website
url <- "https://climate.weather.gc.ca/climate_normals/station_inventory_e.html?yr=1971"
download.file(url, "Canadian_Climate_Normals_1971_2000_station_inventory.csv")
climate_normals_1971 <- read_csv("Canadian_Climate_Normals_1971_2000_station_inventory.csv",
                                 show_col_types = F)

all_normals <- climate_normals_1971$CLIMATE_ID[-573] #Gives an Error

# Helper Function to Extract Climate Normals from Each Station
get_normals <- function(climate_id) {
  normals_dl(climate_ids = climate_id, normals_years = "1971-2000", quiet = TRUE) %>% 
  pull(normals) %>% 
  bind_rows() %>% 
  mutate(climate_id = climate_id)
}

# Select Variables of interest
normals_data <- suppressMessages(map(all_normals, get_normals) %>% bind_rows()) %>%
  transmute(period, 
                climate_id,
                mean_temp_c = temp_daily_average, 
                mean_max_temp_c = temp_daily_max,
                mean_min_temp_c = temp_daily_min,
                total_precip_mm = precip,
                snow_grnd_last_day = snow_depth_month_end,
                extreme_max_temp_c = temp_extreme_max,
                extreme_min_temp_c = temp_extreme_min,
                total_rain_mm = rain,
                total_snow_cm = snow
  )

# Join Climate and Station Information
climate_data_full <- climate_normals_1971 %>% 
  select(longitude_x = LONGITUDE, 
         latitude_y = LATITUDE, 
         climate_id = CLIMATE_ID,
         station_name = STATION_NAME,
         provincename = PROVINCE_OR_TERRITORY) %>%
  right_join(normals_data, by = "climate_id")


write_csv(climate_data_full, "climate_normals_canada.csv")


