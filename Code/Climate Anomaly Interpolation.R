interpolate_anomalies <- function(numCores = 1) {

# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, sf, geojsonsf, lubridate, 
               canadianmaps, furrr, terra)

# Read In Provincial Map Data
data("PROV", package = "canadianmaps")
PROV <- PROV

# Identify CSD's with Productivity Measurements
GeoUIDs <- list.files('Productivity per NAICS within region/', full.names = T) %>% 
  lapply(read_csv, show_col_types = FALSE) %>% bind_rows() %>%
  filter(!if_any(everything(), is.na), !rowSums(.[3:16]) == 0) %>% 
  distinct(GeoUID) %>% pull()

# Read In Geographic CSD Data and Add Province Abbreviations
geography_data_full <- list.files('geojson_files', full.names = T) %>% 
  lapply(geojson_sf) %>% 
  bind_rows() %>% filter(GeoUID %in% GeoUIDs)

# Load and Transform Climate Anomaly Data to Appropriate Format
climate_anomalies <- read_csv("climate_anomalies.csv", show_col_types = F) %>%  
  select(lon, lat, date, ends_with("anomaly")) %>%
  pivot_longer(cols = ends_with("anomaly"), names_to = "variable", 
                                    values_to = "measurement", values_drop_na = T) %>%
  group_by(date, variable) %>% nest() %>% 
  cross_join(x = tibble(provincename = unique(geography_data_full$provincename))) %>% 
  rowwise() %>%
  transmute(
    data = list(tibble(data, provincename, date, variable))
  ) %>% ungroup()

# Define Interpolation Function
  interpolate_measurements <- function(data) {
    prov = data$provincename[1]
    mon = data$date[1]
    var = data$variable[1]
    print(paste(prov, mon, var)) # can be removed 
    
    weather_vec <- data %>% select(-c(provincename, date, variable)) %>%
      st_as_sf(coords = 1:2, crs = 4269) %>% st_transform(crs = 3857) %>% vect()

    v <- voronoi(weather_vec)
    
    geography_vec <- PROV %>% filter(PRENAME == prov) %>%
      st_transform(crs = 3857) %>% st_buffer(dist = 0) %>% vect()
    
    province_csd <- geography_data_full %>% filter(provincename == prov) %>%
      st_transform(crs = 3857)
    
    vca <- crop(v, geography_vec)
    r <- rast(vca, res=5000)
    d <- data.frame(geom(weather_vec)[,c("x", "y")], as.data.frame(weather_vec))
    
    idm <- gstat::gstat(formula = measurement~1, locations = ~x+y, data = d)
    idp <- interpolate(r, idm, debug.level = 0) %>% mask(geography_vec)
    
    pts <- st_as_sf(as.points(idp[[1]])) 
    
    geography_vec %<>% st_as_sf()
    province_csd %<>% mutate(measurement = aggregate(pts, province_csd, mean)$var1.pred)
    
    balanced_weather_data <- rbind(
      province_csd %>% filter(!is.na(measurement)) %>% rbind(
        province_csd %>% filter(is.na(measurement)) %>%
          st_join(
            y = pts,
            join = st_nearest_feature
          ) %>% mutate(measurement = var1.pred) %>% select(-var1.pred)
      ) %>% mutate(mon, var) %>% select(GeoUID, measurement, mon, var)
    )
    return(list(balanced_weather_data %>% st_drop_geometry()))
  }
  
#Run the Interpolation in Parallel
  if (numCores <= as.integer(availableCores()))
    plan(multisession, workers = numCores)
  else
    plan(multisession, workers = as.integer(availableCores()))

interpolated_anomalies <- climate_anomalies %>% 
  mutate(estimates = future_map(data, interpolate_measurements), 
         .keep = "none") %>%
          pull(estimates) %>% bind_rows()

plan(sequential)

write_csv(interpolated_anomalies, "interpolated_climate_anomalies.csv")
}