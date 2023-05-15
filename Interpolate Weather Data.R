# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, sf, geojsonsf, lubridate, 
               canadianmaps, furrr, terra)

# Identify CSD's with Productivity Measurements
GeoUIDs <- list.files('Productivity per NAICS within region/', full.names = T) %>% 
  lapply(read_csv, show_col_types = FALSE) %>% bind_rows() %>%
  filter(!if_any(everything(), is.na), !rowSums(pick(c(3:16))) == 0) %>% 
  distinct(GeoUID) %>% pull()

# Read In Geographic CSD Data
geography_data_full <- list.files('geojson_files', full.names = T) %>% 
  lapply(geojson_sf) %>% 
  bind_rows() %>% filter(GeoUID %in% GeoUIDs)

# Read In Provincial Map Data
data("PROV", package = "canadianmaps")
PROV <- PROV

# Read In Climate Normals Data
climate_normals <- read_csv("climate_normals_canada.csv", show_col_types = F) %>%
  filter(period != "Year",
         !provincename %in% c("YUKON TERRITORY", "NORTHWEST TERRITORIES", "NUNAVUT")) %>%
  # The territories don't have productivity data available
  mutate(date_ym = as.Date(zoo::as.yearmon(paste0(period, "-2000"), format = "%b-%Y")),
         provincename = str_to_title(provincename),
         provincename = ifelse(provincename == "Newfoundland", 
                               "Newfoundland and Labrador", provincename)) %>% 
  select(-period) %>%  pivot_longer(cols = c(6:14), names_to = "variable", 
                                    values_to = "measurement", values_drop_na = T) %>%
  group_by(date_ym, provincename, variable) %>% nest() %>% rowwise() %>%
  transmute(
    dat = list(tibble(data, provincename, date_ym, variable))
  )  %>% ungroup() %>% select(data = dat)

provs <- unique(climate_normals$provincename)

# Read In Weather Data
weather_data <- read_csv(
  "climate_data/weather_Station_data.csv",show_col_types = FALSE) %>%
  mutate(date_ym = make_date(year = as.numeric(Year), month = as.numeric(Month))
  ) %>% janitor::clean_names() %>% select(-c(3, 5:7, 26, 28, ends_with("flag"))) %>% 
  filter(date_ym %within% interval("1998-01-01","2017-12-01")) %>% 
  pivot_longer(cols = c(4:12), names_to = "variable",
               values_to = "measurement", values_drop_na = T) %>%
  group_by(date_ym, variable) %>% nest() %>%
  cross_join(x = tibble(provincename = provs)) %>% 
  rowwise() %>%
  transmute(
    data = list(tibble(data, provincename, date_ym, variable))
  ) %>% ungroup()

# Define Interpolation Function
  interpolate_measurements <- function(data) {
    prov = data$provincename[1]
    mon = data$date_ym[1]
    var = data$variable[1]
    #print(paste(prov, mon, var))
    
    weather_vec <- data %>% select(-c(provincename, date_ym, variable)) %>%
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
plan(multisession, workers = as.integer(availableCores()))


weather <- weather_data %>% 
  transmute(estimates = future_map(data, interpolate_measurements)) %>%
          pull(estimates) %>% bind_rows()

plan(sequential)
write_csv(weather, "interpolated_weather_data.csv")

plan(multisession, workers = as.integer(availableCores()))
normals <-climate_normals %>% transmute(
  estimates = future_map(data, interpolate_measurements)) %>%
    pull(estimates) %>% bind_rows()

plan(sequential)
write_csv(normals, "interpolated_climate_normals.csv")

