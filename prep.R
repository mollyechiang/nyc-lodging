library(broom)
library(janitor)
library(sf)
library(tidyverse)

raw_zillow <- 
  read_csv("http://files.zillowstatic.com/research/public/Neighborhood/Neighborhood_Zhvi_Summary_AllHomes.csv", col_types = 
             cols(
               Date = col_date(format = ""),
               RegionID = col_double(),
               RegionName = col_character(),
               State = col_character(),
               Metro = col_character(),
               County = col_character(),
               City = col_character(),
               SizeRank = col_double(),
               Zhvi = col_double(),
               MoM = col_double(),
               QoQ = col_double(),
               YoY = col_double(),
               `5Year` = col_double(),
               `10Year` = col_double(),
               PeakMonth = col_character(),
               PeakQuarter = col_character(),
               PeakZHVI = col_double(),
               PctFallFromPeak = col_double(),
               LastTimeAtCurrZHVI = col_character()
             )) %>%
  clean_names() %>% 
  filter(state == "NY",city == "New York") %>%
  select(region_name, zhvi)

raw_airbnb <-
  read_csv("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-09-12/visualisations/listings.csv", col_types = 
             cols(
               id = col_double(),
               name = col_character(),
               host_id = col_double(),
               host_name = col_character(),
               neighbourhood_group = col_character(),
               neighbourhood = col_character(),
               latitude = col_double(),
               longitude = col_double(),
               room_type = col_character(),
               price = col_double(),
               minimum_nights = col_double(),
               number_of_reviews = col_double(),
               last_review = col_date(format = ""),
               reviews_per_month = col_double(),
               calculated_host_listings_count = col_double(),
               availability_365 = col_double()
             )) %>%
  select(id, 
         name, 
         neighbourhood_group, 
         neighbourhood, 
         latitude, 
         longitude, 
         price, 
         host_id, 
         minimum_nights, 
         number_of_reviews, 
         availability_365)



nyc_shapes <- st_read("http://data.insideairbnb.com/united-states/ny/new-york-city/2019-09-12/visualisations/neighbourhoods.geojson")
nyc_boroughs <- st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")



nyc_data <- raw_airbnb %>%
  full_join(raw_zillow, by = c("neighbourhood" = "region_name")) 

median_ppn_data <- nyc_data %>% 
  group_by(neighbourhood, zhvi, neighbourhood_group) %>% 
  summarize(median_ppn = median(price))

nyc_shapes_full <- nyc_shapes %>%
  full_join(median_ppn_data, by = c("neighbourhood", "neighbourhood_group"))




regression <- median_ppn_data %>% 
  ungroup(zhvi) %>%
  mutate(zhvi = zhvi/1000) %>%
  drop_na() %>%
  group_by(neighbourhood_group) %>%
  nest() %>%
  mutate(models = map(data, ~lm(data = .x, formula = median_ppn ~ zhvi))) %>%
  mutate(values = map(models, ~coef(.x))) %>%
  mutate(slope = map_dbl(values, ~pluck(.x, "zhvi"))) 

stats <- nyc_boroughs %>%
  inner_join(regression, by = c("boro_name" = "neighbourhood_group")) %>%
  select(boro_name, slope)



write_rds(nyc_data, "nyc-lodging-shiny/nyc_data.rds")
write_rds(median_ppn_data, "nyc-lodging-shiny/ppn_nyc_data.rds")
write_rds(nyc_shapes_full, "nyc-lodging-shiny/nyc_shapes_clean.rds")
write_rds(stats, "nyc-lodging-shiny/nyc_statistics.rds")


