# Packages 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)
library(leaflet)
library(shinyWidgets)
library(leaflet.extras)
library(googleway)
library(htmlwidgets)
library(htmltools)
library(fontawesome)
library(sass)
library(sf)
library(here)
library(plotly)
library(lubridate)
library(tidylog)
library(janitor)
library(snakecase)

## ==============================
##         Compile CSS       ----
## ==============================

sass(
  input = sass_file(here("www", "sass-styles.scss")),
  output = here("www", "sass-styles.css"),
  
  # OPTIONAL, but speeds up page load time by removing white-space & line-breaks that make css files more human-readable
  options = sass_options(output_style = "compressed"))

## =========================================================
##                        Read in Data                  ----
## =========================================================

# store data file path
data_path <- here("data")

# .................. vernal pools polygons .................
vernal_polygon <- st_read(here(data_path, "vernal_pools_monitored", "vernal_pools_monitored.shp")) %>% 
  rename(location = locatin,
         location_pool_id = lctn_p_) %>% 
  st_make_valid()

# ................... hydrology data .......................
hydro <- read_csv(here(data_path, "hydro_data.csv")) 


# ...................... abiotic data ......................
abiotic <- read_csv(here(data_path, "pc_abiotic_data.csv")) 


# ..................... invertebrate spp ...................
invert_species <- read_csv(here(data_path, "invert_species.csv"))

# ...................... percent cover ...................
# read in & combine percent cover data
percent_cover_2019 <- read_csv(here(data_path, "percent_cover_data.csv")) %>% 
  select(monitoring_date, transect_distance_of_quadrat, 
         percent_bare_ground, percent_natural_thatch,
         count_of_native_species, sum_of_native_cover, 
         sum_of_non_native_cover, count_of_non_native_species,
         type, species, percent_cover, location_pool_id, 
         vernal_pool_axis, spp_cal_flora)

percent_cover_2024 <- read_csv(here(data_path, "percent_cover_2024.csv")) %>% 
  select(monitoring_date, transect_distance_of_quadrat, 
         percent_bare_ground, percent_natural_thatch,
         count_of_native_species, sum_of_native_cover, 
         sum_of_non_native_cover, count_of_non_native_species,
         type, species, percent_cover, location_pool_id, 
         vernal_pool_axis, spp_cal_flora)

percent_cover <- rbind(percent_cover_2019, percent_cover_2024) %>% 
  filter(vernal_pool_axis %in% c("Major", "Minor"))

rm(percent_cover_2019, percent_cover_2024)

## =========================================================
##                       Data  Prepping                 ----
## =========================================================

# ...................... percent cover ...................
# remove underscore from species names
percent_cover$species <- gsub("_", " ", percent_cover$species)

# ....................... abiotic .......................
# calculate mean
summarized_abiotic <- abiotic %>%
  group_by(location_pool_id) %>% #created column
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# join to gemoetries & make valid
vernal_polygon_abiotic <- vernal_polygon %>%
  left_join(summarized_abiotic, by = c("location_pool_id" = "location_pool_id")) %>% 
  st_make_valid()

# adding month to abiotic
vernal_polygon_abiotic <- vernal_polygon_abiotic %>%
  mutate(month = month(gps_dat, label = TRUE, abbr = FALSE),
         year = year(gps_dat),
         research_conducted_status = ifelse(is.na(site_area_m2), "Non-Active Monitoring", "Active Monitoring")) %>% 
  mutate(year = case_when(year == 1899 ~ 2020,
                          TRUE ~ 2019))

# calculate centroids for clustering map feature
centroids <- st_centroid(vernal_polygon_abiotic)