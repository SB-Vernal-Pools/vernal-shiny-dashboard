# Packages 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)
library(leaflet)
library(shinyWidgets)
library(leaflet.extras)
#library(leaflet.markercluster)
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
library(BAMMtools)  # for getJenksBreaks function

## ==============================
##         Compile CSS       ----
## ==============================

sass(
  input = sass_file(here("www", "sass-styles.scss")),
  output = here("www", "sass-styles.css"),
  
  # OPTIONAL, but speeds up page load time by removing white-space & line-breaks that make css files more human-readable
  options = sass_options(output_style = "compressed"))

## ==============================
##         Read in Data      ----
## ==============================

# store data file path
data_path <- here("data")


#vernal pools polygons
vernal_polygon <- st_read(here(data_path, "vernal_pools_monitored", "vernal_pools_monitored.shp")) %>% 
  rename(location = locatin,
         location_pool_id = lctn_p_) %>% 
  st_make_valid()

# hydrology data
hydro <- read_csv(here(data_path, "hydro_data.csv")) 



# abiotic data
abiotic <- read_csv(here(data_path, "pc_abiotic_data.csv")) 


# invert species list
invert_species <- read_csv(here(data_path, "invert_species.csv"))

## ==============================
##      Data  Prepping    ----
## ==============================

# cleaning percent cover labels for images
percent_cover <- read_csv(here(data_path, "percent_cover_data.csv")) 
percent_cover$species <- gsub("_", " ", percent_cover$species)


summarized_abiotic <- abiotic %>%
  group_by(location_pool_id) %>% #created column
  summarise(across(where(is.numeric), mean, na.rm = TRUE))



vernal_polygon_abiotic <- vernal_polygon %>%
  left_join(summarized_abiotic, by = c("location_pool_id" = "location_pool_id"))


vernal_polygon_abiotic <- st_make_valid(vernal_polygon_abiotic)


# adding month to abiotic
vernal_polygon_abiotic <- vernal_polygon_abiotic %>%
  mutate(month = month(gps_dat, label = TRUE, abbr = FALSE),
         year = year(gps_dat),
         research_conducted_status = ifelse(is.na(site_area_m2), "Non-Active Monitoring", "Active Monitoring")) %>% 
  mutate(year = case_when(year == 1899 ~ 2020,
                          TRUE ~ 2019))

jenks_breaks <- getJenksBreaks(vernal_polygon_abiotic$pool_area_m2, k = 5)  # Adjust k for desired number of categories

# Create labels for the intervals
jenks_labels <- paste(
  round(jenks_breaks[-length(jenks_breaks)], 2),
  "-",
  round(jenks_breaks[-1], 2),
  "sq m"
)

# Calculate centroids for clustering
centroids <- st_centroid(vernal_polygon_abiotic)
