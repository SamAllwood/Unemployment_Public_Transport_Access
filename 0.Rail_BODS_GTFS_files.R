# Setup
library(tidytransit)
library(UK2GTFS)
library(tidyverse)
library(gtfstools)
library(sf)
library(mapview)
library(here)

setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# 2. Load and Filter Datasets ------------------------------------------------------------

# GMCA (Greater Manchester) Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)
# Create a 20km buffer around the GMCA boundary line
buffered_GMCA_boundary <- st_buffer(GMCA_boundary, dist = 20000) %>% 
  st_transform(4326)

# LCR (Liverpool) Boundary + buffer
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)
# Create a 20km buffer around the LCR boundary line
buffered_LCR_boundary <- st_buffer(LCR_boundary, dist = 20000) %>% 
  st_transform(4326)

# Joint Boundary
Joint_boundary <- st_union(buffered_GMCA_boundary, buffered_LCR_boundary)

# BODS --------------------------------------------------------------------
## BODS GTFS Data 
BODS <- tidytransit::read_gtfs("Data/GTFS_Data/itm_all_gtfs 2.zip")
# Filter BODS stops in GMCA
BODS_GMCA <- filter_feed_by_area(BODS, buffered_GMCA_boundary)
tidytransit::write_gtfs(BODS_GMCA, "Data/GTFS_Data/r5r/BODS_MANCH.gtfs.zip")

# Filter BODS stops in LCR
BODS_LCR <- filter_feed_by_area(BODS, buffered_LCR_boundary)
tidytransit::write_gtfs(BODS_LCR, "Data/GTFS_Data/r5r/BODS_LCR.gtfs.zip")


# Transform ATOC (rail data) ----------------------------------------------
# Uses UK2GTFS V. ‘0.1.1’
# More info: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html
# remotes::install_github("ITSleeds/UK2GTFS")

# Source: https://data.atoc.org/?q=user

# Detect number of cores
n_cores <- parallel::detectCores() -1

# Transform ATOC to GTFS
path_in <- "Data/GTFS_Data/ttis209.zip"
locations <- "Data/GTFS_Data/tiplocs-merged.csv"
ttis209 <- atoc2gtfs(path_in = path_in, locations = locations, silent = FALSE, shapes = TRUE, ncores = n_cores)

# Check internal validity
UK2GTFS::gtfs_validate_internal(ttis209)

## Force valid. This function does not fix problems, it just removes them
ttis209_gtfs <- UK2GTFS::gtfs_force_valid(ttis209)

## Compare original and valid
# Find difference
map2(ttis209, ttis209_gtfs, identical)
# Stops not included in GTFS version
gtfs_diff <- anti_join(ttis209$stop_times, ttis209_gtfs$stop_times)
gtfs_diff
# Stops missing
unique(gtfs_diff$stop_id)
# Frequency
count(gtfs_diff, stop_id)
# Trips affected
unique(gtfs_diff$trip_id)


# Filter stops in LCR and GMCA
stops <- st_as_sf(ttis209_gtfs$stops, remove = FALSE, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_LCR_GMCA <- stops[Joint_boundary,]

# Filter other GTFS components based on filtered stops
filtered_trips <- ttis209_gtfs$stop_times %>%
  filter(stop_id %in% stops_LCR_GMCA$stop_id) %>%
  dplyr::select(trip_id) %>%
  distinct()

filtered_stop_times <- ttis209_gtfs$stop_times %>%
  filter(stop_id %in% stops_LCR_GMCA$stop_id)

filtered_trips_data <- ttis209_gtfs$trips %>%
  filter(trip_id %in% filtered_trips$trip_id)

filtered_routes <- ttis209_gtfs$routes %>%
  filter(route_id %in% filtered_trips_data$route_id)

stops_LCR_GMCA <- stops_LCR_GMCA %>%
  as.data.frame() %>%
  dplyr::select(-c(easting, northing, geometry)) 


# Create the filtered GTFS list
ttis_LCR_GMCA <- list(
  agency = ttis209_gtfs$agency,
  stops = stops_LCR_GMCA,
  routes = filtered_routes,
  trips = filtered_trips_data,
  stop_times = filtered_stop_times,
  calendar = ttis209_gtfs$calendar,
  calendar_dates = ttis209_gtfs$calendar_dates,
  transfers = ttis209_gtfs$transfers
)

ttis_LCR_GMCA_gtfs <- UK2GTFS::gtfs_force_valid(ttis_LCR_GMCA)
map2(ttis_LCR_GMCA, ttis_LCR_GMCA_gtfs, identical)


## Write as GTFS - writes a zip folder
UK2GTFS::gtfs_write(ttis_LCR_GMCA_gtfs, 
                    quote = TRUE, 
                    folder = "Data/GTFS_Data/r5r", 
                    name = "rail_GMCA_LCR.gtfs")

# Clean env. to free memory
rm(list = ls())
gc(reset = TRUE)

# Load GMCA / LCR rail GTFS in tidytransit
GMCA_LCR_rail_timetables <- tidytransit::read_gtfs("Data/GTFS_Data/r5r/rail_GMCA_LCR.gtfs.zip")

# Mapview the stations
sf <- gtfs_as_sf(GMCA_LCR_rail_timetables)
mapview(sf$stops) + 
  mapview(sf$shapes)+
  mapview(Joint_boundary, CRS=4382)



