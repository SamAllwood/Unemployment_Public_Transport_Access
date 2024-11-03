## Script to filter Bus Open Data Services GTFS timetables for GMCA (Greater Manchester) and LCR (Liverpool City Region) 
## Script to translate ATOC (rail services) timetables to GTFS format and filter for GMCA and LCR

# Setup libraries
library(tidytransit)
library(UK2GTFS)
library(tidyverse)
library(gtfstools)
library(sf)
library(mapview)
library(here)

setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access")

# 2. Load and Filter Datasets ------------------------------------------------------------
# Combined Authority Boundary + buffer
Boundaries <- read_sf("../Data/CAUTH_DEC_2023_EN_BFC.shp")
CA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester" | 
                                       CAUTH23NM =="Liverpool City Region" | 
                                       CAUTH23NM =="West Yorkshire") %>%
  st_transform(4326) 
#mapview(CA_boundary)
CA_bound_small_buffer <- CA_boundary %>% st_buffer(dist=25)
# Create a 20km buffer around the GMCA boundary line
buffered_CA_boundary <- st_buffer(CA_boundary, dist = 20000) %>% 
  st_transform(4326)

# LCR (Liverpool) Boundary + buffer
#LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
#  st_transform(4326) 
#LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)
## Create a 20km buffer around the LCR boundary line
#buffered_LCR_boundary <- st_buffer(LCR_boundary, dist = 20000) %>% 
#  st_transform(4326)

# Combined Boundary
#Joint_boundary <- st_union(buffered_GMCA_boundary, buffered_LCR_boundary)

# BODS timetables filter for CA ---------------------------------
## BODS GTFS Data 
BODS <- tidytransit::read_gtfs("../Data/r5r_data/itm_all_gtfs.zip") # Download from BODS
# Filter BODS stops in CAs
BODS_CA <- filter_feed_by_area(BODS, buffered_CA_boundary)
tidytransit::write_gtfs(BODS_CA, "../Data/r5r_data/BODS_CA.gtfs.zip")

# Transform ATOC (rail data) ----------------------------------------------
# Uses UK2GTFS V. ‘0.1.1’
# More info: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html
# remotes::install_github("ITSleeds/UK2GTFS")
# Data source: https://data.atoc.org/?q=user

# Detect number of cores
n_cores <- parallel::detectCores() -1

# Transform ATOC to GTFS
path_in <- "../Data/r5r_data/ttis272.zip" # Also too large for github but available at the link above
locations <- "../Data/tiplocs-merged.csv"
ttis272 <- atoc2gtfs(path_in = path_in, locations = locations, silent = FALSE, shapes = TRUE, ncores = n_cores)

# Check internal validity
UK2GTFS::gtfs_validate_internal(ttis272)

## Force valid. This function does not fix problems, it just removes them
ttis272_gtfs <- UK2GTFS::gtfs_force_valid(ttis272)

## Compare original and valid
# Find difference
map2(ttis272, ttis272_gtfs, identical)
# Stops not included in GTFS version
gtfs_diff <- anti_join(ttis272$stop_times, ttis272_gtfs$stop_times)
gtfs_diff
# Stops missing
unique(gtfs_diff$stop_id)
# Frequency
count(gtfs_diff, stop_id)
# Trips affected
unique(gtfs_diff$trip_id)

# Filter stops in CA
stops <- st_as_sf(ttis272_gtfs$stops, remove = FALSE, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_CA <- stops[CA_boundary,]

# Filter other GTFS components based on filtered stops
filtered_trips <- ttis272_gtfs$stop_times %>%
  filter(stop_id %in% stops_CA$stop_id) %>%
  dplyr::select(trip_id) %>%
  distinct()

filtered_stop_times <- ttis272_gtfs$stop_times %>%
  filter(stop_id %in% stops_CA$stop_id)

filtered_trips_data <- ttis272_gtfs$trips %>%
  filter(trip_id %in% filtered_trips$trip_id)

filtered_routes <- ttis272_gtfs$routes %>%
  filter(route_id %in% filtered_trips_data$route_id)

stops_CA <- stops_CA %>%
  as.data.frame() %>%
  dplyr::select(-c(easting, northing, geometry)) 


# Create the filtered GTFS list
ttis_CA <- list(
  agency = ttis272_gtfs$agency,
  stops = stops_CA,
  routes = filtered_routes,
  trips = filtered_trips_data,
  stop_times = filtered_stop_times,
  calendar = ttis272_gtfs$calendar,
  calendar_dates = ttis272_gtfs$calendar_dates,
  transfers = ttis272_gtfs$transfers
)

# filter directly from original GTFS version
ttis_CA_GTFS_1 <- filter_feed_by_area(ttis272_gtfs, buffered_CA_boundary)
ttis_CA_gtfs <- UK2GTFS::gtfs_force_valid(ttis_CA)
map2(ttis_CA, ttis_CA_gtfs, identical)

## Write as GTFS - writes a zip folder
UK2GTFS::gtfs_write(ttis_CA_gtfs, 
                    quote = TRUE, 
                    folder = "../Data/r5r_data", 
                    name = "rail_CA.gtfs")

# Clean env. to free memory
rm(list = ls())
gc(reset = TRUE)

# Load GMCA / LCR rail GTFS in tidytransit
CA_rail_timetables <- tidytransit::read_gtfs("../Data/r5r_data/rail_CA.gtfs.zip")

# Mapview the stations (note, the routes do not show on mapview but the stops will)
sf <- gtfs_as_sf(GMCA_LCR_rail_timetables)
mapview(sf$stops) + 
  mapview(sf$shapes)+
  mapview(Joint_boundary, CRS=4382)
