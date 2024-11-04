## Script to filter Bus Open Data Services GTFS timetables for GMCA (Greater Manchester) and LCR (Liverpool City Region) 
## Script to translate ATOC (rail services) timetables to GTFS format and filter for GMCA and LCR
# Setup libraries

library(UK2GTFS)
library(tidyverse)
library(gtfstools)
library(sf)
library(mapview)
library(here)

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

combined_area <- st_union(buffered_CA_boundary)


# BODS timetables filter for CA ---------------------------------
## BODS GTFS Data 
BODS <- gtfstools::read_gtfs("../Data/r5r_data/itm_all_gtfs.zip") # Download from BODS
# Filter BODS stops in CAs
BODS_CA <- filter_by_spatial_extent(BODS, buffered_CA_boundary)
gtfstools::write_gtfs(BODS_CA, "../Data/r5r_data/BODS_CA.gtfs.zip")

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
# indicates there's a problem with the stops data
# Check the stops data
stops <- ttis272$stops

# Remove invalid columns
stops <- stops %>% select(-easting, -northing, -platform_code, -stop_url)

# Check for NA values in latitude and longitude
stops <- stops %>% filter(!is.na(stop_lat) & !is.na(stop_lon))

na_columns <- colSums(is.na(stops)) > 0
print(na_columns)

# Update the GTFS stops data
ttis272$stops <- stops

## Force valid. This function does not fix problems, it just removes them
ttis272_gtfs <- UK2GTFS::gtfs_force_valid(ttis272)
UK2GTFS::gtfs_validate_internal(ttis272_gtfs)

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

UK2GTFS::gtfs_write(ttis272_gtfs, 
                    quote = TRUE, 
                    folder = "../Data/r5r_data", 
                    name = "rail.gtfs")
rail_gtfstools <- gtfstools::read_gtfs("../Data/r5r_data/rail.gtfs.zip")
rail_CA_gtfs <- filter_by_spatial_extent(rail_gtfstools, buffered_CA_boundary)
UK2GTFS::gtfs_validate_internal(rail_CA_gtfs)

gtfstools::write_gtfs(rail_CA_gtfs, "../Data/r5r_data/rail_CA.gtfs.zip")


latest_validator <- download_validator(tempdir())
path_output_dir <- tempfile("validation_from_path")
validate_gtfs("../Data/r5r_data/rail_CA.gtfs.zip", path_output_dir, latest_validator)

sf_rail <- convert_stops_to_sf(rail_CA_gtfs)
sf_BODS <- convert_stops_to_sf(BODS_CA)
mapview(sf_rail) + mapview(sf_BODS) + mapview(CA_boundary) 
