## Script to calculate public transport job accessibility for GMCA

# 1. Setup libraries ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(tidytransit)
library(sf)
library(r5r)
library(osmextract)
library(mapview)
library(ggmap)
library(accessibility)
library(lwgeom)
library(patchwork)
options(java.parameters = "-Xmx8G")
options(timeout = 1000)

# 2. Load and Filter Datasets ------------------------------------------------------------

# GMCA Boundary + buffer
Boundaries <- read_sf("Data/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)

# Create a 20km buffer around the GMCA boundary line
buffered_GMCA_boundary <- st_buffer(GMCA_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("Data/LAD_DEC_2021_GB_BFC.shp") %>% # too large for github but available from ONS Geoportal
  st_transform(4326)
# Filter LADs within GMCA
LADs_MANCH <- LADs %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# Read LSOA population-weighted centroids
lsoas <- st_read("Data/LSOA_PopCentroids_EW_2021_V3.shp") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  rename(id = LSOA21CD) 

# Filter LSOA PW-centroids
lsoa_PWC_within_GMCA <- lsoas %>% 
  filter(as.vector(st_within(., GMCA_boundary, sparse = FALSE))) 
lsoa_PWC_within_GMCA_buffer <- lsoas %>% 
  filter(as.vector(st_within(., buffered_GMCA_boundary, sparse = FALSE))) 

# Read LSOA boundaries (too large for github but available on ONS Geoportal)
lsoa_boundaries <- st_read("Data/LSOA_2021_EW_BFC_V8.shp") %>% 
  st_transform(4326) %>%
  st_make_valid() 
# Calculate LSOA area
lsoa_boundaries$LSOA_area <- st_area(lsoa_boundaries$geometry)
lsoa_boundaries$LSOA_area_km2 <- lsoa_boundaries$LSOA_area/1000000

# Filter LSOA boundaries for GMCA
lsoa_boundaries_within_GMCA_buffer <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                                buffered_GMCA_boundary, 
                                                                sparse = FALSE), ]
lsoa_boundaries_within_GMCA <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                         GMCA_bound_small_buffer, 
                                                         sparse = FALSE), ]
# Read Towns and City boundaries
towns <- st_read("Data/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) %>%
  st_make_valid()
towns_within_GMCA_buffer <- towns[st_within(towns, buffered_GMCA_boundary, sparse = FALSE), ]

# Towns and City centroids
towns_centroids <- st_centroid(towns_within_GMCA_buffer) %>%
  rename(id = TCITY15CD) %>% st_transform(4326)
# Geometric centroids are in unrepresentative places, so manually identify town/city centres
towns_manual_MAN <- data.frame(
  id = c("Manchester", "Salford", "Stockport", "Oldham", "Rochdale", "Bury", "Bolton", "Wigan"),
  lon = c(-2.243185, -2.277002, -2.161260, -2.112658, -2.153749, -2.297645, -2.429588, -2.630668), # Updated longitudes
  lat = c(53.478214, 53.485645, 53.406539, 53.540754, 53.611260, 53.592658, 53.578226, 53.545255)  # Updated latitudes
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) # convert directly to sf

# Adjust LSOAs within town boundaries - manual corrections to tidy up intersections where town boundaries and LAD boundaries 
# overlap
towns_with_buffer <- towns_within_GMCA_buffer %>% st_buffer(dist=400)
lsoa_with_town_info <- st_join(lsoa_boundaries_within_GMCA, towns_with_buffer, join = st_within)
lsoa_with_town_info <- lsoa_with_town_info %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005160", "Manchester", TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005100", "Manchester", TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005057", NA, TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005056", NA, TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005055", NA, TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005058", NA, TCITY15NM)) %>%
  mutate(TCITY15NM = ifelse(LSOA21CD == "E01032558", NA, TCITY15NM))
  
lsoa_with_town_info$townsuburb <- ifelse(is.na(lsoa_with_town_info$TCITY15NM), "Suburb", "Urban") 
lsoa_with_town_info$TCITY15NM <- ifelse(is.na(lsoa_with_town_info$TCITY15NM), "Suburb", lsoa_with_town_info$TCITY15NM)
lsoa_with_town_info_min <- lsoa_with_town_info %>%  
  select(LSOA21CD, 
         LSOA21NM, 
         geometry, 
         LSOA_area_km2, 
         townsuburb, 
         TCITY15NM) %>% 
  as.data.frame() %>% 
  distinct(LSOA21CD, .keep_all=TRUE)

# Employed Residents
Employment <-read_csv("Data/census2021-ts066/census2021-ts066-lsoa.csv") %>%
  dplyr::select("geography",
                "geography code",
                "Economic activity status: Economically active (excluding full-time students)") %>%
  rename("LSOA_name" = "geography",
         "id" = "geography code",
         "Economically_active" = "Economic activity status: Economically active (excluding full-time students)")
 
# Manchester City Centre - St. Peter's Square
MAN_CC <- 
  data.frame(id = "MAN_CC", lat = 53.478298, lon = -2.243281) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
MAN_CC_point <- 
  geom_sf(data = MAN_CC, shape = 21, fill = 'white', size = 2)


# 3. OpenStreetMap ---------------------------------------------------------------
# PBF road and pedestrian network for England - necessary to 
# get all of transit network activity and avoid boundary issues.
# File is large, only need to download once so including the url and code for reference
# england_pbf <- "https://download.geofabrik.de/europe/united-kingdom/england-latest.osm.pbf"
# file.remove("./r5r/geofabrik_england-latest.osm.pbf")
# oe_download(file_url = england_pbf, download_directory = "./r5r")


# 4. R5R Network Setup -------------------------------------------------------
# Get input files
input_files <- list.files("Data/GTFS_Data", 
  recursive = TRUE,
  pattern = 'gtfs\\.zip$|pbf$',
  full.names = TRUE)
# List input files
input_files

# Load and validate BODS GTFS data - not necessary every time but for diagnostics in case of issues
# BODS_MANCH <- read_gtfs("BODS_MANCH.gtfs.zip")
# BODS_manch_valid <- tidytransit::validate_gtfs(BODS_MANCH, warnings = TRUE)
# view(BODS_manch_valid)

# Read multi-modal network
gc() # this setup step requires quite a bit of memory, so best to gc first
r5r_core <- setup_r5(data_path = "Data/GTFS_Data", 
                     verbose=TRUE,
                     overwrite = FALSE) 
summary(r5r_core)
network <- transit_network_to_sf(r5r_core) 

#Check transit network visuals - only for diagnostics and also shapefile doesn't show rail network
#network <- transit_network_to_sf(r5r_core) 
#network$routes <- network$routes %>% st_transform(4326) %>% filter(short_name==356)
#network$stops <- network$stops %>% st_transform(4326) 
#network$routes <- network$routes %>% filter(mode == "TRAM")

# Travel Time Matrix Calculation Parameters ------------------------------------------------------
# Time and date of departure
departure_datetime <- as.POSIXct("2024-05-21 08:00:00") 
mode = c( "TRANSIT", "WALK") # note function always includes 'WALK' anyway, and 'Transit' includes rail, bus, train and tram
walk_speed = 4.32 # from the literature
# max_duration = 200L # including a max duration leads to NA's where the max duration is exceeded so I removed it
max_walk_time = 30L

MAN_TTM_CC <- 
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = MAN_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  rename("Traveltime_CC" = "travel_time_p50") %>%
  select(-"to_id")

# Extended travel time matrix shows more, takes longer, but helpful for diagnostics
MAN_ETTM_CC <-
  expanded_travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = MAN_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 

# Travel time to employment centres (centre points of major towns and cities) ----------------------
MAN_TTM_Emp <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 500,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 
# Calculate the closest employment centre for each LSOA
closest_emp_centre_LSOA <- MAN_TTM_Emp %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Travel Time to employment centres - comparisons --------------------------------------------
MAN_comp_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Jobcentre Plus Proximity ------------------------------------------------
# https://www.gov.uk/government/publications/dwp-jobcentre-register
# Jobcentre_plus <- read_csv("../Jobcentre_locations.csv") # Jobcentre locations 2019
# Geolocate the postcodes - don't need to do this every time as it uses Google API so takes a few mins
# Jobcentre_plus <- Jobcentre_plus %>% mutate_geocode(Postcode, source="google", output="latlon", key="AIzaSyC7Ld1-mUEUP9ONWlipSAuHcjWaMjZS2nQ")
# Filter out discontinued geocodes
# Jobcentre_plus <- Jobcentre_plus %>% filter(!is.na(lon) & !is.na(lat))
# write_csv(Jobcentre_plus, "../Jobcentre_locations_geocoded.csv")
Jobcentre_plus_geocoded <- read_csv("Data/Jobcentre_locations_geocoded.csv")
# Convert job centres to simple feature 
Jobcentre_plus_geocoded <- Jobcentre_plus_geocoded %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
# Filter jobcentres within buffered GMCA boundary
jobcentres_within_GMCA_buffer <- Jobcentre_plus_geocoded %>% 
  filter(as.vector(st_within(., buffered_GMCA_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = "NOMIS Office Code") 
jobcentres_within_GMCA <-  Jobcentre_plus_geocoded %>% 
  filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = "NOMIS Office Code") 

# calculate travel time from each LSOA to all job centres
jobcentre_proximity <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = jobcentres_within_GMCA_buffer, 
    mode = c("WALK","TRANSIT"),
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 100,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# Get the minimum travel time for each LSOA
closest_jobcentre_by_LSOA <- jobcentre_proximity %>%
  group_by(from_id) %>%
  summarize(closest_jobcentre = min(travel_time_p50, na.rm = TRUE))
# Join jobcentre travel time to LSOAs dataframe
MANCH_TT_Jobcentre <- left_join(lsoa_with_town_info_min, closest_jobcentre_by_LSOA, by = c("LSOA21CD" = "from_id"))


# Travel Time Matrix - all-to-all ------------------------------------------
MANCH_ttm <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = lsoa_PWC_within_GMCA_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

MANCH_ttm_bus <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = lsoa_PWC_within_GMCA_buffer, 
    mode = c("BUS", "WALK"),
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

MANCH_ttm_tram <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = lsoa_PWC_within_GMCA_buffer, 
    mode = "TRAM",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

MANCH_ttm_walk <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = lsoa_PWC_within_GMCA_buffer, 
    mode =  "WALK",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# Business Register and Employment Survey (BRES) --------------------------------------------------------------------
# Load employment data from BRES. Note BRES uses 2011 LSOA codes, so lookup for 2021 conversion required
BRES <- read_csv("Data/Business_reg_Emp_Surv(BRES)2021.csv", skip = 8) %>% 
  separate("...1", into = c("LSOA11CD", "LSOA11NM"), sep = " : ") %>%
  rename("Employed" = "...2") %>%
  dplyr::select(-c("...3", "...4","...5")) %>%
  na.omit()
# Load 2011-2021 LSOA lookup table
LSOA_lookup <- read_csv("Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv")

# Lookup correct LSOA codes
# left join will duplicate 2011 employment numbers if LSOA is merged in 2021. 
# Group by id and summing employment numbers to get total employment in 2021 LSOA
BRES$Employed <- as.numeric(BRES$Employed)
BRES_2021 <- BRES %>% left_join(LSOA_lookup, by = "LSOA11CD", keep=FALSE) %>%
  dplyr::select(-c("LSOA11CD", "LSOA11NM.x", "LSOA11NM.y", "LAD22NMW", "ObjectId")) %>%
  rename("id" = "LSOA21CD")
BRES_2021_corrected <- BRES_2021 %>%
  group_by(id) %>%
  summarise(Employed = sum(Employed)) %>%
  ungroup() %>%
  mutate(Employed = as.numeric(Employed))

# Public Transport Job Accessibility Index --------------------------------
job_access <- gravity(
  travel_matrix=MANCH_ttm,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index" = "Employed")

job_access_bus <- gravity(
  travel_matrix=MANCH_ttm_bus,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Bus" = "Employed")

job_access_tram <- gravity(
  travel_matrix=MANCH_ttm_tram,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Tram" = "Employed")

job_access_walk <- gravity(
  travel_matrix=MANCH_ttm_walk,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Walk" = "Employed")

# Demand-Potential Adjusted Public Transport Job Accessibility Index --------------------------------
# Travel Time Matrix for whole buffer as demand may emanate from outside GMCA boundary
MANCH_ttm_buffer <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA_buffer, 
    destinations = lsoa_PWC_within_GMCA_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# Demand potential of LSOAs - needs to be based on full buffer TTM
demand_potential <- gravity(
  travel_matrix = MANCH_ttm_buffer,
  travel_cost="travel_time_p50",
  land_use_data=Employment,
  opportunity="Economically_active",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("Demand_potential" = "Economically_active") %>%
  left_join(BRES_2021_corrected, by = "id") %>%
  mutate(Jobs_over_demand_potential = Employed/Demand_potential) %>%
  as_tibble()

# PTJA adjusted for demand potential of population decayed according to travel time to location
job_access_demand <- gravity(
  travel_matrix=MANCH_ttm,
  travel_cost="travel_time_p50",
  land_use_data=demand_potential,
  opportunity="Jobs_over_demand_potential",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_demand" = "Jobs_over_demand_potential")

# Join datasets to LSOA boundaries for export
MANCH_dataset <- MANCH_TT_Jobcentre %>% 
  left_join(job_access, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_bus, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_tram, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_walk, by = c("LSOA21CD"="id")) %>%
  left_join(BRES_2021_corrected, by = c("LSOA21CD"="id")) %>%
  rename("Travel_time_Job_Centre" = "closest_jobcentre") %>%
  left_join(closest_emp_centre_LSOA, by = c("LSOA21CD"="from_id")) %>%
  rename("Travel_time_Emp_Centre" = "closest_empcentre") %>%
  left_join(MAN_TTM_CC, by = c("LSOA21CD"="from_id")) %>%
  left_join(job_access_demand, by = c("LSOA21CD"="id")) 

# Write datasets to shapefiles
st_write(MANCH_dataset, "Data/MANCH_dataset.shp", append=FALSE)
st_write(towns_manual_MAN, "Data/towns_centroids.shp", append=FALSE)
