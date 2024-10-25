# Script to create the travel time matrix for West Yorkshire Combined Authority (WYCA) using the R5r package
# 1. Setup ----------------------------------------------------------------
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
options(scipen=999)

# 2. Load and Filter Datasets ------------------------------------------------------------
setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access/WYCA")

# WYCA Boundary + buffer
Boundaries <- read_sf("../../Data/CAUTH_DEC_2023_EN_BFC.shp")
WYCA_boundary <- Boundaries %>% filter(CAUTH23NM == "West Yorkshire") %>%
  st_transform(4326) 
WYCA_bound_small_buffer <- WYCA_boundary %>% st_buffer(dist=25)

# Create a 20km buffer around the WYCA boundary line
buffered_WYCA_boundary <- st_buffer(WYCA_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("../../Data/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within WYCA
LADs_WYCA <- LADs %>% filter(as.vector(st_within(., WYCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)
# mapview(LADs_WYCA)+mapview(WYCA_boundary)

# Read LSOA pop-weighted centroids
lsoas <- st_read("../../Data/LSOA_PopCentroids_EW_2021_V3.shp") %>%
  st_transform(4326)
# Filter LSOA PW-centroids
lsoas <- st_make_valid(lsoas)
lsoa_PWC_within_WYCA <- lsoas %>% 
  filter(as.vector(st_within(., WYCA_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
lsoa_PWC_within_WYCA_buffer <- lsoas %>% 
  filter(as.vector(st_within(., buffered_WYCA_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
# transform CRS to WGS84
lsoa_PWC_within_WYCA_buffer$geometry <- st_transform(lsoa_PWC_within_WYCA_buffer$geometry, 4326)
lsoa_PWC_within_WYCA$geometry <- st_transform(lsoa_PWC_within_WYCA$geometry, 4326)

#Read LSOA boundaries
lsoa_boundaries <- st_read("../../Data/LSOA_2021_EW_BFC_V8.shp") %>%
  st_transform(4326)
lsoa_boundaries$geometry <- st_make_valid(lsoa_boundaries$geometry)

# Filter LSOA boundaries for WYCA buffer
#lsoa_boundaries <- st_make_valid(lsoa_boundaries)
lsoa_boundaries_within_WYCA_buffer <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                                buffered_WYCA_boundary, 
                                                                sparse = FALSE), ]
lsoa_boundaries_within_WYCA <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                         WYCA_bound_small_buffer, 
                                                         sparse = FALSE), ]
lsoa_boundaries_within_WYCA_buffer$LSOA_area <- st_area(lsoa_boundaries_within_WYCA_buffer$geometry)
lsoa_boundaries_within_WYCA_buffer$LSOA_area_km2 <- lsoa_boundaries_within_WYCA_buffer$LSOA_area/1000000
lsoa_boundaries_within_WYCA$LSOA_area <- st_area(lsoa_boundaries_within_WYCA$geometry)
lsoa_boundaries_within_WYCA$LSOA_area_km2 <- lsoa_boundaries_within_WYCA$LSOA_area/1000000
# Read Towns and City boundaries
towns <- st_read("../../Data/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_WYCA_buffer <- towns[st_within(towns, buffered_WYCA_boundary, sparse = FALSE), ]
towns_within_WYCA <- towns[st_within(towns, WYCA_bound_small_buffer, sparse = FALSE), ]

towns_manual_WYCA <- data.frame( 
  id = c("Barnsley", "Bradford", "Burnley", "Bury", "Doncaster", "Halifax", "Harrogate", "Huddersfield", "Leeds", "Oldham"),
  lon = c(-1.479237, -1.756356, -2.245021, -2.297645, -1.131974, -1.859140, -1.537945, -1.782488, -1.545146, -2.112658), 
  lat = c(53.553464, 53.794762, 53.789720, 53.592658, 53.524176, 53.723237,53.996504, 53.646861, 53.796904, 53.540754),
  buffer = c("Y", "N", "Y", "Y", "Y", "N", "Y", "N", "N", "Y")
) 

# Convert the dataframe to an sf object, assuming WGS 84 (EPSG: 4326) coordinate reference system
towns_manual_WYCA <- st_as_sf(towns_manual_WYCA, coords = c("lon", "lat"), crs = 4326)
#mapview(towns_manual_WYCA)+
#  mapview(towns_within_WYCA_buffer)+
#  mapview(WYCA_boundary)+
#  mapview(buffered_WYCA_boundary) +
#  mapview(towns_centroids)

# Label LSOAs within town boundaries
towns_with_buffer <- towns_within_WYCA_buffer %>% st_buffer(dist=25)
lsoa_with_town_info <- st_join(lsoa_boundaries_within_WYCA, towns_with_buffer, join = st_within)
# code for moving LSOAs to different town:
#lsoa_with_town_info <- lsoa_with_town_info %>%
#  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005160", "Manchester", TCITY15NM)) %>%
#  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005100", "Manchester", TCITY15NM)) %>%
#  mutate(TCITY15NM = ifelse(LSOA21CD == "E01005057", NA, TCITY15NM))

#create townsuburb variable
lsoa_with_town_info$townsuburb <- ifelse(is.na(lsoa_with_town_info$TCITY15NM), "Suburb", "Urban") 
# populate rest of TCITY15NM with suburb if no city assigned
lsoa_with_town_info$TCITY15NM <- ifelse(is.na(lsoa_with_town_info$TCITY15NM), "Suburb", lsoa_with_town_info$TCITY15NM)
# simplify dataframe
lsoa_with_town_info_min <- lsoa_with_town_info %>%  select(LSOA21CD, 
                                                           LSOA21NM, 
                                                           geometry, 
                                                           LSOA_area_km2, 
                                                           townsuburb, 
                                                           TCITY15NM,
                                                           geometry) %>% 
  as.data.frame() %>% 
  distinct(LSOA21CD, .keep_all=TRUE)

#mapview(towns_within_WYCA_buffer)+mapview(WYCA_boundary)+mapview(towns_centroids)+mapview(towns_manual_WYCA)
#mapview(lsoa_with_town_info)+ mapview(towns_with_buffer)

# Liverpool City Centre - LOCATION not super-important as not used for any calculation. Arbitrary city centre point
Leeds_CC <- 
  data.frame(id = "Leeds_CC", lat = 53.796904, lon = -1.545146) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
Leeds_CC_point <- 
  geom_sf(data = Leeds_CC, shape = 21, fill = 'white', size = 2)
mapview(Leeds_CC)
# Employed Residents
Employment <-read_csv("../../Data/census2021-ts066-lsoa.csv") %>%
  dplyr::select("geography",
                "geography code",
                "Economic activity status: Economically active (excluding full-time students)") %>%
  rename("LSOA_name" = "geography",
         "id" = "geography code",
         "Economically_active" = "Economic activity status: Economically active (excluding full-time students)")


# Load and validate BODS GTFS data - for diagnostics only
#BODS_WYCA <- read_gtfs("Data/GTFS_Data/r5r/BODS_WYCA.gtfs.zip")
#BODS_WYCA_valid <- tidytransit::validate_gtfs(BODS_WYCA, warnings = TRUE)
#view(BODS_manch_valid)


# 3. OpenStreetMap ---------------------------------------------------------------
# PBF road and pedestrian network for England - necessary to 
# get all of transit network activity and avoid boundary issues.
# Makes running transport model slow, consider downloading smaller maps section
# Only need to download once so including the url for reference
#england_pbf <- "https://download.geofabrik.de/europe/united-kingdom/england-latest.osm.pbf"
#file.remove("./r5r/geofabrik_england-latest.osm.pbf")
#oe_download(file_url = england_pbf, download_directory = "./r5r")

# 4. R5R Network Setup -------------------------------------------------------
# Get input files - will load BODS_MANCH and BODS_WYCA - consider simplifying if
# too slow.
input_files <- list.files("../../Data/r5r_data", 
  recursive = TRUE,
  pattern = 'gtfs\\.zip$|pbf$',
  full.names = TRUE)
# List input files
input_files


# Read multi-modal network
gc() #this setup step requires quite a bit of memory, so best to gc first
options(java.parameters = "-Xmx8G")
r5r_WYCA <- setup_r5(data_path = "../../Data/r5r_data", 
                     verbose=TRUE,
                     overwrite = FALSE) # make sure you leave this to run after it indicates it's finished

summary(r5r_WYCA)
print(r5r_WYCA)
#network <- transit_network_to_sf(r5r_WYCA) 
#mapview(network)
#Check transit network visuals - only for diagnostics and also shapefile won't show rail network
#network <- transit_network_to_sf(r5r_WYCA) 
#network$routes <- network$routes %>% st_transform(4326) %>% filter(short_name==356)
#network$stops <- network$stops %>% st_transform(4326) 
#network$routes <- network$routes %>% filter(mode == "TRAM")

# Transit Calculation Parameters ------------------------------------------------------
# Time and date of departure
departure_datetime <- as.POSIXct("2024-05-21 08:00:00") 
mode = c( "TRANSIT") # note function always includes 'WALK' anyway
walk_speed = 4.32
max_duration = 2000L
max_walk_time = 30L

WYCA_TT_CC <- 
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = WYCA_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  rename("Traveltime_CC" = "travel_time_p50",
         "LSOA21CD" = "from_id") %>%
  select(-"to_id")

WYCA_Emp_ettm <-
  expanded_travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = WYCA_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 50,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 

# Travel time to employment centres ---------------------------------------
WYCA_Emp_ttm <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = towns_manual_WYCA, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 500,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 
# Get the minimum employment centre travel time for each LSOA
closest_emp_centre_LSOA <- WYCA_Emp_ttm %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Travel Time Matrix - all-to-all ------------------------------------------
WYCA_ttm <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = lsoa_PWC_within_WYCA_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

WYCA_ttm_bus <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = lsoa_PWC_within_WYCA_buffer, 
    mode = c("BUS", "WALK"),
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

WYCA_ttm_train <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = lsoa_PWC_within_WYCA_buffer, 
    mode = "RAIL",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

WYCA_ttm_walk <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA, 
    destinations = lsoa_PWC_within_WYCA_buffer, 
    mode =  "WALK",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# BRES --------------------------------------------------------------------
# Load employment data from BRES. Note BRES uses 2011 LSOA codes, so lookup for 2021 conversion required
BRES <- read_csv("../../Data/Business_reg_Emp_Surv(BRES)2021.csv", skip = 8) %>% 
  separate("...1", into = c("LSOA11CD", "LSOA11NM"), sep = " : ") %>%
  rename("Employed" = "...2") %>%
  dplyr::select(-c("...3", "...4","...5")) %>%
  na.omit()
# Load 2011-2021 LSOA lookup table
LSOA_lookup <- read_csv("../../Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv")

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
  travel_matrix=WYCA_ttm,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index" = "Employed")

job_access_bus <- gravity(
  travel_matrix=WYCA_ttm_bus,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Bus" = "Employed")

job_access_train <- gravity(
  travel_matrix=WYCA_ttm_train,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Train" = "Employed")

job_access_walk <- gravity(
  travel_matrix=WYCA_ttm_walk,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Walk" = "Employed")

#Testing data
trav <- data.frame(
  from_id = 123,
  to_id = 456,
  travel_time_p50 =45)
opp <- data.frame(
  id = 456,
  Employed = 1000)
# access function testing        
(job_access_10 <- gravity(
  travel_matrix=trav,
  travel_cost="travel_time_p50",
  land_use_data=opp,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index" = "Employed"))

# Demand Potential Job Accessibility Index --------------------------------
# Travel Time Matrix for whole buffer
WYCA_ttm_buffer <-
  travel_time_matrix(
    r5r_core = r5r_WYCA, 
    origins = lsoa_PWC_within_WYCA_buffer, 
    destinations = lsoa_PWC_within_WYCA_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# Demand potential of LSOAs - needs to be based on full buffer TTM
demand_potential <- gravity(
  travel_matrix = WYCA_ttm_buffer,
  travel_cost="travel_time_p50",
  land_use_data=Employment,
  opportunity="Economically_active",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("Demand_potential" = "Economically_active") %>%
  left_join(BRES_2021_corrected, by = "id") %>%
  mutate(Jobs_over_demand_potential = Employed/Demand_potential) %>%
  as_tibble()

# PTJA adjusted for demand potential of highly populated areas
job_access_demand <- gravity(
  travel_matrix=WYCA_ttm,
  travel_cost="travel_time_p50",
  land_use_data=demand_potential,
  opportunity="Jobs_over_demand_potential",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_demand" = "Jobs_over_demand_potential")

# Join job_access_index to LSOA boundaries and other data
WYCA_dataset <- WYCA_TT_CC %>% 
  left_join(job_access, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_bus, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_train, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_walk, by = c("LSOA21CD"="id")) %>%
  left_join(BRES_2021_corrected, by = c("LSOA21CD"="id")) %>%
  left_join(closest_emp_centre_LSOA, by = c("LSOA21CD"="from_id")) %>%
  rename("Travel_time_Emp_Centre" = "closest_empcentre") %>%
  left_join (lsoa_with_town_info_min, by = "LSOA21CD") %>%
  left_join(job_access_demand, by = c("LSOA21CD"="id")) 

# Write dataset to shapefile
st_write(WYCA_dataset, "../../Data/WYCA_dataset.shp", append=FALSE)
st_write(towns_manual_WYCA, "../../Data/towns_centres_WYCA.shp", append=FALSE)
