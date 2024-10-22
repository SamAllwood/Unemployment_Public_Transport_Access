
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
setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# LCR Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)

# Create a 20km buffer around the LCR boundary line
buffered_LCR_boundary <- st_buffer(LCR_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("Data/LAD_Dec_2021_GB_BFC_2022/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within LCR
LADs_LCR <- LADs %>% filter(as.vector(st_within(., LCR_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)
# mapview(LADs_LCR)+mapview(LCR_boundary)

# Read LSOA pop-weighted centroids
lsoas <- st_read("Data/GTFS_Data/LSOA/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022/LSOA_PopCentroids_EW_2021_V3.shp") %>%
  st_transform(4326)
# Filter LSOA PW-centroids
lsoas <- st_make_valid(lsoas)
lsoa_PWC_within_LCR <- lsoas %>% 
  filter(as.vector(st_within(., LCR_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
lsoa_PWC_within_LCR_buffer <- lsoas %>% 
  filter(as.vector(st_within(., buffered_LCR_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
# transform CRS to WGS84
lsoa_PWC_within_LCR_buffer$geometry <- st_transform(lsoa_PWC_within_LCR_buffer$geometry, 4326)
lsoa_PWC_within_LCR$geometry <- st_transform(lsoa_PWC_within_LCR$geometry, 4326)

#Read LSOA boundaries
lsoa_boundaries <- st_read("Data/GTFS_Data/LSOA/Lower_layer_Super_Output_Areas_2021_EW_BFC_V8/LSOA_2021_EW_BFC_V8.shp") %>%
  st_transform(4326)
lsoa_boundaries$geometry <- st_make_valid(lsoa_boundaries$geometry)
lsoa_boundaries$LSOA_area <- st_area(lsoa_boundaries$geometry)
lsoa_boundaries$LSOA_area_km2 <- lsoa_boundaries$LSOA_area/1000000

# Filter LSOA boundaries for LCR buffer
#lsoa_boundaries <- st_make_valid(lsoa_boundaries)
lsoa_boundaries_within_LCR_buffer <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                                buffered_LCR_boundary, 
                                                                sparse = FALSE), ]
lsoa_boundaries_within_LCR <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                         LCR_bound_small_buffer, 
                                                         sparse = FALSE), ]
# Read Towns and City boundaries
towns <- st_read("Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_LCR_buffer <- towns[st_within(towns, buffered_LCR_boundary, sparse = FALSE), ]
towns_centroids <- st_centroid(towns_within_LCR_buffer) %>%
  rename(id = TCITY15CD) %>% st_transform(4326)

towns_manual_LCR <- data.frame( 
  id = c("Birkenhead", "Chester", "Liverpool", "Southport", "St Helens", "Warrington", "Wigan"),
  lon = c(-3.023030, -2.888427, -2.981652, -3.004679, -2.730498, -2.593275, -2.630668), 
  lat = c(53.391067, 53.193262, 53.407438, 53.646784, 53.452760, 53.392443, 53.545255),
  buffer = c("N", "Y", "N", "N", "N", "Y", "Y")
) 

# Convert the dataframe to an sf object, assuming WGS 84 (EPSG: 4326) coordinate reference system
towns_manual_LCR <- st_as_sf(towns_manual_LCR, coords = c("lon", "lat"), crs = 4326)
mapview(towns_manual_LCR)+
  mapview(towns_within_LCR_buffer)+
  mapview(LCR_boundary)+
  mapview(buffered_LCR_boundary) +
  mapview(towns_centroids)

# Label LSOAs within town boundaries
towns_with_buffer <- towns_within_LCR_buffer %>% st_buffer(dist=400)
lsoa_with_town_info <- st_join(lsoa_boundaries_within_LCR, towns_with_buffer, join = st_within)
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

#mapview(towns_within_LCR_buffer)+mapview(LCR_boundary)+mapview(towns_centroids)+mapview(towns_manual_LCR)
#mapview(lsoa_with_town_info)+ mapview(towns_with_buffer)

# Liverpool City Centre - LOCATION? And fix coords
LCR_CC <- 
  data.frame(id = "LCR_CC", lat = 53.407438, lon = -2.981652) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
LCR_CC_point <- 
  geom_sf(data = LCR_CC, shape = 21, fill = 'white', size = 2)

# Employed Residents
Employment <-read_csv("Data/census2021-ts066/census2021-ts066-lsoa.csv") %>%
  dplyr::select("geography",
                "geography code",
                "Economic activity status: Economically active (excluding full-time students)") %>%
  rename("LSOA_name" = "geography",
         "id" = "geography code",
         "Economically_active" = "Economic activity status: Economically active (excluding full-time students)")


# Load and validate BODS GTFS data - for diagnostics only
#BODS_LCR <- read_gtfs("Data/GTFS_Data/r5r/BODS_LCR.gtfs.zip")
#BODS_LCR_valid <- tidytransit::validate_gtfs(BODS_LCR, warnings = TRUE)
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
# Get input files - will load BODS_MANCH and BODS_LCR - consider simplifying if
# too slow.
input_files <- list.files("Data/GTFS_Data/r5r", 
  recursive = TRUE,
  pattern = 'gtfs\\.zip$|pbf$',
  full.names = TRUE)
# List input files
input_files


# Read multi-modal network
gc() #this setup step requires quite a bit of memory, so best to gc first
options(java.parameters = "-Xmx8G")
r5r_LCR <- setup_r5(data_path = "Data/GTFS_Data/r5r", 
                     verbose=TRUE,
                     overwrite = FALSE) # make sure you leave this to run after it indicates it's finished

summary(r5r_LCR)
print(r5r_LCR)
#network <- transit_network_to_sf(r5r_LCR) 
#mapview(network)
#Check transit network visuals - only for diagnostics and also shapefile won't show rail network
#network <- transit_network_to_sf(r5r_LCR) 
#network$routes <- network$routes %>% st_transform(4326) %>% filter(short_name==356)
#network$stops <- network$stops %>% st_transform(4326) 
#network$routes <- network$routes %>% filter(mode == "TRAM")

# Transit Calculation Parameters ------------------------------------------------------
# Time and date of departure
departure_datetime <- as.POSIXct("2024-05-21 08:00:00") 
mode = c( "TRANSIT") # note function always includes 'WALK' anyway
walk_speed = 4.32
max_duration = 200L
max_walk_time = 30L

LCR_TT_CC <- 
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = LCR_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  rename("Traveltime_CC" = "travel_time_p50",
         "LSOA21CD" = "from_id") %>%
  select(-"to_id")

LCR_Emp_ettm <-
  expanded_travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = LCR_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 50,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 

# Travel time to employment centres ---------------------------------------
LCR_Emp_ttm <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = towns_manual_LCR, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 500,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 
# Get the minimum employment centre travel time for each LSOA
closest_emp_centre_LSOA <- LCR_Emp_ttm %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Travel Time Matrix - all-to-all ------------------------------------------
LCR_ttm <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = lsoa_PWC_within_LCR_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

LCR_ttm_bus <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = lsoa_PWC_within_LCR_buffer, 
    mode = c("BUS", "WALK"),
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

LCR_ttm_tram <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = lsoa_PWC_within_LCR_buffer, 
    mode = "TRAM",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

LCR_ttm_walk <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR, 
    destinations = lsoa_PWC_within_LCR_buffer, 
    mode =  "WALK",
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# BRES --------------------------------------------------------------------
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
  travel_matrix=LCR_ttm,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index" = "Employed")

job_access_bus <- gravity(
  travel_matrix=LCR_ttm_bus,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Bus" = "Employed")

job_access_tram <- gravity(
  travel_matrix=LCR_ttm_tram,
  travel_cost="travel_time_p50",
  land_use_data=BRES_2021_corrected,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_Tram" = "Employed")

job_access_walk <- gravity(
  travel_matrix=LCR_ttm_walk,
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
  travel_time_p50 =60)
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
LCR_ttm_buffer <-
  travel_time_matrix(
    r5r_core = r5r_LCR, 
    origins = lsoa_PWC_within_LCR_buffer, 
    destinations = lsoa_PWC_within_LCR_buffer, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = max_duration,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE)

# Demand potential of LSOAs - needs to be based on full buffer TTM
demand_potential <- gravity(
  travel_matrix = LCR_ttm_buffer,
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
  travel_matrix=LCR_ttm,
  travel_cost="travel_time_p50",
  land_use_data=demand_potential,
  opportunity="Jobs_over_demand_potential",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_demand" = "Jobs_over_demand_potential")

# Join job_access_index to LSOA boundaries and other data
LCR_dataset <- LCR_TT_CC %>% 
  left_join(job_access, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_bus, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_tram, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_walk, by = c("LSOA21CD"="id")) %>%
  left_join(BRES_2021_corrected, by = c("LSOA21CD"="id")) %>%
  left_join(closest_emp_centre_LSOA, by = c("LSOA21CD"="from_id")) %>%
  rename("Travel_time_Emp_Centre" = "closest_empcentre") %>%
  left_join (lsoa_with_town_info_min, by = "LSOA21CD") %>%
  left_join(job_access_demand, by = c("LSOA21CD"="id")) 

# Write dataset to shapefile
st_write(LCR_dataset, "Data/LCR_dataset.shp", append=FALSE)
st_write(towns_manual_LCR, "Data/towns_centroids_LCR.shp", append=FALSE)
