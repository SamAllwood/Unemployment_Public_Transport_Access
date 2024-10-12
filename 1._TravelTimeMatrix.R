
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

# 2. Load and Filter Datasets ------------------------------------------------------------
setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# GMCA Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)

# Create a 20km buffer around the GMCA boundary line
buffered_GMCA_boundary <- st_buffer(GMCA_boundary, dist = 20000) %>% 
  st_transform(4326)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("Data/LAD_Dec_2021_GB_BFC_2022/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within GMCA
LADs_MANCH <- LADs %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)
# mapview(LADs_MANCH)+mapview(GMCA_boundary)

# Read LSOA pop-weighted centroids
lsoas <- st_read("Data/GTFS_Data/LSOA/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022/LSOA_PopCentroids_EW_2021_V3.shp") %>%
  st_transform(4326)
# Filter LSOA PW-centroids
lsoas <- st_make_valid(lsoas)
lsoa_PWC_within_GMCA <- lsoas %>% 
  filter(as.vector(st_within(., GMCA_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
lsoa_PWC_within_GMCA_buffer <- lsoas %>% 
  filter(as.vector(st_within(., buffered_GMCA_boundary, sparse = FALSE))) %>% 
  st_transform(4326) %>% rename(id = LSOA21CD) 
# transform CRS to WGS84
lsoa_PWC_within_GMCA_buffer$geometry <- st_transform(lsoa_PWC_within_GMCA_buffer$geometry, 4326)
lsoa_PWC_within_GMCA$geometry <- st_transform(lsoa_PWC_within_GMCA$geometry, 4326)

#Read LSOA boundaries
lsoa_boundaries <- st_read("Data/GTFS_Data/LSOA/Lower_layer_Super_Output_Areas_2021_EW_BFC_V8/LSOA_2021_EW_BFC_V8.shp") %>%
  st_transform(4326)
lsoa_boundaries$geometry <- st_make_valid(lsoa_boundaries$geometry)
lsoa_boundaries$LSOA_area <- st_area(lsoa_boundaries$geometry)
lsoa_boundaries$LSOA_area_km2 <- lsoa_boundaries$LSOA_area/1000000

# Filter LSOA boundaries for GMCA buffer
#lsoa_boundaries <- st_make_valid(lsoa_boundaries)
lsoa_boundaries_within_GMCA_buffer <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                                buffered_GMCA_boundary, 
                                                                sparse = FALSE), ]
lsoa_boundaries_within_GMCA <- lsoa_boundaries[st_within(lsoa_boundaries, 
                                                         GMCA_bound_small_buffer, 
                                                         sparse = FALSE), ]
# Read Towns and City boundaries
towns <- st_read("Data/Major_Towns_and_Cities_Dec_2015_Boundaries_V2_2022/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_GMCA_buffer <- towns[st_within(towns, buffered_GMCA_boundary, sparse = FALSE), ]
towns_centroids <- st_centroid(towns_within_GMCA_buffer) %>%
  rename(id = TCITY15CD) %>% st_transform(4326)

towns_manual_MAN <- data.frame(
  id = c("Manchester", "Salford", "Stockport", "Oldham", "Rochdale", "Bury", "Bolton", "Wigan"),
  lon = c(-2.243185, -2.277002, -2.161260, -2.112658, -2.153749, -2.297645, -2.429588, -2.630668), # Updated longitudes
  lat = c(53.478214, 53.485645, 53.406539, 53.540754, 53.611260, 53.592658, 53.578226, 53.545255)  # Updated latitudes
) 

# Convert the dataframe to an sf object, ensuring WGS 84 (EPSG: 4326) CRS
towns_manual_MAN <- st_as_sf(towns_manual_MAN, coords = c("lon", "lat"), crs = 4326)

# Label LSOAs within town boundaries - manual adjustments to tidy up intersections
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

#lsoa_with_town_info_min %>% count(TCITY15NM=="Manchester")
#mapview(lsoa_with_town_info %>% filter(TCITY15NM != "Suburb"))+ mapview(towns_within_GMCA_buffer)
#mapview(lsoa_boundaries_within_towns)#+mapview(towns_within_GMCA_buffer)
#mapview(towns_within_GMCA_buffer)+mapview(GMCA_boundary)+mapview(towns_centroids)+mapview(towns_manual_MAN)
#mapview(lsoa_with_town_info)+ mapview(towns_with_buffer)

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


## BODS GTFS Data - just download once, filter once then use the BODS_MANCH.gtfs.zip file created from the filter.  
# BODS <- read_gtfs("itm_all_gtfs 2.zip")
# Filter BODS stops in Manchester
#BODS_MANCH <- filter_feed_by_area(BODS, buffered_GMCA_boundary)
#summary(BODS_MANCH)
#write_gtfs(BODS_MANCH, "BODS_MANCH.gtfs.zip")

# 3. OpenStreetMap ---------------------------------------------------------------
# PBF road and pedestrian network for England - necessary to 
# get all of transit network activity and avoid boundary issues.
# Only need to download once so including the url for reference
#england_pbf <- "https://download.geofabrik.de/europe/united-kingdom/england-latest.osm.pbf"
#file.remove("./r5r/geofabrik_england-latest.osm.pbf")
#oe_download(file_url = england_pbf, download_directory = "./r5r")

# 4. R5R Network Setup -------------------------------------------------------

# Get input files
input_files <- list.files("Data/GTFS_Data/r5r", 
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
gc() #this setup step requires quite a bit of memory, so best to gc first
options(java.parameters = "-Xmx8G")
r5r_core <- setup_r5(data_path = "Data/GTFS_Data/r5r", 
                     verbose=TRUE,
                     overwrite = FALSE) # make sure you leave this to run after it indicates it's finished

summary(r5r_core)
print(r5r_core)
network <- transit_network_to_sf(r5r_core) 

#Check transit network visuals - only for diagnostics and also shapefile doesn't show rail network
#network <- transit_network_to_sf(r5r_core) 
#network$routes <- network$routes %>% st_transform(4326) %>% filter(short_name==356)
#network$stops <- network$stops %>% st_transform(4326) 
#network$routes <- network$routes %>% filter(mode == "TRAM")

# Transit Calculation Parameters ------------------------------------------------------
# Time and date of departure
departure_datetime <- as.POSIXct("2024-05-21 08:00:00") 
departure_datetime7 <- as.POSIXct("2024-05-21 07:00:00") 
departure_datetime830 <- as.POSIXct("2024-05-21 08:30:00") 
mode = c( "TRANSIT", "WALK") # note function always includes 'WALK' anyway
walk_speed = 4.32
max_duration = 200L
max_walk_time = 30L

MAN_TT_CC <- 
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

MAN_Emp_ettm <-
  expanded_travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = MAN_CC, 
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_trip_duration = 50,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) 

# Travel time to employment centres ---------------------------------------
MAN_Emp_ttm <-
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
# Get the minimum employment centre travel time for each LSOA
closest_emp_centre_LSOA <- MAN_Emp_ttm %>%
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

# Travel time with 20-min extended time window
MAN_comp_ttm_ext20 <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    time_window = 20,
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Travel time with 60-min extended time window
MAN_comp_ttm_ext60 <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    time_window = 60,
    mode = mode,
    departure_datetime = departure_datetime,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  group_by(from_id) %>%
  summarize(closest_empcentre = min(travel_time_p50, na.rm = TRUE))

# Join city centre travel time to LSOAs dataframe
MAN_Traveltime_compare <- left_join(lsoa_boundaries_within_GMCA, MAN_comp_ttm, by = c("LSOA21CD" = "from_id")) %>% 
  rename("travel_time_10min_window" = "closest_empcentre") %>% 
  left_join(MAN_comp_ttm_ext20, by=c("LSOA21CD" = "from_id")) %>% 
  rename("travel_time_20min_window" = "closest_empcentre")  %>% 
  left_join(MAN_comp_ttm_ext60, by=c("LSOA21CD" = "from_id")) %>% 
  rename("travel_time_60min_window" = "closest_empcentre")  %>% 
  mutate(travel_time_diff20_10 = travel_time_20min_window - travel_time_10min_window) %>% 
  mutate(travel_time_diff60_10 = travel_time_60min_window - travel_time_10min_window) %>% 
    drop_na()

map1 <- MAN_Traveltime_compare %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = travel_time_10min_window), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
  labs(fill =  "10min time window") +
  guides(fill = guide_legend(override.aes = list(color = NA, fill = NA),
                             label.theme = element_blank()))+
  theme_void() +
  theme(legend.position = "bottom")

map2 <- MAN_Traveltime_compare %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = travel_time_60min_window), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
  labs(fill = "60min time window") +
  theme_void()+
  theme(legend.position = "bottom")

combined_time_window <- map1 + map2 + plot_annotation(title = "Comparison of Time Windows")
#ggsave(file = "Plots/Time_window_comparison.jpeg", device = "jpeg", plot = combined_time_window)

# Compare departure times
MAN_comp_ttm_7 <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    mode = mode,
    departure_datetime = departure_datetime7,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  group_by(from_id) %>%
  summarize(closest_empcentre7 = min(travel_time_p50, na.rm = TRUE))
MAN_comp_ttm_830 <-
  travel_time_matrix(
    r5r_core = r5r_core, 
    origins = lsoa_PWC_within_GMCA, 
    destinations = towns_manual_MAN, 
    mode = mode,
    departure_datetime = departure_datetime830,
    walk_speed = walk_speed,
    max_walk_time = max_walk_time,
    verbose = FALSE,
    progress = TRUE) %>%
  group_by(from_id) %>%
  summarize(closest_empcentre830 = min(travel_time_p50, na.rm = TRUE))

MAN_depart_time <- left_join(lsoa_boundaries_within_GMCA, MAN_comp_ttm_7, by = c("LSOA21CD" = "from_id")) %>% 
  left_join(MAN_comp_ttm_830, by=c("LSOA21CD" = "from_id")) %>% 
  left_join(MAN_comp_ttm, by=c("LSOA21CD" = "from_id")) %>% 
  rename("closest_empcentre8" = "closest_empcentre")  

map7 <- MAN_depart_time %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = closest_empcentre7), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
  labs(fill =  "7:00am") +
  guides(fill = guide_legend(override.aes = list(color = NA, fill = NA),
                             label.theme = element_blank()))+
  theme_void() +
  theme(legend.position = "bottom")
map8 <- MAN_depart_time %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = closest_empcentre8), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
  labs(fill =  "8:00am") +
  guides(fill = guide_legend(override.aes = list(color = NA, fill = NA),
                             label.theme = element_blank()))+
  theme_void() +
  theme(legend.position = "bottom")
map830 <- MAN_depart_time %>%
  ggplot() +
  geom_sf(data=GMCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = closest_empcentre830), col = NA) +
  scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
  labs(fill =  "8:30am") +
  theme_void() +
  theme(legend.position = "bottom")

combined_depart <- map7+ map8 + map830 + plot_annotation(title = "Comparison of Planned Departure Times")
ggsave(file = "Plots/Departure_time_comparison.jpeg", device = "jpeg", plot = combined_depart)


# Jobcentre Plus Proximity ------------------------------------------------
# https://www.gov.uk/government/publications/dwp-jobcentre-register
#Jobcentre_plus <- read_csv("../Jobcentre_locations.csv") # Jobcentre locations 2019
# Geolocate the postcodes - don't need to do this every time as it uses Google API so takes a few mins
#Jobcentre_plus <- Jobcentre_plus %>% mutate_geocode(Postcode, source="google", output="latlon", key="AIzaSyC7Ld1-mUEUP9ONWlipSAuHcjWaMjZS2nQ")
# Filter out discontinued geocodes
#Jobcentre_plus <- Jobcentre_plus %>% filter(!is.na(lon) & !is.na(lat))
#write_csv(Jobcentre_plus, "../Jobcentre_locations_geocoded.csv")
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

# calculate travel time from each LSOA to closest job centre
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

# Demand Potential Job Accessibility Index --------------------------------
# Travel Time Matrix for whole buffer
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

# PTJA adjusted for demand potential of highly populated areas
job_access_demand <- gravity(
  travel_matrix=MANCH_ttm,
  travel_cost="travel_time_p50",
  land_use_data=demand_potential,
  opportunity="Jobs_over_demand_potential",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
  rename("PT_Job_Access_Index_demand" = "Jobs_over_demand_potential")

#Testing data
trav <- data.frame(
  from_id = 123,
  to_id = 456,
  travel_time_p50 =10)
opp <- data.frame(
  id = 456,
  Employed = 1000)
        
(job_access_10 <- gravity(
  travel_matrix=trav,
  travel_cost="travel_time_p50",
  land_use_data=opp,
  opportunity="Employed",
  decay_function=decay_logistic(39.7,12.6), #note mean=39.7 and sd=12.6 are calculated in Decay_Params.R file.
  fill_missing_ids = TRUE) %>%
    rename("PT_Job_Access_Index" = "Employed"))

# Join job_access_index to LSOA boundaries and other data
MANCH_dataset <- MANCH_TT_Jobcentre %>% 
  left_join(job_access, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_bus, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_tram, by = c("LSOA21CD"="id")) %>%
  left_join(job_access_walk, by = c("LSOA21CD"="id")) %>%
  left_join(BRES_2021_corrected, by = c("LSOA21CD"="id")) %>%
  rename("Travel_time_Job_Centre" = "closest_jobcentre") %>%
  left_join(closest_emp_centre_LSOA, by = c("LSOA21CD"="from_id")) %>%
  rename("Travel_time_Emp_Centre" = "closest_empcentre") %>%
  left_join(MAN_TT_CC, by = c("LSOA21CD"="from_id")) %>%
  left_join(job_access_demand, by = c("LSOA21CD"="id")) 

# Write dataset to shapefile
st_write(MANCH_dataset, "Data/MANCH_dataset.shp", append=FALSE)
st_write(towns_manual_MAN, "Data/towns_centroids.shp", append=FALSE)
