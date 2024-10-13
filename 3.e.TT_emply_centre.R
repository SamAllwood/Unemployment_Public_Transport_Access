
# 1. Setup ----------------------------------------------------------------

library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)

setwd("~/Google Drive/My Drive/MSc Urban Transport/1.Dissertation/Programming")

# 2. Load Data ------------------------------------------------------------
MANCH_dataset_full_sf <- read_sf("Data/MANCH_dataset_full_sf.shp") %>%
    rename("LSOA_Code" = LSOA_Cd,
         "LSOA_Name" = LSOA_Nm,
         "LSOA_Area" = LSOA_Ar,
         Traveltime_Jobcentre = TrvlT_J,
         "PT_Job_Access_Index" = PT_Jb_A_I,
         "Job_Locations" = Emply_P,
         "Traveltime_empcent" = Trvltm_,
         "Townsuburb" = twnsbrb,
         "Economically_active_residents" = Ecnmcl_,
         "Unemployed_pop" = Unmplyd,
         "Unemployment_rate" = Unmply_,
         "No_car_rate" = N_cr_rt,
         "White_percent" = "% White",
         "Single_parent_households" = Sng_P_H,
         "Single_parent_household_rate" = "%",
         "Urban_rural" = Urbn_Rr,
         "Pop_16" = Tt___16,
         "Low_qual" = Lw_Qlfd,
         "Total_residents" = Ttl_rsd
)

# Read population density shapefile from "Population_Density_Map.R" script
MAN_pop <- read_sf("Data/MANCH_population.shp") %>%
  st_transform(4326) %>%
  rename("Pop_Dens_change" = "Pp_dns_",
         "LSOA21CD" = "LSOA21C",
         "Man_Pop_Dens"  = "P_2021_",
         "Man_Pop" = "U_P_202") %>%
  dplyr::select(LSOA21CD, Pop_Dens_change, Man_Pop_Dens, Man_Pop) %>%
  as.data.frame()

# Load Metrolink Shapefile
Metrolink <- st_read("Data/GM_Metrolink_MapData/SHP-format/Metrolink_Lines_Functional.shp")
Metrolink$LineName <- "Metrolink"

# GMCA Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)

towns_centroids <- read_sf("Data/towns_centroids.shp") #manually updated in 1. TravelTimeMatrix.R
towns_centroids_Man <- towns_centroids %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# Calculate UBDC estimates ------------------------------------------------

# Calculate UBDC estimates of time to employment centres
UBDC_access_cities <- read_csv("Data/access_cities_pt.csv") %>%
  mutate(nearest_main_bua = as.numeric(nearest_main_bua),
         nearest_sub_bua = as.numeric(nearest_sub_bua))
# Load population data
UK_pop <- read_csv("Data/Census2021_UsualPopulation.csv", skip=6, col_names = TRUE) %>%
  drop_na() %>%
  rename("Pop_2021" = "2021",
         "LSOA21CD" = "2021 super output area - lower layer") %>%
  mutate(LSOA21CD = sapply(strsplit(as.character(LSOA21CD), " : "), "[", 1))
# Join population to UBDC accessibility table
UBDC_pop <- left_join(UBDC_access_cities, UK_pop, by=c("geo_code"  = "LSOA21CD")) %>%
  mutate(Pop_2021 = as.numeric(Pop_2021)) %>%
  dplyr::select(geo_code, nearest_main_bua, nearest_sub_bua, Pop_2021) %>%
  as.data.frame() %>%
  drop_na()

# Calculate proportion of population within 45 minutes of employment centre from UBDC tables (whole of UK)
UBDC_count <- UBDC_pop %>% filter(nearest_main_bua <= 45) %>% summarize(Total_Pop = sum(Pop_2021))
Total_pop <- sum(UBDC_pop$Pop_2021)
UBDC_Percent_45 <- UBDC_count/Total_pop
UBDC_Percent_45

# Join population data to travel time - calculate proportion of population within 45 or 30 minutes of employment centre
MANCH_traveltime_pop <- left_join(MAN_pop, 
                                  MANCH_dataset_full_sf %>% 
                                    dplyr::select(LSOA_Code, Traveltime_empcent), 
                                  by=c("LSOA21CD"  ="LSOA_Code")) %>%
  mutate(Traveltime_empcent = as.numeric(Traveltime_empcent),
         Man_Pop = as.numeric(Man_Pop))

# Population within 30 and 45 mins
total_pop <- sum(MANCH_traveltime_pop$Man_Pop)
pop_within_45 <- MANCH_traveltime_pop %>% filter(Traveltime_empcent <= 45) %>% summarize(Total_Pop_45 = sum(Man_Pop))
pc_within_45 <- pop_within_45/total_pop
pop_within_30 <- MANCH_traveltime_pop %>% filter(Traveltime_empcent <= 30) %>% summarize(Total_Pop_30 = sum(Man_Pop))
pc_within_30 <- pop_within_30/total_pop



# Plot Employment Centre Travel Time Map ----------------------------------
# Plot travel time to nearest employment Centre
(emp_cent_map <- 
  MANCH_dataset_full_sf %>% 
   select(Traveltime_empcent, geometry) %>%
    ggplot() +
    geom_sf(data=GMCA_boundary, colour="black", linewidth=1.5)+
    geom_sf(aes(fill = Traveltime_empcent), col = NA) +
    scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
    labs(fill = "Travel time to 
                nearest employment
                centre (mins)",
         color = "Metrolink") +
    geom_label( x=-2.19, y=53.46, label="Manchester", size=3) +
    geom_label( x=-2.321, y=53.50, label="Salford", size=3) +
    geom_label( x=-2.092, y=53.415, label="Stockport", size=3) +
    geom_label( x=-2.04, y=53.54, label="Oldham", size=3) +
    geom_label( x=-2.10, y=53.63, label="Rochdale", size=3) +
    geom_label( x=-2.25, y=53.585, label="Bury", size=3) +
    geom_label( x=-2.483, y=53.58, label="Bolton", size=3) +
    geom_label( x=-2.58, y=53.54, label="Wigan", size=3) +
    geom_sf(data = Metrolink, aes(color = LineName), linewidth = 1.0) +
    geom_sf(data=towns_centroids_Man, shape = 21, fill = 'white', size = 1.5) +
   annotation_north_arrow()+
   annotation_scale(location = "bl",
                    pad_x = unit(2.5, "cm"),
                    pad_y = unit(0, "cm"))+
    theme_void() )

ggsave(file = "Plots/emp_cent_map.jpeg", device = "jpeg", plot = emp_cent_map)
