
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
LCR_dataset_full_sf <- read_sf("Data/LCR_dataset_full_sf.shp") %>%
    rename("LSOA_Code" = LSOA_Cd,
         "LSOA_Name" = LSOA_Nm,
         "LSOA_Area" = LSOA_Ar,
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
LCR_pop <- read_sf("Data/LCR_population.shp") %>%
  st_transform(4326) %>%
  rename("Pop_Dens_change" = "Pp_dns_",
         "LSOA21CD" = "LSOA21C",
         "LCR_Pop_Dens"  = "P_2021_",
         "LCR_Pop" = "U_P_202") %>%
  dplyr::select(LSOA21CD, Pop_Dens_change, LCR_Pop_Dens, LCR_Pop) %>%
  as.data.frame()

# LCR Boundary + buffer
Boundaries <- read_sf("Data/GTFS_Data/Combined_Authorities_December_2023/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)

towns_centroids_LCR <- read_sf("Data/towns_centroids_LCR.shp") %>%
  filter(buffer == "N") #manually updated in 1. TravelTimeMatrix.R

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
LCR_traveltime_pop <- left_join(LCR_pop, 
                                  LCR_dataset_full_sf %>% 
                                    dplyr::select(LSOA_Code, Traveltime_empcent), 
                                  by=c("LSOA21CD"  ="LSOA_Code")) %>%
  mutate(Traveltime_empcent = as.numeric(Traveltime_empcent),
         LCR_Pop = as.numeric(LCR_Pop))

# Population within 30 and 45 mins
total_pop <- sum(LCR_traveltime_pop$LCR_Pop)
pop_within_45 <- LCR_traveltime_pop %>% filter(Traveltime_empcent <= 45) %>% summarize(Total_Pop_45 = sum(LCR_Pop))
pc_within_45 <- pop_within_45/total_pop
pop_within_30 <- LCR_traveltime_pop %>% filter(Traveltime_empcent <= 30) %>% summarize(Total_Pop_30 = sum(LCR_Pop))
pc_within_30 <- pop_within_30/total_pop



# Plot Employment Centre Travel Time Map ----------------------------------
# Plot travel time to nearest employment Centre
(emp_cent_map <- 
  LCR_dataset_full_sf %>% 
   select(Traveltime_empcent, geometry) %>%
    ggplot() +
    geom_sf(data=LCR_boundary, colour="black", linewidth=1.5)+
    geom_sf(aes(fill = Traveltime_empcent), col = NA) +
    scale_fill_viridis_b(breaks = seq(0, 60, 15), direction = -1) +
    labs(fill = "Travel time to 
                nearest employment
                centre (mins)") +
    geom_label( x=-2.88, y=53.40, label="Liverpool", size=3) +
    geom_label( x=-2.73, y=53.43, label="St. Helens", size=3) +
    geom_label( x=-3.10, y=53.64, label="Southport", size=3) +
    geom_label( x=-3.12, y=53.39, label="Birkenhead", size=3) +
    geom_sf(data=towns_centroids_LCR, shape = 21, fill = 'white', size = 1.5) +
   annotation_north_arrow(height = unit(1, "cm"),
                          width = unit(1, "cm"))+
   annotation_scale(location = "bl",
                    pad_x = unit(2.5, "cm"),
                    pad_y = unit(0, "cm"))+
    theme_void() )

ggsave(file = "3.Liverpool_Maps/emp_cent_map_LCR.jpeg", device = "jpeg", plot = emp_cent_map)
