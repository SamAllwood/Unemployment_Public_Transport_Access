## Script to create a map of job opportunities in WYCA
# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(classInt)
library(ggspatial)

# 2. Load Data ------------------------------------------------------------
# Load WYCA Geo data from file
WYCA_dataset_jobs <- read_sf("../Data/WYCA_dataset.shp") %>%
  rename("LSOA_Code" = "LSOA21C",
         "LSOA_Name" = "LSOA21N",
         "PT_Job_Access_Index" = "PT_Jb_A_I",
         "Employed_Population" = "Employd",
         "LSOA_Area" = "LSOA__2",
         "Traveltime_empcent" = "Tr__E_C")

# WYCA Boundary + buffer
Boundaries <- read_sf("../Data/CAUTH_DEC_2023_EN_BFC.shp")
WYCA_boundary <- Boundaries %>% filter(CAUTH23NM == "West Yorkshire") %>%
  st_transform(4326) 
WYCA_bound_small_buffer <- WYCA_boundary %>% st_buffer(dist=1000)

# Town centres
towns_centres_WYCA <- read_sf("../Data/towns_centres_WYCA.shp") %>% 
  st_transform(4326) %>%
  filter(buffer == "N") 

# Town boundaries
towns <- st_read("../Data/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_WYCA_buffer <- towns[st_within(towns, WYCA_bound_small_buffer, sparse = FALSE), ]

## Plot job locations levels across WYCA -------------------------------------------
# create job density variable
WYCA_dataset_jobs$job_density <- WYCA_dataset_jobs$Employed_Population/WYCA_dataset_jobs$LSOA_Area

# Calculate PTJA quantile breaks
breaks_jobs <- classIntervals(WYCA_dataset_jobs$job_density, n = 5, style = "quantile")$brks

# Create a factor variable for PTJA coloring
WYCA_dataset_jobs$job_density_colour <- cut(WYCA_dataset_jobs$job_density, 
                                            breaks = breaks_jobs, 
                                            include.lowest = TRUE, 
                                            labels = FALSE)
WYCA_dataset_jobs$job_density_colour <- as.factor(WYCA_dataset_jobs$job_density_colour)

labels_jobs <- c("<150 jobs/km2","150 - 350","350 - 725","725 - 1650",">1650 jobs/km2")

(jobs <- WYCA_dataset_jobs %>%
  ggplot() +
  geom_sf(data=WYCA_boundary, colour="black",linewidth=1.5)+
  geom_sf(aes(fill = job_density_colour), col = NA) +
  scale_fill_brewer(palette = "Spectral", 
                    direction = -1,
                    labels = labels_jobs) +
  labs(fill = "Job Density (quintiles)",
       title = "West Yorkshire Combined Authority Job Distribution") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_sf(data=towns_within_WYCA_buffer, fill = NA, col = "black", linewidth = 0.3) +
    geom_sf(data=(towns_centres_WYCA %>% filter("Buffer" == "N")), shape = 21, fill = 'white', size = 1.5) +
    geom_label( x=-1.756, y=53.79, label="Bradford", size=3) +
    geom_label( x=-1.859, y=53.723, label="Halifax", size=3) +
    geom_label( x=-1.782, y=53.646, label="Huddersfield", size=3) +
    geom_label( x=-1.545, y=53.796, label="Leeds", size=3) +
    geom_label( x=-1.5058, y=53.676, label="Wakefield", size=3)+
    annotation_north_arrow(  height = unit(1, "cm"),
                             width = unit(1, "cm")) +
    annotation_scale(location = "bl",
                     pad_x = unit(2.5, "cm"),
                     pad_y = unit(0, "cm"))+
  theme_void() )

ggsave(file = "WYCA/Images/jobs_WYCA.jpeg", device = "jpeg", plot = jobs)
