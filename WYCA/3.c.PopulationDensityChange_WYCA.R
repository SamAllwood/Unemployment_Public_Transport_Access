## Script for mapping population density change in WYCA between 2001 and 2021
# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(sf)

# 2. Load and Filter Datasets ------------------------------------------------------------
# WYCA Boundary + buffer
Boundaries <- read_sf("../Data/CAUTH_DEC_2023_EN_BFC.shp")
WYCA_boundary <- Boundaries %>% filter(CAUTH23NM == "West Yorkshire") %>%
  st_transform(4326) 
WYCA_bound_small_buffer <- WYCA_boundary %>% st_buffer(dist=1000)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("../Data/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)

# Town centres
towns_centres_WYCA <- read_sf("../Data/towns_centres_WYCA.shp") %>% 
  st_transform(4326) %>%
  filter(buffer == "N") 

# Filter LADs within WYCA
LADs_WYCA <- LADs %>% filter(as.vector(st_within(., WYCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)
Pop_2001 <- read_csv("../Data/Census2001_UsualPopulation.csv", skip=6) %>%
  rename("LSOA01" = "2001 super output areas - lower layer",
         "Pop_2001" = "2001") %>%
  drop_na() %>%
  mutate(LSOA01 = str_sub(LSOA01,1, 9))%>%
  mutate(Pop_2001 = as.numeric(Pop_2001))

# Read Towns and City boundaries
towns <- st_read("../Data/TCITY_2015_EW_BGG_V2.shp") %>%
  st_transform(4326) 
towns$geometry <- st_make_valid(towns$geometry)
towns_within_WYCA <- towns[st_within(towns, WYCA_bound_small_buffer, sparse = FALSE), ]

# Load 2021 Census data on population
Pop2021 <- read_csv("../Data/Census2021_UsualPopulation.csv", skip=6) %>%
  rename("LSOA21CD" = "2021 super output area - lower layer",
         "Usual_Pop_2021" = "2021") %>%
  drop_na() %>%
  mutate(LSOA21CD = str_sub(LSOA21CD,1, 9))%>%
  mutate(Usual_Pop_2021 = as.numeric(Usual_Pop_2021))


# 2001 - 2011 LSOA Conversion --------------------------------------------------
# Load 2001-2011 LSOA lookup table
LSOA_lookup_01_11 <- read_csv("../Data/LSOA_2001_2011_lookup.csv")

# Split LSOAs - population split between new LSOAs generated in 2011
# Join the LSOA lookup table to the dataset
Pop_2001_corrected_11 <- Pop_2001 %>%
  left_join(LSOA_lookup_01_11, by = c("LSOA01" = "LSOA01CD")) 
# Step 1: Count duplicates
duplicates_count01 <- Pop_2001_corrected_11 %>%
  group_by(LSOA01) %>%
  summarise(Duplicates = n(), .groups = 'drop')
# Step 2: Join the count back to the original dataset
Pop_2001_corrected_11 <- Pop_2001_corrected_11 %>%
  left_join(duplicates_count01, by = "LSOA01")
# Step 3: Adjust the population
Pop_2001_corrected_11 <- Pop_2001_corrected_11 %>%
  mutate(Pop_2001 = Pop_2001 / Duplicates)
# Remove the Duplicates column
Pop_2001_corrected_11 <- Pop_2001_corrected_11 %>%
  dplyr::select(-Duplicates)

# Merged LSOAs - population added together for LSOAs which are merged in 2011
# Step 1: Filter for CHGIND = "M", then sum Population for each LSOA_Code
summarized_population <- Pop_2001_corrected_11 %>%
  filter(CHGIND == "M") %>%
  group_by(LSOA11CD) %>%
  summarise(Pop_2001 = sum(Pop_2001, na.rm = TRUE), .groups = 'drop')
# Step 2: Remove duplicates for LSOA11CD
Pop_2001_corrected_11 <- Pop_2001_corrected_11 %>%
 distinct(LSOA11CD, .keep_all = TRUE)
# Step 3: Combine the datasets
Pop_2001_corrected_11 <- Pop_2001_corrected_11 %>%
  left_join(summarized_population, by = "LSOA11CD")  %>%
  mutate(Pop_2001_corrected_11 = coalesce(Pop_2001.y, Pop_2001.x)) %>%
  dplyr::select(c(Pop_2001_corrected_11, LSOA11CD))  # Assuming you want to remove the New_Population column after update


# 2011 - 2021 LSOA Conversion --------------------------------------------------
# Load 2011-2021 LSOA lookup table
LSOA_lookup_11_21 <- read_csv("../Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv")

# Split LSOAs - population split between new LSOAs generated in 2021
# Join the LSOA lookup table to the dataset
Pop_2001_corrected_21 <- Pop_2001_corrected_11 %>%
  left_join(LSOA_lookup_11_21, by = "LSOA11CD")
# Step 1: Count duplicates
duplicates_count11 <- Pop_2001_corrected_21 %>%
  group_by(LSOA11CD) %>%
  summarise(Duplicates = n(), .groups = 'drop')
# Step 2: Join the count back to the original dataset
Pop_2001_corrected_21 <- Pop_2001_corrected_21 %>%
  left_join(duplicates_count11, by = "LSOA11CD")
# Step 3: Adjust the population
Pop_2001_corrected_21 <- Pop_2001_corrected_21 %>%
  mutate(Pop_2001_corrected_11 = Pop_2001_corrected_11 / Duplicates)
# Remove the Duplicates column
Pop_2001_corrected_21 <- Pop_2001_corrected_21 %>%
  dplyr::select(-Duplicates)

# Merged LSOAs - population added together for LSOAs which are merged in 2011
# Step 1: Filter for CHGIND = "M", then sum Population for each LSOA_Code
summarized_population11 <- Pop_2001_corrected_21 %>%
  filter(CHGIND == "M") %>%
  group_by(LSOA21CD) %>%
  summarise(Pop_2001_corrected_11 = sum(Pop_2001_corrected_11, na.rm = TRUE), .groups = 'drop')
# Step 2: Remove duplicates for LSOA21CD to remove additional LSOAs from merges
Pop_2001_corrected_21 <- Pop_2001_corrected_21 %>%
  distinct(LSOA21CD, .keep_all = TRUE)
# Step 3: Combine the datasets
Pop_2001_corrected_21 <- Pop_2001_corrected_21 %>%
  left_join(summarized_population11, by = "LSOA21CD")  %>%
  mutate(Pop_2001_corrected_21 = coalesce(Pop_2001_corrected_11.y, Pop_2001_corrected_11.x)) %>%
  dplyr::select(c(Pop_2001_corrected_21, LSOA21CD))  
# Step 4: replace NAs in Pop_2001_corrected_21 with population as stated in 2021. 
# There are only 5 across England and Wales, but this prevents any NAs in the dataset
Pop_2001_corrected_21 <- Pop2021 %>% 
  left_join(Pop_2001_corrected_21, by = "LSOA21CD") %>%
  mutate(Pop_2001_corrected_21 = coalesce(Pop_2001_corrected_21, Usual_Pop_2021)) %>%
  dplyr::select(LSOA21CD, Pop_2001_corrected_21)

# Calculate the scaling factor
sum(Pop2021$Usual_Pop_2021, na.rm = TRUE)/sum(Pop_2001_corrected_21$Pop_2001_corrected_21, na.rm = TRUE)

# Load West Yorkshire Dataset Shapefile ---------------------------------------
WYCA_dataset <- read_sf("../Data/WYCA_dataset.shp") %>%
               rename("LSOA21CD" = "LSOA21C",
                      "LSOA21NM" = "LSOA21N",
                      "PT_Job_Access_Index" = "PT_Jb_A_I",
                      "Employed_Population" = "Employd",
                      "LSOA_Area" = "LSOA__2") 


# Join the population datasets to the WYCA dataset
WYCA_population <- WYCA_dataset %>%
  left_join(Pop_2001_corrected_21, by = "LSOA21CD") %>%
  left_join(Pop2021, by = "LSOA21CD") %>%
  dplyr::select(LSOA21CD, LSOA21NM, Pop_2001_corrected_21, Usual_Pop_2021, LSOA_Area) %>%
  mutate(Pop_2001_density = Pop_2001_corrected_21 / LSOA_Area) %>%
  mutate(Pop_2021_density = Usual_Pop_2021 / LSOA_Area) %>%
  mutate(Pop_density_change = Pop_2021_density - Pop_2001_density)

# Average Population Density in WYCA
Total_pop_21 <- sum(WYCA_population$Usual_Pop_2021)
Mean_pop_dens21 <- Total_pop_21 / sum(WYCA_population$LSOA_Area)
Mean_pop_dens21
Total_pop_01 <- sum(WYCA_population$Pop_2001_corrected_21)
Mean_pop_dens01 <- Total_pop_01 / sum(WYCA_population$LSOA_Area)
Mean_pop_dens01
# Add Pop density change variable
WYCA_population <- WYCA_population  %>%
  mutate(Pop_density_change = as.numeric(Pop_density_change), # Ensure variable is numeric
         Pop_dens_change_cat = cut(Pop_density_change,
                                   breaks = c(-30000, 0, 500, 1500, 30000),
                                   include.lowest = TRUE,
                                   labels = c("dP < 0", 
                                              "0 >= dP < 500", 
                                              "500 >= dP <1500", 
                                              "dP >= 1500"))) 

# Plot Population Density Change 2001 - 2021
WYCA_population  %>%
      filter(as.vector(st_within(., WYCA_bound_small_buffer, sparse = FALSE))) %>%
          ggplot() +
            geom_sf(data=WYCA_boundary, colour="black",linewidth=1.5) +
            geom_sf(aes(fill = Pop_dens_change_cat), colour = NA) +
            scale_fill_brewer() +
            labs(fill=" Population Change per
            square kilometer",
                 title = "Population Density Change across WYCA from 2001-2021") +
            geom_sf(data=towns_within_WYCA, fill = NA, col = "black", size = 1) +
            geom_sf(data=towns_centres_WYCA, shape = 21, fill = 'white', size = 1.5) +
            geom_label( x=-1.756, y=53.79, label="Bradford", size=3) +
            geom_label( x=-1.859, y=53.723, label="Halifax", size=3) +
            geom_label( x=-1.782, y=53.646, label="Huddersfield", size=3) +
            geom_label( x=-1.545, y=53.796, label="Leeds", size=3) +
            geom_label( x=-1.5058, y=53.676, label="Wakefield", size=3)+
            theme_void() 

ggsave(file = "WYCA/Images/Population_Density_Change_WYCA.jpg", device = "jpeg")

# Write as output shapefile
write_sf(WYCA_population, "../Data/WYCA_population.shp")

# Calculate Pop_density quantile breaks
breaks_pop <- classIntervals(WYCA_population$Pop_2021_density, n = 5, style = "quantile")$brks
# Create a factor variable for PTJA coloring
WYCA_population$Pop_density_cat <- cut(WYCA_population$Pop_2021_density, breaks = breaks_pop, include.lowest = TRUE, labels = FALSE)
WYCA_population$Pop_density_cat <- as.factor(WYCA_population$Pop_density_cat)
labels_pop <- c("<1,200","1,200 - 2,600","2,600 - 4,000","4,000 - 5,700","> 5,700")
WYCA_population  %>%
  filter(as.vector(st_within(., WYCA_bound_small_buffer, sparse = FALSE))) %>%
  ggplot() +
      geom_sf(data=WYCA_boundary, colour="black",linewidth=1.5) +
      geom_sf(aes(fill = Pop_density_cat), color = NA) +
      scale_fill_brewer(palette = "Spectral", 
                        direction = -1,
                        labels = labels_pop) +
      labs(fill=" Population per 
        square kilometer
           (quintiles)",
           title = "Population Density across WYCA in 2021") +
      theme_void()
ggsave(file = "WYCA/Images/Population_Density_WYCA.jpg", device = "jpeg")
