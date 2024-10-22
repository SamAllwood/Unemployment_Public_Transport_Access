## Script to combine all datasets for analysis
# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(tidytransit)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(classInt)
library(readODS)
library(accessibility)

setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access/Liverpool_City_Region")

# 2. Load Data ------------------------------------------------------------

# Load Liverpool Geo data from file
LCR_dataset <- read_sf("../../Data/LCR_dataset.shp") %>%
                                   rename("LSOA_Code" = "LSOA21C",
                                          "LSOA_Name" = "LSOA21N",
                                          "PT_Job_Access_Index" = "PT_Jb_A_I",
                                          "PT_Job_Access_Index_Bus" = "PT_J_A_I_B",
                                          "PT_Job_Access_Index_Tram" = "PT_J_A_I_T",
                                          "PT_Job_Access_Index_Walk" = "PT_J_A_I_W",
                                          "Employed_Population" = "Employd",
                                          "LSOA_Area" = "LSOA__2",
                                          "Traveltime_empcent" = "Tr__E_C",
                                          "Traveltime_CC" = "Trvl_CC",
                                          "PT_Job_Access_Index_Demand" = "PT_Jb_A_I_")
towns_centres_LCR <- read_sf("../../Data/towns_centres_LCR.shp") 

# LCR Boundary + buffer
Boundaries <- read_sf("../../Data/CAUTH_DEC_2023_EN_BFC.shp")
LCR_boundary <- Boundaries %>% filter(CAUTH23NM == "Liverpool City Region") %>%
  st_transform(4326) 
LCR_bound_small_buffer <- LCR_boundary %>% st_buffer(dist=25)

towns_centres_LCR <- towns_centres_LCR %>% filter(as.vector(st_within(., LCR_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)
# Read Local Authority District (LAD) boundaries
LADs <- read_sf("../../Data/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within LCR
LADs_LCR <- LADs %>% filter(as.vector(st_within(., LCR_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

# Output area lookup
Output_Area_Lookup <- read_csv("../../Data/Output_Area_to_Lower_layer_Super_Output_Area_to_Middle_layer_Super_Output_Area_to_Local_Authority_District_(December_2021)_Lookup_in_England_and_Wales_v3.csv") %>%
  dplyr::select(LSOA21CD, MSOA21CD, LAD22CD) %>%
  distinct(LSOA21CD, .keep_all=TRUE)

# Jobcentres Data - downloaded and filtered in TravelTimeMatrix.R
Jobcentre_plus_geocoded <- read_csv("../../Data/Jobcentre_locations_geocoded.csv")
# Convert job centres to simple feature 
Jobcentre_plus_geocoded <- Jobcentre_plus_geocoded %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
# Filter jobcentres within LCR
Jobcentres_within_LCR <- Jobcentre_plus_geocoded %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(as.vector(st_within(., LCR_boundary, sparse = FALSE)))


# Unemployment Levels
Employment <-read_csv("../../Data/census2021-ts066-lsoa.csv") %>%
  dplyr::select("geography",
         "geography code",
         "Economic activity status: Total: All usual residents aged 16 years and over",
         "Economic activity status: Economically active (excluding full-time students)",
         "Economic activity status: Economically active (excluding full-time students): Unemployed") %>%
  rename("LSOA_name" = "geography",
         "id" = "geography code",
         "Population (16 and over)" =  "Economic activity status: Total: All usual residents aged 16 years and over",
         "Economically_active" = "Economic activity status: Economically active (excluding full-time students)",
         "Unemployed" = "Economic activity status: Economically active (excluding full-time students): Unemployed") %>%
  mutate(Unemployment_rate = Unemployed*100/Economically_active)


#Car ownership rate
Car_ownership <- read_csv("../../Data/census2021-ts045-lsoa.csv") %>%
  dplyr::select("geography",
         "geography code",
         "Number of cars or vans: Total: All households",
         "Number of cars or vans: No cars or vans in household") %>%
  rename("LSOA_name" = "geography",
         "id" = "geography code",
         "Total_Households" = "Number of cars or vans: Total: All households",
         "No_Cars" = "Number of cars or vans: No cars or vans in household") %>%
  mutate(No_car_rate = No_Cars*100/Total_Households)

# Ethnicity
Ethnicity <- read_csv("../../Data/census2021-ts021-lsoa.csv", skip=6) %>%
  dplyr::select("2021 super output area - lower layer",
         "White",
         "%...11") %>%
  rename("% White" = "%...11",
         "Number white" = "White",
         "LSOA_Code" = "2021 super output area - lower layer") %>%
  drop_na() %>%
  mutate(LSOA_Code = sapply(strsplit(as.character(LSOA_Code), " : "), "[", 1))

# Single-parent household
SingleParent <- read_csv("../../Data/census2021-ts003-lsoa.csv", skip=6) %>%
  rename("LSOA_Code" = "2021 super output area - lower layer",
         "Single_Parent_Households" = "2021") %>%
  drop_na() %>%
  mutate(LSOA_Code = sapply(strsplit(as.character(LSOA_Code), " : "), "[", 1))

# NS-Socio-Economic Classification
NS_SEC <- read_csv("../../Data/census2021-ts062-lsoa.csv", skip=0) %>%
  rename(LSOA_Code = "geography code",
         Total = "National Statistics Socio-economic Classification (NS-SEC): Total: All usual residents aged 16 years and over",
         L1_L2_L3 = "National Statistics Socio-economic Classification (NS-SEC): L1, L2 and L3 Higher managerial, administrative and professional occupations",
         L4_L5_L6 = "National Statistics Socio-economic Classification (NS-SEC): L4, L5 and L6 Lower managerial, administrative and professional occupations",
         L7 = "National Statistics Socio-economic Classification (NS-SEC): L7 Intermediate occupations",
         L8_L9 = "National Statistics Socio-economic Classification (NS-SEC): L8 and L9 Small employers and own account workers",
         L10_L11 = "National Statistics Socio-economic Classification (NS-SEC): L10 and L11 Lower supervisory and technical occupations",
         L12 = "National Statistics Socio-economic Classification (NS-SEC): L12 Semi-routine occupations",
         L13 = "National Statistics Socio-economic Classification (NS-SEC): L13 Routine occupations",
         L14 = "National Statistics Socio-economic Classification (NS-SEC): L14.1 and L14.2 Never worked and long-term unemployed",
        Students = "National Statistics Socio-economic Classification (NS-SEC): L15 Full-time students") %>% 
  mutate(
    SEC_Upper_Management_pc = (L1_L2_L3)/Total*100,
    SEC_Management_pc = (L1_L2_L3 + L4_L5_L6)/Total*100,
    SEC_Intermediate_pc = L7/Total*100,
    SEC_Small_Employers_Self_Employed_pc = L8_L9/Total*100,
    SEC_Lower_supervisory_routine_pc = (L10_L11 + L12 + L13)/Total*100,
    SEC_long_term_unemployed_pc = L14/Total*100) %>%
  dplyr::select(LSOA_Code, 
                SEC_Upper_Management_pc, 
                SEC_Management_pc, 
                SEC_Intermediate_pc, 
                SEC_Small_Employers_Self_Employed_pc, 
                SEC_Lower_supervisory_routine_pc,
                SEC_long_term_unemployed_pc) 


# Qualification levels
Qualifications <- read_csv("../../Data/census2021-ts067-lsoa.csv") %>%
  dplyr::select("geography code",
         "Highest level of qualification: Total: All usual residents aged 16 years and over",
         "Highest level of qualification: No qualifications",
         "Highest level of qualification: Level 1 and entry level qualifications",
         "Highest level of qualification: Level 2 qualifications",
         "Highest level of qualification: Apprenticeship") %>%
  rename("LSOA_Code" = "geography code",
         "Total_residents_over_16" = "Highest level of qualification: Total: All usual residents aged 16 years and over",
         "No_quals" = "Highest level of qualification: No qualifications",
         "Level_1_qual" = "Highest level of qualification: Level 1 and entry level qualifications",
         "Level_2_qual" = "Highest level of qualification: Level 2 qualifications",
         "Apprent_qual" = "Highest level of qualification: Apprenticeship") %>%
  mutate(Low_Qualified = 
         No_quals+
         Level_1_qual+
         Level_2_qual+
         Apprent_qual) %>%
  mutate(Low_qual_percent = 
           Low_Qualified/Total_residents_over_16*100)

# Urban rural classifications
Urban_Rural <- read_ods("../../Data/Rural_Urban_Classification_2011_lookup_tables_for_small_area_geographies.ods", 
                        sheet = "LSOA11", 
                        skip=2) %>%
  rename("LSOA11CD" = "Lower Super Output Area 2011 Code",
         "LSOA11_Name" = "Lower Super Output Area 2011 Name",
         "Urban_Rural" = "Rural Urban Classification 2011 (2 fold)") %>%
  dplyr::select("LSOA11CD",
         "LSOA11_Name",
          "Urban_Rural") 

# Load 2011-2021 LSOA lookup table
LSOA_lookup <- read_csv("../../Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv")
# Lookup 2021 LSOA codes
# left join 2011 Urban Rural classification if LSOA is split into two in 2021. 
# Split LSOAs - population split between new LSOAs generated in 2011
# Join the LSOA lookup table to the dataset
Urban_Rural_2021 <- Urban_Rural %>%
  left_join(LSOA_lookup, by =  "LSOA11CD") 
# Remove duplicates for split LSOAs
Urban_Rural_2021 <- Urban_Rural_2021 %>%
  distinct(LSOA21CD, .keep_all = TRUE)

# 3. Data Cleaning --------------------------------------------------------
# Join datasets
LCR_dataset_full <- left_join(LCR_dataset, Employment, by = c("LSOA_Code" = "id"), keep=FALSE) %>%
  left_join(Car_ownership, by = c("LSOA_Code" = "id"), keep=FALSE) %>%
  left_join(Ethnicity, by = c("LSOA_Code" = "LSOA_Code"), keep=FALSE) %>%
  left_join(SingleParent, by = c("LSOA_Code" = "LSOA_Code"), keep=FALSE) %>%
  left_join(Urban_Rural_2021, by = c("LSOA_Code" = "LSOA21CD"), keep=FALSE) %>%
  left_join(Qualifications, by = c("LSOA_Code" = "LSOA_Code"), keep=FALSE) %>%
  left_join(NS_SEC, by = c("LSOA_Code" = "LSOA_Code"), keep=FALSE) %>%
  dplyr::select(.,-c("LSOA_name.x","LSOA_name.y","LAD22NMW", LAD22NM, )) %>%
  left_join(Output_Area_Lookup, by = c("LSOA_Code" = "LSOA21CD"), keep=FALSE) %>%
  mutate(Total_residents = as.numeric(Total_residents_over_16),
         LSOA_Area = as.numeric(LSOA_Area)) %>%
  rename(LAD22CD = LAD22CD.x)

# Write dataset to csv
write_csv(LCR_dataset_full, "../../Data/LCR_dataset_full.csv")

LCR_dataset_full %>% dplyr::select ( -c(
                                   "Population (16 and over)",
                                   Total_Households,
                                   No_Cars, 
                                   "Number white",
                                   LSOA11CD,
                                   LSOA11_Name,
                                   LSOA11NM,
                                   LSOA21NM,
                                   ObjectId,
                                   No_quals,
                                   Level_1_qual,
                                   Level_2_qual,
                                   Apprent_qual
                                )) %>%
  st_write( "../../Data/LCR_dataset_full_sf.shp", append = FALSE)

# Calculate higher-level unemployment rates
Overall_unemp_rate <- sum(Employment$Unemployed) / sum(Employment$"Economically_active") *100
LCR_unemp_rate <- sum(LCR_dataset_full$Unemployed) / sum(LCR_dataset_full$"Economically_active") *100
Town_unemp_rate <- LCR_dataset_full %>%
  filter(twnsbrb == "Urban") %>%
  summarise(Unemployed_sum = sum(Unemployed),
            Economically_active_sum = sum(Economically_active)) %>%
  mutate(Unemployment_rate = (Unemployed_sum / Economically_active_sum) * 100) %>%
  pull(Unemployment_rate)
Suburb_unemp_rate <- LCR_dataset_full %>%
  filter(twnsbrb == "Suburb") %>%
  summarise(Unemployed_sum = sum(Unemployed),
            Economically_active_sum = sum(Economically_active)) %>%
  mutate(Unemployment_rate = (Unemployed_sum / Economically_active_sum) * 100) %>%
  pull(Unemployment_rate)

