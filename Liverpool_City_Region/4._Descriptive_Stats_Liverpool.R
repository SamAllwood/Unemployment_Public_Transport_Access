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
library(gtsummary)
library(kableExtra)
library(officer)
library(flextable)

# 2. Load Data ------------------------------------------------------------
# Read population density shapefile from "PopulationDensityChange.R" script
LCR_pop <- read_sf("../../Data/LCR_population.shp") %>%
  st_transform(4326) %>%
  rename("Pop_Dens_change" = "Pp_dns_",
         "LSOA_Code" = "LSOA21C",
         "LCR_Pop_Dens"  = "P_2021_",
         "LCR_Pop" = "U_P_202") %>%
  dplyr::select(LSOA_Code, Pop_Dens_change, LCR_Pop_Dens, LCR_Pop) %>%
  as.data.frame()

LCR_dataset_descript <- read_csv("../../Data/LCR_dataset_full.csv") %>%
  dplyr::select(-c("Apprent_qual",
            "Level_2_qual", 
            "Level_2_qual", 
            "No_quals",
            "Total_Households",
            "No_Cars", 
            "Number white")) %>%
  rename("White_percent" = "% White",
         "Single_parent_household_rate" = "%",
         "Townsuburb" = "twnsbrb") %>%
  left_join(LCR_pop, by =  "LSOA_Code") %>%
  dplyr::select("LSOA_Code",
                "LSOA_Name",
                "LAD22CD",
                "LSOA_Area",
                "Unemployment_rate",
                "PT_Job_Access_Index",
                "LCR_Pop_Dens",
                "LCR_Pop",
                "Townsuburb",
                "TCITY15",
                "Employed_Population",
                "Economically_active",
                "Unemployed",
                "No_car_rate",
                "White_percent",
                "Single_parent_household_rate",
                "Low_qual_percent",
                "SEC_Management_pc") %>%
  mutate(PT_Job_Access_Index = as.numeric(PT_Job_Access_Index) / 1000,
         Unemployment_rate = as.numeric(Unemployment_rate),
  #       Traveltime_empcent = as.numeric(Traveltime_empcent),
         No_car_rate = as.numeric(No_car_rate),
         White_percent = as.numeric(White_percent),
         Single_parent_household_rate = as.numeric(Single_parent_household_rate),
         Low_qual_percent = as.numeric(Low_qual_percent),
         Townsuburb = as.factor(Townsuburb),
         TownNamed = as.factor(TCITY15),
         LCR_Pop = as.numeric(LCR_Pop),
         LCR_Pop_Dens = as.numeric(LCR_Pop_Dens),
         SEC_Management_pc = as.numeric(SEC_Management_pc))

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


# Descriptive Table
(Descrp_table <- (LCR_dataset_descript %>% 
                                 dplyr::select(-c("LSOA_Code", 
                                           "LSOA_Name", 
                                           "LAD22CD",
                                           "Townsuburb",
                                           "TCITY15",
                                           "TownNamed")) %>%
   tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
        label = list(      LCR_Pop ~ "Population",
                           LCR_Pop_Dens ~ "Population Density (persons / sq.km)",
                      #     TravelTime_Jobcentre ~ "Travel Time to Jobcentre (mins)",
                      #     Traveltime_empcent ~ "Travel Time to Employment Centre (mins)",
                           PT_Job_Access_Index ~ "PTJA Index (/1,000)",
                           Unemployment_rate ~ "Unemployment Rate (%)",
                           No_car_rate ~ "No Car Rate (%)",
                           White_percent ~ "White Ethnicity (%)",
                           Single_parent_household_rate ~ "Single Parent Households (%)",
                           Low_qual_percent ~ "Low Qualified (%)",
                           SEC_Management_pc ~ "Socio-economic Class - Senior Management (%)"
                    #       SEC_Lower_supervisory_routine_pc ~ "Socio-economic Class - Lower Supervisory / Routine Occupations (%)"
                           )) %>%
#  bold_labels() %>%
#  add_overall() %>%
  modify_header(label ~ "Variable" )%>%
#  italicize_levels() %>%
#  modify_caption("Table 1. Summary Statistics for Liverpool City Region Combined Authority (LCR) at Lower Super Output Area (LSOA) Level") %>%
  modify_footnote(everything() ~ NA) ) %>%
  modify_header(stat_0 ~ "Mean (Inter-quartile Range)") %>%
    
  as.data.frame() %>%
  flextable() %>%
    set_table_properties(layout = "autofit",
                         align = "center") )
  
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = Descrp_table)
# Save the Word document
# print(doc, target = "descriptive_table_LCR.docx")

# Calculate higher-level unemployment rates
Overall_unemp_rate <- round((sum(Employment$Unemployed) / sum(Employment$"Economically_active") *100),2)
LCR_unemployment_rate <- round((sum(LCR_dataset_descript$Unemployed) / sum(LCR_dataset_descript$"Economically_active") *100),2)
Town_unemp_rate <- round((LCR_dataset_descript %>%
  filter(Townsuburb == "Urban") %>%
  summarise(Unemployed_sum = sum(Unemployed),
            Economically_active_sum = sum(Economically_active)) %>%
  mutate(Unemployment_rate = (Unemployed_sum / Economically_active_sum) * 100) %>%
  pull(Unemployment_rate)),2)
Suburb_unemp_rate <- round((LCR_dataset_descript %>%
  filter(Townsuburb == "Suburb") %>%
  summarise(Unemployed_sum = sum(Unemployed),
            Economically_active_sum = sum(Economically_active)) %>%
  mutate(Unemployment_rate = (Unemployed_sum / Economically_active_sum) * 100) %>%
  pull(Unemployment_rate)),2)
LCR_jobs <- sum(LCR_dataset_descript$Employed_Population)
LCR_population <- sum(LCR_dataset_descript$Economically_active)
LCR_urban_pop <- sum(LCR_dataset_descript %>%
  filter(Townsuburb == "Urban") %>%
  select(Economically_active))
LCR_suburb_pop <- sum(LCR_dataset_descript %>%
  filter(Townsuburb == "Suburb") %>%
  select(Economically_active))
LCR_urban_jobs <- sum(LCR_dataset_descript %>%
  filter(Townsuburb == "Urban") %>%
  select(Employed_Population))
LCR_suburb_jobs <- sum(LCR_dataset_descript %>%
  filter(Townsuburb == "Suburb") %>%
  select(Employed_Population))
# Create a data frame with the additional rows
additional_rows <- data.frame(
                              TownNamed = c("LCR",
                                            "LCR Urban Areas",
                                            "LCR Suburban Areas",
                                            "England and Wales"),
                              Jobs = c(LCR_jobs,
                                       LCR_urban_jobs,
                                       LCR_suburb_jobs,
                                       NA),
                              Population = c(LCR_population,
                                             LCR_urban_pop,
                                             LCR_suburb_pop,
                                             NA),
                              Jobs_per_person = c(round((LCR_jobs / LCR_population), 2),
                                                  round((LCR_urban_jobs/LCR_urban_pop), 2),
                                                  round((LCR_suburb_jobs/LCR_suburb_pop), 2),
                                                  NA),
                              Unemployment_Rate_Town = c(LCR_unemployment_rate,
                                                         Town_unemp_rate,
                                                         Suburb_unemp_rate,
                                                         Overall_unemp_rate)) %>%
  dplyr::arrange(match(TownNamed, c("LCR Urban Areas",
                              "LCR Suburban Areas",
                              "LCR",
                              "England and Wales")))

# Cities and Towns Table
towns_jobs <- LCR_dataset_descript  %>%
  dplyr::select("TownNamed", 
                "Employed_Population",
                "LCR_Pop",
                "Unemployed",
                "Economically_active") %>%
  group_by(TownNamed) %>%
  summarise(Jobs = sum(Employed_Population), 
            Population = sum(LCR_Pop),
            Jobs_per_person = round((Jobs/Population), 2),
            Unemployment_Rate_Town = round((sum(Unemployed)/sum(Economically_active)*100),2)) %>%
  filter(TownNamed != "Suburb") %>%
  arrange(desc(Jobs_per_person)) %>%
  rbind(additional_rows) %>%
  rename("Jobs per person" = "Jobs_per_person",
         " " = "TownNamed",
         "Unemployment Rate (%)" = "Unemployment_Rate_Town") %>%
  as.data.frame() %>%
  flextable() %>%
  hline(i=c(4,6,7,8)) %>%
  set_table_properties(layout = "autofit",
                       align = "center") %>%
  set_caption("Table 2. Employment and Population in Liverpool City Region Combined Authority (LCRCA) Cities and Towns") 

# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = towns_jobs)
# Save the Word document
# print(doc, target = "towns_jobs_LCR.docx")
