## Script to calculate the parameters to decay attractiveness of jobs by commuting time.
## Logistic decay function is completely described by Mean (mu) and standard deviation (sigma).
## Data downloaded from the National Travel Survey (NTS) 2002-2022.

# Setup libraries
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(stats4)
library(flextable)
library(officer)
library(MASS)

setwd("/Users/samallwood/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access")
# Read datasets. Available from the UK Data Service website.
individual <- read_tsv("../Data/individual_eul_2002-2022.tab") 
trip <- read_tsv("../Data/trip_eul_2002-2022.tab")
household <- read_tsv("../Data/household_eul_2002-2022.tab")

#create subsets of interesting variables
ind_subset <- dplyr::select(individual,
                     IndividualID,
                     HouseholdID,
                     IndWkGOR_B02ID
                    )
trip_subset <- dplyr::select(trip,
                      MainMode_B04ID,
                      IndividualID,
                      TripPurpose_B04ID,
                      TripDisIncSW_B01ID,
                      TripTotalTime,
                      )
household_subset <- dplyr::select(household,
                           HouseholdID,
                           HHoldGOR_B02ID,
                           Settlement2011EW_B04ID
                          )

#join tables by individual ID
data <- left_join(trip_subset, ind_subset, by="IndividualID") %>%
    left_join(household_subset, by="HouseholdID") %>%
  mutate("Year" = substr(IndividualID, 1, 4) ) %>% 
  mutate(Year = as.numeric(Year)) 

# commuting summary data over past 20 years - NorthWest region
commute_summary_NW <- data %>% 
  filter(IndWkGOR_B02ID==2) %>% #filter for NW region
  filter(TripPurpose_B04ID == 1 ) %>% #filter for commuting trips
  group_by(Year) %>%
  summarise(
    Annual_Commutes = n(),
    Annual_PT_Commutes = sum(MainMode_B04ID %in% c(7,8,9,10,11,12,13))) %>%
  mutate(CommutePercentage_NW = (Annual_PT_Commutes / Annual_Commutes) * 100)
# commuting summary data over past 20 years - UK
commute_summary_UK <- data %>% filter(TripPurpose_B04ID == 1 ) %>%
  group_by(Year) %>%
  summarise(
    Annual_Commutes = n(),
    Annual_PT_Commutes = sum(MainMode_B04ID %in% c(7,8,9,10,11,12,13))) %>%
  mutate(CommutePercentage_UK = (Annual_PT_Commutes / Annual_Commutes) * 100)
#summary table
Commuting <- as.data.frame(c(dplyr::select(commute_summary_NW, Year, CommutePercentage_NW),
                dplyr::select(commute_summary_UK,  CommutePercentage_UK))) %>%
  mutate(CommutePercentage_NW = round(CommutePercentage_NW, 1),
         CommutePercentage_UK = round(CommutePercentage_UK, 1)) %>%
  mutate("Year" = as.character(Year)) %>%
  rename("NorthWest \n Region (%)" = CommutePercentage_NW,
         "England (%)" = CommutePercentage_UK) %>%
  flextable() %>%
  set_table_properties(layout = "autofit") %>%
  add_header_lines(values = c("Public Transport Commuting Modeshare in NorthWest England, 2002 - 2022")) %>%
  add_footer_lines(values = c("Source: National Travel Survey 2002-2022")) 


# Create a Word document with table
doc <- read_docx()
doc <- body_add_flextable(doc, value = Commuting)
print(doc, target = "Final_Report_(GMCA)/Commuting.docx")

# Some summary figures
number_commutes <- data %>% filter(TripPurpose_B04ID == 1) %>% nrow()
num_PT_commutes <- data %>% filter(TripPurpose_B04ID == 1) %>% filter(MainMode_B04ID %in% c(7,8,9,10,11,12,13)) %>% nrow()
commute_PT_pc <- num_PT_commutes/number_commutes*100

# Calculate max walk time cutoff ------------------------------------------
# Filter only the walking trips and commuting purpose
walking_trips <- data %>% filter(MainMode_B04ID == 1) %>% filter(TripPurpose_B04ID==1)
# Calculate the 99th percentile of the 'TripTotalTime' column
quantile_walk_99 <- quantile(walking_trips$TripTotalTime, 0.99)

# Calculate Commuting Time ------------------------------------------------
commuting_trips_A <- data %>% filter(TripPurpose_B04ID==1) # leaves 824436 trips
# filter for NorthWest region
commuting_trips_NW <- commuting_trips_A %>% filter(IndWkGOR_B02ID==2) # leaves 91914 trips
# filter for public transport trips
commuting_trips_NW_PT <- commuting_trips_NW %>% filter(MainMode_B04ID %in% c(7,8,9,10,11,12,13)) # leaves 11680 trips
# Calculate the 99th percentile of the 'TripTotalTime' column
quantile_commute_99 <- quantile(commuting_trips_NW_PT$TripTotalTime, 0.99) # this calculation shows 
                                                                     # 1% of the commute times exceed 120mins
# filter realistic daily commuting durations (between 1 and 120mins) - leaves 11568 trips
commuting_trips <- commuting_trips_NW_PT %>% 
  filter(TripTotalTime<=121) %>%
  filter(TripTotalTime>1)

# plot histograms and density plots
ggplot(data = commuting_trips, aes(x=TripTotalTime)) + 
  geom_histogram(bins=8) 
ggplot(data = commuting_trips, aes(x=TripTotalTime)) + 
  geom_density() 

# Compute the empirical cumulative distribution function
ecdf_func <- ecdf(commuting_trips$TripTotalTime)

# Create a data frame for plotting
plot_data <- data.frame(TripTotalTime = unique(commuting_trips$TripTotalTime),
                        CDF = ecdf_func(unique(commuting_trips$TripTotalTime)))

# Fit a logistic distribution to travel data
fit <- fitdistr(commuting_trips$TripTotalTime, "logistic")

mean <- fit$estimate[1] # 39.7
sd <- fit$estimate[2] # 12.6


# PLOTS
# Fit a logistic decay function to the data
logistic_decay <- function(x, mu, sigma) {
  1 / (1 + exp(-(x - mu) / sigma))
}

# Fit a logistic decay function to the data
# Generate the estimated curve using the logistic decay function
x_vals <- seq(min(commuting_trips$TripTotalTime, na.rm = TRUE), 
              max(commuting_trips$TripTotalTime, na.rm = TRUE), 
              length.out = 100)
y_vals <- logistic_decay(x_vals, mean, sd)
log_decay_data <- data.frame(x = x_vals, y = y_vals)

# Plot the CDF
CDF <- ggplot(plot_data, aes(x = TripTotalTime, y = CDF)) +
  geom_line(data = plot_data, aes(x = TripTotalTime, y = CDF, color = "CDF"), linewidth = 1) +
  geom_line(data = log_decay_data, aes(x = x, y = y, color = "Estimated Logistic Decay Curve"), size = 1) +
  theme_stata() +
  scale_color_manual(values = c("CDF" = "blue", "Estimated Logistic Decay Curve" = "red")) +
  labs(x = "Trip Total Time", 
       y = "Density", 
       title = "Cumulative Density Function (CDF) of Trip Total Time \n and Calculated Logistic Decay Curve",
       caption = "Red line shows the curve calculated from the derived parameters, and the blue line is the 
       cumulative density function of the actual commuting travel data. \n Data Source: National Travel Survey 2002-2022"
       )

ggsave("0.CumDenFun_LogisticDecay.jpeg", CDF, width = 10, height = 6)
