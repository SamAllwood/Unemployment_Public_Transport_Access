## Script to calculate the parameters to decay the commuting time distribution
## according to a logistic decay function. Mean (mu) and standard deviation (sigma) 
## are required to fully define the logistic decay function.
## Data downloaded from the National Travel Survey (NTS) 2002-2022.

# Setup libraries
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(stats4)
library(flextable)
library(officer)
library(MASS)
library(here)

setwd(here::here())
# Read datasets. These are too large for github so will need downloading directly 
# from the UK Data Service website.

individual <- read_tsv("Data/UKDA-5340-tab/tab/individual_eul_2002-2022.tab") 
trip <- read_tsv("Data/UKDA-5340-tab/tab/trip_eul_2002-2022.tab")
household <- read_tsv("Data/UKDA-5340-tab/tab/household_eul_2002-2022.tab")

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

# commuting summary data over past 20 years - NW
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
  rename("NorthWest" = CommutePercentage_NW,
         "UK" = CommutePercentage_UK) %>%
  flextable() %>%
  set_table_properties(width = 1, layout = "autofit")
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = Commuting)
# Save the Word document
print(doc, target = "../Final_Report/Commuting.docx")


number_commutes <- data %>% filter(TripPurpose_B04ID == 1) %>% nrow()
num_PT_commutes <- data %>% filter(TripPurpose_B04ID == 1) %>% filter(MainMode_B04ID %in% c(7,8,9,10,11,12,13)) %>% nrow()
commute_PT_pc <- num_PT_commutes/number_commutes*100

# Calculate max walk time cutoff ------------------------------------------
# Filter only the walking trips and commuting purpose
walking_trips <- data %>% filter(MainMode_B04ID == 1) %>% filter(TripPurpose_B04ID==1)
# Calculate the 99th percentile of the 'TripTotalTime' column
quantile_walk_99 <- quantile(walking_trips$TripTotalTime, 0.99)

# Calculate Commuting Time ------------------------------------------------
#filter for commuting trips
commuting_trips_A <- data %>% filter(TripPurpose_B04ID==1) # leaves 824436 trips
#filter for NorthWest region
commuting_trips_NW <- commuting_trips_A %>% filter(IndWkGOR_B02ID==2) # leaves 91914 trips
#filter for public transport trips
commuting_trips_NW_PT <- commuting_trips_NW %>% filter(MainMode_B04ID %in% c(7,8,9,10,11,12,13)) # leaves 11680 trips
# Calculate the 99th percentile of the 'TripTotalTime' column
quantile_commute_99 <- quantile(commuting_trips_NW_PT$TripTotalTime, 0.99) # this calculation shows 
                                                                     # 1% of the commute times exceed 120mins
#filter realistic daily commuting durations (between 1 and 120mins) - leaves 11568 trips
commuting_trips <- commuting_trips_NW_PT %>% 
  filter(TripTotalTime<=121) %>%
  filter(TripTotalTime>1)

#plot histograms and density plots
ggplot(data = commuting_trips, aes(x=TripTotalTime)) + 
  geom_histogram(bins=8) 
ggplot(data = commuting_trips, aes(x=TripTotalTime)) + 
  geom_density() 

# Compute the empirical cumulative distribution function
ecdf_func <- ecdf(commuting_trips$TripTotalTime)

# Create a data frame for plotting
plot_data <- data.frame(TripTotalTime = unique(commuting_trips$TripTotalTime),
                        CDF = ecdf_func(unique(commuting_trips$TripTotalTime)))

# Plot the CDF
ggplot(plot_data, aes(x = TripTotalTime, y = CDF)) +
  geom_line() +
  theme_stata() +
  labs(x = "Trip Total Time", y = "CDF", title = "CDF of Trip Total Time")


# Fit a logistic distribution to your travel cost data
fit <- fitdistr(commuting_trips$TripTotalTime, "logistic")

mean <- fit$estimate[1]
sd <- fit$estimate[2]

# log transform data
log_trips <- log(commuting_trips$TripTotalTime)

#plot histograms and density plots of log-transformed data
ggplot(data = commuting_trips, aes(x=log_trips)) + 
  geom_histogram(bins=8) 

ggplot(data = commuting_trips, aes(x=log_trips)) + 
  geom_density() 
# log-transforming data seems to make the distribution look more normal, so will continue with lognormal distribution

# Define likelihood function
log_likelihood <- function(mu, sigma) {
  -sum(dlnorm(log_trips, meanlog = mu, sdlog = sigma, log = TRUE)) 
}

# Optimization
mle_fit <- mle(log_likelihood, start = list(mu = 1, sigma = 1)) # requires no zero values in the log_trips dataset, 
                                                                # so trip durations must be greater than 1

# Estimated parameters
mu_estimated <- as.numeric(mle_fit@coef[1])
sigma_estimated <- as.numeric(mle_fit@coef[2])

summary(mle_fit)

#generate samples
n_samples <- length(commuting_trips$TripTotalTime) 

# Generate samples from the fitted log-normal distribution
simulated_times <- rlnorm(n = n_samples, meanlog = mu_estimated, sdlog = sigma_estimated)

#calculate and plot theoretical values
theor_times <- seq(min(log_trips), max(log_trips), length.out = 100)
theor_density <- dlnorm(theor_times, meanlog = mu_estimated, sdlog = sigma_estimated) 
ggplot(data = commuting_trips, aes(x = log_trips)) +
  geom_density(alpha = 0.5, fill = "lightblue") +  # Density plot of data (semi-transparent)
  geom_line(data = data.frame(theor_times, theor_density), 
            aes(x = theor_times, y = theor_density), 
            color = "green", size = 1)+
  geom_density(data = data.frame(simulated_times), 
               aes(x = simulated_times), color = "red", size = 1) # Log-normal curve, this is duplicated but leaving it in anyway

residuals <- log_trips-simulated_times

#Predicted values on log-scale (tried just using the conditional mean but didn't give any range of fitted values obviously)
predicted_log_values <- dlnorm(log_trips, meanlog = mu_estimated, sdlog = sigma_estimated, log=FALSE) 

# Exponentiate to get predictions on the original scale
predicted_values <- exp(predicted_log_values)

simulated_times_sort <- sort(simulated_times)
log_trips_sort <- sort(log_trips)
residuals <- log_trips_sort - simulated_times_sort

#plot residuals vs fitted plots
plot(simulated_times, residuals, xlab = "Simulated Times", ylab = "Residuals")+
  abline(h = 0, col = "red")  # Add a horizontal line at zero 

qqnorm(residuals,
       main = "Model 4")+
  qqline(residuals)
