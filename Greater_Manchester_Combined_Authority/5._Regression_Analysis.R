## Script for fixed effects regression analysis and instrumental variables
## And summary tables for final report.
# 1. Setup ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(mapview)
library(ggmap)
library(sf)
library(ggplot2)
library(nlme)
library(lfe)
library(broom)
library(car)
library(lmtest)
library(lindia)
library(spdep)
library(AER)
library(MuMIn)
library(flextable)
library(officer)
library(texreg)
library(systemfit)
library(patchwork)
library(sandwich)
library(stats)
library(gtsummary)
library(stargazer)
setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access/Greater_Manchester_Combined_Authority")

# 2. Load Data ------------------------------------------------------------
MANCH_dataset_full <- read_csv("../../Data/MANCH_dataset_full.csv") %>%
  rename("White_percent" = "% White",
         "Single_parent_household_rate" = "%",
         "Townsuburb" = "twnsbrb",
         "TownNamed" = "TCITY15") 

MANCH_pop_regress <- read_sf("../../Data/MANCH_population.shp") %>%
  st_transform(4326) %>%
  rename("LSOA_Code" = "LSOA21C",
         "Pop_Dens" = "P_2021_",
         "Population" = "U_P_202") %>%
    dplyr::select(LSOA_Code, Pop_Dens, Population, geometry)

MANCH_data_regress <- MANCH_pop_regress %>%
  left_join(MANCH_dataset_full, by="LSOA_Code", keep=FALSE) %>%
  dplyr::select("LSOA_Code", 
         "LSOA_Name", 
         "LAD22CD",
         "TownNamed",
         "TravelTime_Jobcentre", 
         "PT_Job_Access_Index",
         "Unemployment_rate", 
         "Traveltime_empcent",
         "Traveltime_CC",
         "No_car_rate",
         "White_percent",
         "Single_parent_household_rate",
         "Low_qual_percent",
         "Pop_Dens",
         "Population",
         "Townsuburb",
         "SEC_Upper_Management_pc",
         "SEC_Management_pc", 
         "SEC_Intermediate_pc", 
         "SEC_Small_Employers_Self_Employed_pc", 
         "SEC_Lower_supervisory_routine_pc",
         "SEC_long_term_unemployed_pc",
         "MSOA21CD",
         "PT_Job_Access_Index_Demand"
         ) %>%
  mutate(TravelTime_Jobcentre = as.numeric(TravelTime_Jobcentre),
         PT_Job_Access_Index = as.numeric(PT_Job_Access_Index)/1000,
         Unemployment_rate = as.numeric(Unemployment_rate),
         Traveltime_empcent = as.numeric(Traveltime_empcent),
         No_car_rate = as.numeric(No_car_rate),
         White_percent = as.numeric(White_percent),
         Single_parent_household_rate = as.numeric(Single_parent_household_rate),
         Low_qual_percent = as.numeric(Low_qual_percent),
         Pop_Dens = as.numeric(Pop_Dens),
         Population = as.numeric(Population),
         Townsuburb = as.factor(Townsuburb),
         TownNamed = as.factor(TownNamed),
         SEC_Upper_Management_pc = as.numeric(SEC_Upper_Management_pc),
         SEC_Management_pc = as.numeric(SEC_Management_pc), 
         SEC_Intermediate_pc = as.numeric(SEC_Intermediate_pc), 
         SEC_Small_Employers_Self_Employed_pc = as.numeric(SEC_Small_Employers_Self_Employed_pc), 
         SEC_Lower_supervisory_routine_pc = as.numeric(SEC_Lower_supervisory_routine_pc),
         MSOA21CD = as.factor(MSOA21CD),
         PT_Job_Access_Index_Demand = as.numeric(PT_Job_Access_Index_Demand)
         )

# GMCA Boundary + buffer
Boundaries <- read_sf("../../Data/CAUTH_DEC_2023_EN_BFC.shp")
GMCA_boundary <- Boundaries %>% filter(CAUTH23NM == "Greater Manchester") %>%
  st_transform(4326) 
GMCA_bound_small_buffer <- GMCA_boundary %>% st_buffer(dist=25)

# Read Local Authority District (LAD) boundaries
LADs <- read_sf("../../Data/LAD_DEC_2021_GB_BFC.shp") %>%
  st_transform(4326)
# Filter LADs within GMCA
LADs_MANCH <- LADs %>% filter(as.vector(st_within(., GMCA_bound_small_buffer, sparse = FALSE))) %>% 
  st_transform(4326)

## Tidy up districts with overlapping boundaries manually
MANCH_data_regress$District <- 
  ifelse(MANCH_data_regress$TownNamed=="Suburb",
         MANCH_data_regress$LAD22CD,
         MANCH_data_regress$TownNamed) 
MANCH_data_regress <- MANCH_data_regress %>%
  mutate(District = ifelse(LSOA_Code == "E01004789", "E08000002", District),
         District = ifelse(LSOA_Code == "E01005386", "E08000008", District),
         District = ifelse(LSOA_Code == "E01005440", "E08000008", District),
         
         District = ifelse(LSOA_Code == "E01005084", "E08000008", District),
         District = ifelse(LSOA_Code == "E01005082", "E08000008", District),
         District = ifelse(LSOA_Code == "E01005157", "E08000009", District),
         )

count(MANCH_data_regress %>% distinct(LSOA_Code))

MANCH_districts_joined <- MANCH_data_regress %>% distinct(LSOA_Code, .keep_all=TRUE) %>%
  group_by(District) %>%
  summarize(geometry = st_union(geometry.x))

# Plot fixed effect areas with outlines only 
(FE_boundaries <- MANCH_districts_joined %>%
    ggplot() +
    geom_sf( aes(fill = "yellow"), color = "black", size = 1) + # Add outlines for each District
    labs(title = "Fixed Effect Area Outlines") +
    theme_void() +
    theme(legend.position = "none"))
ggsave("Images/FE_areas_GMCA.jpeg", plot = FE_boundaries, units = "cm")

# 3. Assess need for multilevel regression --------------------------------
# Base model (intercept only) is compared with fixed effects at LAD, town-named or combination 
# of the two geographical areas (which is termed 'district' in this analysis) to see which reduces the variance
# the most.
intercept_only <- gls(Unemployment_rate ~1, 
                      data=MANCH_data_regress, 
                      method="ML")
random_intercept_LAD <- lme(Unemployment_rate ~1, 
                             data=MANCH_data_regress, 
                             random = ~1|LAD22CD, 
                             method="ML")
random_intercept_town <- lme(Unemployment_rate ~1, 
                             data=MANCH_data_regress, 
                             random = ~1|TownNamed, 
                             method="ML")

random_intercept_district <- lme(Unemployment_rate ~1, 
                             data=MANCH_data_regress, 
                             random = ~1|District, 
                             method="ML")
# as an extensions the MSOA level was tested to see if any advantage was inferred, 
# which it wasn't.
random_intercept_MSOA <- lme(Unemployment_rate ~1, 
                                 data=MANCH_data_regress, 
                                 random = ~1|MSOA21CD, 
                                 method="ML") 

# Comparision:
anova <- anova(intercept_only, random_intercept_LAD, random_intercept_town, random_intercept_district,random_intercept_MSOA)
# Comparison of selected fixed effect level with intercept only to quantify how much benefit is inferred by fixed effects modelling:
anova_district <- anova(intercept_only, random_intercept_district)

FE_areas <- anova %>% as.data.frame() %>%
  dplyr::select(Model, df, AIC) %>%
  mutate(AIC = round(AIC, 0)) %>%
  rename("Degrees of Freedom" = df) %>%
  mutate(Model = c("Intercept Only", "LAD Fixed Effects", "Town and City Fixed Effects", "Combination Fixed Effects", "MSOA Fixed Effects"),
         "p-value" = c("NA", "<0.001", "<0.001", "<0.001", "<0.001")) %>%
 flextable() %>%
  set_caption("Comparison of Fixed Effect Geographies") 
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = FE_areas)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/FE_areas_table_GMCA.docx")


# note BIC has reduced from 8744 to 8393 when allowing the intercept to vary by
# a combination of urban centre and local authority district, which is a larger reduction than
# either local authority district or urban centre individually. This means the model accuracy is 
# slightly higher when allowing the intercepts to vary. The improved model fit is statistically significant (p<0.0001)
# i.e. there is variation between Local Authority Districts and therefore a multilevel modelling 
# is appropriate.

# 4. Multilevel regression ------------------------------------------------
multilevel_model1 <- lme(Unemployment_rate ~ 
                          PT_Job_Access_Index,  
                          data=MANCH_data_regress, 
                        random = ~1|District, 
                        method="ML")

multilevel_model2 <- lme(Unemployment_rate ~ 
                          PT_Job_Access_Index + 
                          No_car_rate, 
                        data=MANCH_data_regress, 
                        random = ~1|District, 
                        method="ML")

multilevel_model3 <- lme(Unemployment_rate ~ 
                          PT_Job_Access_Index + 
                          No_car_rate + 
                          White_percent, 
                        data=MANCH_data_regress, 
                        random = ~1|District, 
                        method="ML")

multilevel_model4 <- lme(Unemployment_rate ~ 
                          PT_Job_Access_Index + 
                          No_car_rate + 
                          White_percent + 
                          Single_parent_household_rate,
                        data=MANCH_data_regress, 
                        random = ~1|District, 
                        method="ML")

multilevel_model5 <- lme(Unemployment_rate ~ 
                           PT_Job_Access_Index + 
                           No_car_rate + 
                           White_percent + 
                           Single_parent_household_rate + 
                           Low_qual_percent,
                         data=MANCH_data_regress, 
                         random = ~1|District, 
                         method="ML")

# compare all models
anova(intercept_only, random_intercept_district, multilevel_model1, multilevel_model2,
      multilevel_model3, multilevel_model4, multilevel_model5)
# compare models with and without interaction term
anova(intercept_only, random_intercept_district, multilevel_model5)

# 5. Add Instrumental Variable to Fixed effects regression Model ------------------------------
# IV = Population Density - check for strength of instrument
IV_model <- lm(PT_Job_Access_Index ~ Pop_Dens, data=MANCH_data_regress)
summary_IV <- summary(IV_model)  

IV_model_2 <- lm(No_car_rate ~ SEC_Management_pc, data=MANCH_data_regress)
summary_IV2 <- summary(IV_model_2) 

# Relationship between endo. var. and IV is significant (p<0.0001) and has a positive relationship

# Scatter plot of relationship between endo. var. and IV 
ggplot(MANCH_data_regress, aes(x=Pop_Dens, y=PT_Job_Access_Index)) + 
  geom_point() + 
  geom_smooth(method="lm")
# IV is significant (p<0.0001) and has a positive relationship with endogenous variable
# Correlation coefficient for IV and endog. var.
(correlation_IV <- cor(MANCH_data_regress$PT_Job_Access_Index, MANCH_data_regress$Pop_Dens,
  method="pearson"))
# IV is strongly correlated with endogenous variable (cor=0.47) 

# Scatter plot and correlation of No-car rate and SEC: management rate
# I.e. second endogenous variable and second IV
ggplot(MANCH_data_regress, aes(x=No_car_rate, y=SEC_Management_pc)) + 
  geom_point() + 
  geom_smooth(method="lm")
correlation_IV2 <- cor(MANCH_data_regress$No_car_rate, MANCH_data_regress$SEC_Management_pc,
    method="pearson")
# Scatter plot of SEC: upper management rate and SEC: long term unemployed
ggplot(MANCH_data_regress, aes(x=SEC_Upper_Management_pc, y=SEC_long_term_unemployed_pc)) + 
  geom_point() + 
  geom_smooth(method="lm")
correlation_IV_endog <- cor(MANCH_data_regress$SEC_long_term_unemployed_pc, MANCH_data_regress$SEC_Upper_Management_pc,
                       method="pearson")

# Instrument Strength table
Ins_strength <- data.frame(
  Model = c("PTJA ~ Population Density", "No-car rate ~ SEC Management"),
  "FStat" = c(round(summary_IV$fstatistic[1], 0), round(summary_IV2$fstatistic[1], 0)),
  "DoF"= c(round(summary_IV$fstatistic[3], 0), round(summary_IV2$fstatistic[3], 0)),
  "CorCoeff" = c(round(correlation_IV, 2), round(correlation_IV2, 2))
) %>%
  rename("Endog.Var. ~ IV" = Model,
         "F-Statistic" = FStat,
         "Degrees of Freedom" = DoF,
         "Pearson Correlation Coefficient" = CorCoeff) %>%
    flextable() %>%
    set_table_properties( width = 1, layout = "autofit")

# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = Ins_strength)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/Ins_strength_GMCA.docx")

# FE model (felm)
FE_Model <- felm(Unemployment_rate ~ PT_Job_Access_Index +
                      No_car_rate +
                      White_percent + 
                      Single_parent_household_rate + 
                      Low_qual_percent 
                    | District | 0 | 0, MANCH_data_regress)
summary(FE_Model)
FE_Model_MSOA <- felm(Unemployment_rate ~ PT_Job_Access_Index +
                   No_car_rate +
                   White_percent + 
                   Single_parent_household_rate + 
                   Low_qual_percent 
                 | MSOA21CD | 0 | 0, MANCH_data_regress)
summary(FE_Model_MSOA)

 # presenting robust standard errors

# FE_LL model (felm)
FE_LL_Model <- felm(log(Unemployment_rate) ~ PT_Job_Access_Index +
                   No_car_rate +
                   White_percent + 
                   Single_parent_household_rate + 
                   Low_qual_percent 
               | District | 0 | 0, MANCH_data_regress)
summary(FE_LL_Model)

FE_LL_Model_demand <- felm(log(Unemployment_rate) ~ PT_Job_Access_Index_Demand +
                      No_car_rate +
                      White_percent + 
                      Single_parent_household_rate + 
                      Low_qual_percent 
                    | District | 0 | 0, MANCH_data_regress)
summary(FE_LL_Model_demand)

FE_LL_Model_MSOA <- felm(log(Unemployment_rate) ~ PT_Job_Access_Index +
                      No_car_rate +
                      White_percent + 
                      Single_parent_household_rate + 
                      Low_qual_percent 
                    | MSOA21CD | 0 | 0, MANCH_data_regress)
summary(FE_LL_Model_MSOA)

coeftest(FE_LL_Model, vcov = vcovHC(FE_LL_Model, type = 'HC0')) # presenting robust standard errors

# FE_LL model with IVs
FE_LL_Model_IV <- felm(log(Unemployment_rate) ~ 
                            White_percent + 
                            Single_parent_household_rate + 
                            Low_qual_percent 
                           | District 
                           | (PT_Job_Access_Index+ No_car_rate ~ Pop_Dens+SEC_Management_pc) 
                           | 0, MANCH_data_regress)
summary(FE_LL_Model_IV)
# coeftest(FE_LL_Model_IV, vcov = vcovHC(FE_LL_Model_IV, type = 'HC0')) # apply robust standard errors, doesn't work

# Check IV regression manually - note standard errors (and hence p-values) are not calculated correctly this way
# First stage: Regress PTJA on the instrumental variable and other exogenous variables, 
# don't know how to do this for 2 IVs but did it for one IV to reassure myself that the calculations were doing 
# what I expected them to do.
first_stage_pop <- lm(PT_Job_Access_Index ~ Pop_Dens + 
                                        No_car_rate + 
                                        White_percent + 
                                        Single_parent_household_rate + 
                                        Low_qual_percent +
                                       District, 
                               data = MANCH_data_regress)

MANCH_data_regress$PTJA_hat <- predict(first_stage_pop)

first_stage_manage <- lm(No_car_rate ~ #PT_Job_Access_Index + 
                                      SEC_Upper_Management_pc + 
                                      White_percent + 
                                      Single_parent_household_rate + 
                                      Low_qual_percent +
                                      District, 
                             data = MANCH_data_regress)

MANCH_data_regress$No_car_hat <- predict(first_stage_manage)
# Second stage: Regress Unemployment_rate on the predicted values of PTJA, no-car rate and other exogenous variables, 
# including fixed effects for District
second_stage <- lm(log(Unemployment_rate) ~ PTJA_hat + 
                                         No_car_hat + 
                                       White_percent + 
                                       Single_parent_household_rate + 
                                       Low_qual_percent + 
                                       District, 
                             data = MANCH_data_regress)
summary(second_stage)

# Check a third way - use ivreg package
ivreg_model <- ivreg(log(Unemployment_rate) ~ PT_Job_Access_Index  +
                       No_car_rate + 
                       White_percent + 
                       Single_parent_household_rate + 
                       Low_qual_percent + 
                      District
                         | Pop_Dens + 
                          SEC_Management_pc + 
                           White_percent + 
                           Single_parent_household_rate + 
                           Low_qual_percent + 
                          District,
                    data=MANCH_data_regress)
(iv_summary <- summary(ivreg_model, diagnostics=TRUE))

wu_hausman_stat <- iv_summary$diagnostics["Wu-Hausman", "statistic"]
wu_hausman_pvalue <- iv_summary$diagnostics["Wu-Hausman", "p-value"]
Weak_instruments_PTJA <- iv_summary$diagnostics["Weak instruments (PT_Job_Access_Index)", "statistic"]
Weak_instruments_PTJA_p <- iv_summary$diagnostics["Weak instruments (PT_Job_Access_Index)", "p-value"]
Weak_instruments_No_car <- iv_summary$diagnostics["Weak instruments (No_car_rate)", "statistic"]
Weak_instruments_No_car_p <- iv_summary$diagnostics["Weak instruments (No_car_rate)", "p-value"]

IV_diagnostics <- iv_summary$diagnostics %>%
  as.data.frame() %>%
  rename("p.value" = "p-value") %>%
  dplyr::select(-c(df1,df2)) %>%
    filter(statistic != "NA") %>%
  mutate(statistic= as.numeric(statistic),
         p.value = as.numeric(p.value)) %>%
  mutate(p.value= format(p.value, digits =2, scientific=TRUE),
         statistic = round(statistic, 0)) %>%
  rownames_to_column( var = "Test") %>%
  flextable()%>%
  set_table_properties( width = 1, layout = "autofit")
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = IV_diagnostics)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/IV_diag_GMCA.docx")


# 7. Model Diagnostics ------------------------------------------------
# Linearity Assumption #1	Predictor variables are independent of each other. Check VIFs

vif_initial <- vif(lm(log(Unemployment_rate) ~ PT_Job_Access_Index +
                        No_car_rate + 
                        White_percent + 
                        Single_parent_household_rate + 
                        Low_qual_percent,
                      data=MANCH_data_regress))

# VIF tabulation
set_flextable_defaults(digits=2, pct_digits=2, na_str="", width = "1", layout="autofit")
(vif_ft <- as.data.frame(bind_rows(vif_initial)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    rename("Public Transport Job Accessibility (PTJA)" = PT_Job_Access_Index,
           "No-car rate (%)" = No_car_rate, 
            "White people (%)" = White_percent,
            "Single-parent Households (%)" = Single_parent_household_rate, 
            "Low-qualified rate (%)" = Low_qual_percent) %>% 
  t() %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename("Variable" = "rowname",
         "Loglinear model" = "V1") %>%
     flextable() %>%
    set_table_properties( width = 1, layout = "autofit"))
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = vif_ft)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/VIFs_GMCA.docx")

#2	Correct functional form - good
# OLS Linear Model for comparison - removed district variable
Linear_Model <- lm(Unemployment_rate ~ PT_Job_Access_Index +
                     No_car_rate +
                     White_percent + 
                     Single_parent_household_rate + 
                     Low_qual_percent, 
                   data=MANCH_data_regress)

# Fixed effects (district-level) introduced and log transform dependent variable
LogLinear_Model <- lm(log(Unemployment_rate) ~ PT_Job_Access_Index +
                        No_car_rate + 
                        White_percent + 
                        Single_parent_household_rate + 
                        Low_qual_percent + 
                        District,
                      data=MANCH_data_regress)

FE_Model_lm <- lm(Unemployment_rate ~ PT_Job_Access_Index +
                     No_car_rate + 
                     White_percent + 
                     Single_parent_household_rate + 
                     Low_qual_percent + 
                      District,
                   data=MANCH_data_regress)

# Create Residuals vs Fitted plot for Linear_Model
linear_resid_plot <- ggplot(data = FE_Model_lm, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "FE-Model; Unemployment Rate (Fitted values)", y = "Residuals")

# Create Residuals vs Fitted plot for LogLinear_Model
loglinear_resid_plot <- ggplot(data = LogLinear_Model, aes(.fitted, .resid)) +
  geom_point() +
  scale_y_continuous(
    limits = c(-5, 11),  # Set y-axis limits
    breaks = c(-5,0,5,10),  # Set y-axis breaks
    labels = scales::comma  # Format y-axis labels
  ) +
  geom_smooth(method = "loess") +
  labs(x = "FE-LL-Model; log(Unemployment Rate) (Fitted values)", y = "Residuals")

# Extract fitted values and residuals
FE_LL_Model_IV_df <- data.frame(
  fitted = fitted(ivreg_model),
  resid = residuals(ivreg_model)
)

# Create Residuals vs Fitted plot for IV model
FE_IV_resid_plot <- ggplot(data = FE_LL_Model_IV_df, aes(fitted , resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_y_continuous(
    limits = c(-5, 11),  # Set y-axis limits
    breaks = c(-5,0,5,10),  # Set y-axis breaks
    labels = scales::comma  # Format y-axis labels
  ) +
  labs(x = "FE-LL-IV-Model; log(Unemployment Rate) (Fitted values)", y = "Residuals")

# Combine the plots using patchwork
Residfit <- linear_resid_plot + loglinear_resid_plot + 
            FE_IV_resid_plot+
            plot_layout(nrow = 3)

# Print the combined plot
print(Residfit)


# Ramsey RESET test - null hypothesis is the model is linear, 
#  rejected if p<0.05 and need to look at alternative models
# Loglinear model is better, not as good as the fifth root model
linear_reset <- resettest(Linear_Model, power = 2:3, type = "fitted")
FE_reset <- resettest(FE_Model, power = 2:3, type = "fitted")
loglinear_reset <- resettest(LogLinear_Model, power = 2:3, type = "fitted")
FE_LL_IV_reset <- resettest(FE_LL_Model_IV, power = 2:3, type = "fitted")
(reset_test <- data.frame(
  Model = c("OLS Model","FE Model", "FE-LL Model", "FE-LL-IV Model"),
  RESET_stat = c(linear_reset$statistic, FE_reset$statistic, loglinear_reset$statistic, FE_LL_IV_reset$statistic),
  p_value = c(linear_reset$p.value, FE_reset$p.value, loglinear_reset$p.value, FE_LL_IV_reset$p.value)) %>%
  rename("RESET Statistic" = "RESET_stat",
         "p-value" = "p_value") %>%
  mutate(across("p-value", ~ format(.x, digits = 2,scientific = TRUE)),
         across("RESET Statistic", ~ format(.x, digits =2))) %>%
  flextable()%>%
  set_table_properties( width = 1, layout = "autofit"))
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = reset_test)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/RESET_GMCA.docx")

# Plot PTJA and unemployment to visually check for non-linearity - doesn't show anything
plot <- ggplot(MANCH_data_regress, aes(x=Unemployment_rate, y=PT_Job_Access_Index)) + 
  geom_point() + 
  geom_smooth(method="lm")
logplot <- ggplot(MANCH_data_regress, aes(x=log(Unemployment_rate), y=PT_Job_Access_Index)) + 
  geom_point() + 
  geom_smooth(method="lm")
linearity <- plot + logplot +
  plot_annotation(title = "Comparison of Functional Forms") +
  plot_layout(nrow=2)
linearity
Linear_Model_crPlot <- lm(Unemployment_rate ~ PT_Job_Access_Index +
                               No_car_rate + 
                               White_percent + 
                               Single_parent_household_rate + 
                               Low_qual_percent , 
                             data=MANCH_data_regress)
LogLinear_Model_crPlot <- lm(log(Unemployment_rate) ~ PT_Job_Access_Index +
                        No_car_rate + 
                        White_percent + 
                        Single_parent_household_rate + 
                        Low_qual_percent, 
                    data=MANCH_data_regress)
Linear_Model_crPlot %>% car::crPlots(smooth = list(smoother=car::gamLine, k = 10))

LogLinear_Model_crPlot %>%car::crPlots(ylab="log(Unemp. Rate)",
                                smooth = list(smoother=car::gamLine, k = 10))

second_stage %>%car::crPlots(ylab="log(Unemp. Rate) (IV)",
                            smooth = list(smoother=car::gamLine, k = 10),
                            terms = ~.-District)

#3	Errors have constant variance (homoskedasctic) - null hypothesis is homoskedasticity - 
#   rejected if p<0.05 and need to use robust standard errors. Loglinear model better
FE_LL_IV_BP <- FE_LL_Model_IV %>%
  bptest(studentize = TRUE)
LL_BP <- LogLinear_Model %>% 
  bptest()
FE_BP <- FE_Model %>%
  bptest()
Linear_BP <- Linear_Model %>%
  bptest()


(BP_test <- data.frame(
  Model = c("OLS Model", "FE Model", "FE-LL Model", "FE-LL-IV Model"),
  BP_stat = c(Linear_BP$statistic, FE_BP$statistic, LL_BP$statistic, FE_LL_IV_BP$statistic),
  p_value = c(Linear_BP$p.value, FE_BP$p.value, LL_BP$p.value, FE_LL_IV_BP$p.value)) %>%
  rename("Breusch-Pagan Statistic" = "BP_stat",
         "p-value" = "p_value") %>%
  mutate(across("p-value", ~ format(.x, digits = 2,scientific = TRUE))) %>%
    mutate(across("Breusch-Pagan Statistic", ~ format(.x, digits = 3))) %>%  
  flextable() %>%
  set_table_properties( width = 1, layout = "autofit"))

# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = BP_test)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/BP_test_GMCA.docx")

#calculate robust standard errors for model coefficients
coeftest(LogLinear_Model, vcov = vcovHC(LogLinear_Model, type = 'HC0'))
coeftest(ivreg_model, vcov = vcovHC(ivreg_model, type = 'HC0'))
coeftest(FE_Model, vcov = vcovHC(FE_Model, type = 'HC0'))


#4	No autocorrelation
# Spatial autocorrelation controlled for by using fixed effects model. Also check Moran's I above.

#5		Errors normally distributed - good for FE model, doesn't work for IV model. 
# Can work out if necessary
LogLinear_Model  %>%
  plot(which = 2)%>%
  title (main = "Plot X. Quantile-Quantile plot for residuals")
# Create the histogram of residuals
residual_histogram_FE <- gg_reshist(LogLinear_Model, bins=30) +
  labs(x = "Residuals - FE-Model (Log-Linear)", y = "Count") +
  theme_bw()

# Create the histogram of residuals
residual_histogram_IV <- ggplot(FE_LL_Model_IV_df, aes(x = resid)) +
  geom_histogram(bins = 30, fill = "black", color = "black", alpha = 0.7) +
  theme_bw() +
  labs(x = "Residuals - FE-LL-IV Model", y = "Count")
residual_histogram_FE+residual_histogram_IV + plot_annotation(title = "Comparison of Residuals Distribution")

#7		No influential outlier data points - good for FE model, doesn't work for IV. Can work out if necessary
LogLinear_Model  %>%
  plot(which = 5)%>%
  title (main = "Plot X. Leverage vs Residuals plot")

#8 Spatial Autocorrelation - Calculate Moran's I statistic
# Calculate coordinates of centroids of LSOAs
centroids <- st_centroid(MANCH_data_regress)
coords <- st_coordinates(centroids)
#spatial weights matrix
nb <- knn2nb(knearneigh(coords, k=8)) # k = 4 is an example, adjust as needed
listw <- nb2listw(nb, style = "W")
# create binary continuity spatial weights matrix
neighbors <- poly2nb(MANCH_data_regress)
# Convert the neighbors list to a binary spatial weights list
binary_weights <- nb2listw(neighbors, style = "B")

# Calculate Moran's I statistic on unemployment rates - Moran's stat 0.5
(moran_unemp <- moran.test(MANCH_data_regress$Unemployment_rate, binary_weights))
moran_I_unemp <- round(moran_unemp$estimate[1],3)
moran_expect_unemp <- round(moran_unemp$estimate[2],3)
moran_p_unemp <- round(moran_unemp$p.value,22)

# Calculate Moran's I for PTJA
(moran_PTJA <- moran.test(MANCH_data_regress$PT_Job_Access_Index, binary_weights))
moran_I_PTJA <- round(moran_PTJA$estimate[1],3)
moran_PTJA_expect <- round(moran_PTJA$estimate[2],3)
moran_p_PTJA <- round(moran_PTJA$p.value,22)

# Moran's I for residuals from OLS model
residuals_linear <- residuals(Linear_Model)
(moran_test_linear <- moran.test(residuals_linear, binary_weights))
moran_OLS <- round(moran_test_linear$estimate[1],3)
moran_OLS_expect <- round(moran_test_linear$estimate[2],3)
moran_p_OLS <- round(moran_test_linear$p.value,22)

# Moran's I for residuals from FE-linear model
residuals_FE <- residuals(FE_Model)
(moran_test_FE <- moran.test(residuals_FE, binary_weights))
moran_FE <- round(moran_test_FE$estimate[1],3)
moran_FE_expect <- round(moran_test_FE$estimate[2],3)
moran_p_FE <- round(moran_test_FE$p.value,22)

# Moran's I for residuals from the loglinear fixed effects model - loglinear Moran's stat 0.08
residuals_loglinear <- residuals(LogLinear_Model)
(moran_test_loglinear <- moran.test(residuals_loglinear, binary_weights))
moran_FE_LL <- round(moran_test_loglinear$estimate[1],3)
moran_FE_LL_expect <- round(moran_test_loglinear$estimate[2],3)
moran_p_FE_LL <- round(moran_test_loglinear$p.value,22)

# Moran's I for residuals from loglinear FE-IV model - final model
residuals_FE_LL_IV <- residuals(FE_LL_Model_IV)
(moran_test_FE_LL_IV <- moran.test(residuals_FE_LL_IV, binary_weights))
moran_FE_LL_IV <- round(moran_test_FE_LL_IV$estimate[1],3)
moran_FE_LL_IV_expect <- round(moran_test_FE_LL_IV$estimate[2],3)
moran_p_FE_LL_IV <- round(moran_test_FE_LL_IV$p.value,22)

# Moran's I table - needs work for publication
(moran_table <- data.frame(
        "Variable / Model"  = c("Unemployment Rate", 
                     "Public Transport Job Accessibility",
                     "OLS Model Residuals",
                     "FE-Linear Model Residuals", 
                     "FE-LL (LogLinear) Model Residuals",
                     "FE-LL-IV Model Residuals"),
        Moran_I_Statistic = c(moran_I_unemp, moran_I_PTJA, moran_OLS, moran_FE, moran_FE_LL, moran_FE_LL_IV),
        Expected_Moran = c(moran_expect_unemp, moran_PTJA_expect, moran_OLS_expect, moran_FE_expect, moran_FE_expect, moran_FE_LL_IV_expect),
        p_value = c(moran_p_unemp, moran_p_PTJA, moran_p_OLS, moran_p_FE, moran_p_FE_LL, moran_p_FE_LL_IV)) %>%
  rename("Moran's I Statistic" = "Moran_I_Statistic",
         "p-value" = "p_value",
         "Expected Moran's I" = Expected_Moran) %>%
    mutate(across("Moran's I Statistic", ~ format(.x, digits = 2)),
           across("p-value", ~format(.x, digits =2, scientific = TRUE))) %>%
  flextable()%>%
  set_table_properties( width = 1, layout = "autofit") %>%
  set_caption("Moran's I statistics for key variables and residuals from models") )
# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = moran_table)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/moran_I_GMCA.docx")


# 6. Compare models FE-IV --------------------------------------------------------------
ivreg_model_data <- glance(ivreg_model) %>%
  mutate(Model = "5) FE-LL-IV Model (ivreg package)")
FE_Model_data <- glance(FE_Model) %>%
  mutate(Model = "3) FE Model (felm package)")
FE_LL_Model_IV_data <- glance(FE_LL_Model_IV)%>%
  mutate(Model = "4) FE-LL-IV Model (felm package)")
Linear_Model_data <- glance(Linear_Model)%>%
  mutate(Model = "1) Linear Model (lm package)")
LogLinear_Model_data <- glance(LogLinear_Model)%>%
  mutate(Model = "2) FE-LL Model (lm package)")

FE_LL_Model_IV_data$AIC <- AIC(FE_LL_Model_IV)
FE_LL_Model_IV_data$BIC <- BIC(FE_LL_Model_IV)
FE_Model_data$AIC <- AIC(FE_Model)
FE_Model_data$BIC <- BIC(FE_Model)
# Calculate AIC and BIC for ivreg model manually
# Extract residuals
residuals_iv <- residuals(ivreg_model)
# Calculate log-likelihood manually
n <- length(residuals_iv)
sigma2 <- sum(residuals_iv^2) / n
logLik_iv <- -n/2 * (log(2 * pi * sigma2) + 1)
# Calculate the number of parameters
k <- length(coef(ivreg_model))
# Calculate AIC manually
ivreg_model_data$AIC <- 2 * k - 2 * logLik_iv
# Calculate BIC manually
ivreg_model_data$BIC <- log(n) * k - 2 * logLik_iv

# Flextable
(model_comparison <- as.data.frame(bind_rows(
                               Linear_Model_data, 
                               LogLinear_Model_data,
                               FE_Model_data, 
                               FE_LL_Model_IV_data, 
                               ivreg_model_data)) %>%
  dplyr::select("Model", "r.squared", "sigma", "p.value", "AIC", "BIC") %>%
  mutate(across("r.squared", ~ format(.x, digits = 3)),
         across("sigma", ~ format(.x, digits = 3)),
         across("AIC", ~ format(.x, digits = 1)),
         across("BIC", ~ format(.x, digits = 1)),
         across("p.value", ~ format(.x, digits = 2))) %>%
  rename("Resid.Std.Error" = sigma,
         "Multiple R-squared" = r.squared,
         "p-value" = p.value) %>%
  flextable() %>%
  set_table_properties(width = 1, layout = "autofit")) %>%
  set_caption("Comparison of Linear, Loglinear (LL), Fixed Effects (FE), FE-LL and FE-LL Instrumental Variables (IV) Models")

# Create a Word document
doc <- read_docx()
# Add the flextable to the document
doc <- body_add_flextable(doc, value = model_comparison)
# Save the Word document
print(doc, target = "../Final_Report_(GMCA)/model_comparison_GMCA.docx")

# 8. Model Comparisons ------------------------------------------------
# PTJA coefficients
# FE_Model
summary(FE_Model)
summary(FE_LL_Model_IV, diagnostics=TRUE)
summary(ivreg_model, diagnostics=TRUE)

p_FE_LL <- (coeftest(FE_LL_Model, vcov = vcovHC(FE_LL_Model, type = 'HC0'))) %>%
  broom::tidy() %>%
  filter(!str_detect(term, "Dist")) %>%
  filter(term!="(Intercept)")%>%
  dplyr::select( "p.value") %>%
  t() %>%
  as.numeric()

s_FE_LL <- (coeftest(FE_LL_Model, vcov = vcovHC(FE_LL_Model, type = 'HC0'))) %>%
  broom::tidy() %>%
  filter(!str_detect(term, "Dist")) %>%
  filter(term!="(Intercept)")%>%
  dplyr::select( "std.error") %>%
  t() %>%
  as.numeric()

p_FE_LL_IV <- coeftest(ivreg_model, vcov = vcovHC(ivreg_model, type = 'HC0')) %>%
   broom::tidy() %>%
  filter(!str_detect(term, "Dist")) %>%
  filter(term != "(Intercept)")%>%
  dplyr::select( "p.value") %>%
  t() %>%
  as.numeric()
p_FE_LL_IV_sort <- p_FE_LL_IV[c(3,4,5,1,2)]

s_FE_LL_IV <- (coeftest(ivreg_model, vcov = vcovHC(ivreg_model, type = 'HC0'))) %>%
  broom::tidy() %>%
  filter(!str_detect(term, "Dist")) %>%
  filter(term!="(Intercept)")%>%
  dplyr::select( "std.error") %>%
  t() %>%
  as.numeric()
s_FE_LL_IV_sort <- s_FE_LL_IV[c(3,4,5,1,2)]

stargazer(FE_LL_Model, FE_LL_Model_IV,  type = "html",
          column.labels = c("FE-LL", 
                            "FE-LL-IV"),
          model.numbers = FALSE,
          dep.var.caption = NULL,
          p.auto=FALSE,
          dep.var.labels.include = TRUE,
          covariate.labels = c("Public Transport Job Accessibility (PTJA) (x10<sup>3</sup>)",
                               "Estimated PTJA - IV (x10<sup>3</sup>)",
                                       "No-car rate (%)",
                               "Estimated no-car rate - IV (%)", 
                               "White people (%)",
                               "Single-parent households (%)", 
                               "Low-qualified rate (%)"), 
          omit = c("District", "Constant"),
          omit.stat = c("adj.rsq"),
          single.row = TRUE,
          report = "vcsp*",
          no.space = TRUE,
          add.lines = list(c("AIC",(round(AIC(FE_Model), 0)),round(AIC(FE_LL_Model_IV)))),
          digits=4, format ="e", 
          ci=FALSE,
          digits.extra = 0,
          order = c(1, 6, 2, 7, 3, 4, 5),
          p = list(p_FE_LL, p_FE_LL_IV_sort),
          se = list(s_FE_LL, s_FE_LL_IV_sort),
          star.cutoffs = 0.05,
          notes = "Confidence Intervals in parentheses. * = p < 0.05",
          notes.append = FALSE,
          out = "Regression_Comparison_GMCA.html")
  
