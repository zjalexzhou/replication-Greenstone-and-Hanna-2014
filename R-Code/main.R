# ------------------------------------------------------------------------------------------------------
# File: main.R 
# Modified: Dec 2, 2021
#
# Replication of "Environmental Regulations, Air and Water Pollution, and Infant Mortality in India" (Greenstone and Hanna, 2014)
# 
# Final project for PUBPOL 870K: Statistics & Program Evaluation at Duke Kunshan University, Fall 2021
# Group Member: Yixin Fang, Yiming Li, Xinkai Wang, Weichen Xu, Zhijie Zhou
# 
# Instructor: Prof. Jiahua Yue
# Teaching Assistant: Xudong Ren
# 
# Full-text + Author-released data & STATA programs available at: 
# https://www.aeaweb.org/articles?id=10.1257/aer.104.10.3038
#
# R version: 4.0.2
# ------------------------------------------------------------------------------------------------------

# Objective: Execute the replication & extension programs.
#
# Structure:
#           0. Environment Setup & Data Import
#              - Import Libraries
#              - Import Source Code (Functions)
#              - Import Raw Data for Replication
#              - Import Raw Data for Extension
#           1. Main Program
#              - Data Cleaning & Event Study (1st Stage Regression)
#              - Mean Shift & Trend Break (2nd Stage Regression)
#              - Quandt Likelihood Ratio Test
#           2. Visualization
#           3. Extension Program


#######################################
## 0.ENVIRONMENT SETUP & DATA IMPORT ##
#######################################

# Import necessary libraries for the program
library(foreign)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(biostat3) # for "lincom" method used in 2nd-stage regression to test the five-year effects of policies

# Disable scientific notation for data within 200 digits for better display
options(scipen = 200)

# Set up reference to pre-packaged functions for the program
source("./R-Code/main-functions.R")
source("./R-Code/plot-functions.R")
source("./R-Code/extension-functions.R")

# For air pollution policies (SCAP & Catalytic converter)
dat_air <- read.dta("./Data-Replication/Air/combined.dta", 
                    convert.factors=NA) %>%
  mutate(city_id = as.numeric(factor(city)))
# For water pollution policies (NRCP)
dat_water <- read.dta("./Data-Replication/Water/india_waters_cityyear.dta",
                      convert.factors=NA) %>%
  mutate(cityriver = paste(city,river))
# For infant mortality rate data (with Catalytic Converter Policy)
dat_IM_air <- read.dta("./Data-Replication/IM/im_air.dta") %>%
  mutate(city_id = as.numeric(factor(city)))

# For preliminary extension (effects of 2008 Economic Crisis on environmental regulation effects)
dat_ext <- read.csv("./Data-Extension/data1990_2015.csv")

####################
## 1.MAIN PROGRAM ##
####################

# --------------------------------------------------------------------------------------------------
# Necessary "names" to match each policy outcome with the corresponding column for data processing

air <- c("SPM", "NO2", "SO2")
water <- c("BOD", "LNFCOLI", "DO")
IM <- c("IM_CAT")
AllOutcomes <- c(air, water, IM)


#---------------------------------------------------
# Data Cleaning & Event Study (1st Stage Regression)
# loop through all outcomes
for(outcome in AllOutcomes){
  assign(paste0(outcome, "_sigmas"), Present_1_Stage_Results(outcome, Perform_Regression_1(outcome, Clean_Data_For_Regression(outcome))))
}
# Behind the loop [Data Cleaning & Event Study (1st Stage Regression)]...
# SPM_sigmas <- Present_1_Stage_Results("SPM", Perform_Regression_1("SPM", Clean_Data_For_Regression("SPM")))
# SO2_sigmas <- Present_1_Stage_Results("SO2", Perform_Regression_1("SO2", Clean_Data_For_Regression("SO2")))
# NO2_sigmas <- Present_1_Stage_Results("NO2", Perform_Regression_1("NO2", Clean_Data_For_Regression("NO2")))
# BOD_sigmas <- Present_1_Stage_Results("BOD", Perform_Regression_1("BOD", Clean_Data_For_Regression("BOD")))
# LNFCOLI_sigmas <- Present_1_Stage_Results("LNFCOLI", Perform_Regression_1("LNFCOLI", Clean_Data_For_Regression("LNFCOLI")))
# DO_sigmas <- Present_1_Stage_Results("DO", Perform_Regression_1("DO", Clean_Data_For_Regression("DO")))
# IM_CAT_sigmas <- Present_1_Stage_Results("IM_CAT", Perform_Regression_1("IM_CAT", Clean_Data_For_Regression("IM_CAT")))


# -----------------------------------------------
# Mean Shift & Trend Break (2nd Stage Regression)
# loop through event study results for all outcomes
for(outcome in AllOutcomes){
  assign(paste0(outcome, "_trend_estimates"), 
         Present_2_Stage_Results(outcome, Perform_Regression_2(outcome, get(paste0(outcome, "_sigmas")))))
}
# Behind the loop [Mean Shift & Trend Break (2nd Stage Regression)]...
# SPM_trend_estimates <- Present_2_Stage_Results("SPM", Perform_Regression_2("SPM", SPM_sigmas))
# SO2_trend_estimates <- Present_2_Stage_Results("SO2", Perform_Regression_2("SO2", SO2_sigmas))
# NO2_trend_estimates <- Present_2_Stage_Results("NO2", Perform_Regression_2("NO2", NO2_sigmas))
# BOD_trend_estimates <- Present_2_Stage_Results("BOD", Perform_Regression_2("BOD", BOD_sigmas))
# LNFCOLI_trend_estimates <- Present_2_Stage_Results("LNFCOLI", Perform_Regression_2("LNFCOLI", LNFCOLI_sigmas))
# DO_trend_estimates <- Present_2_Stage_Results("DO", Perform_Regression_2("DO", DO_sigmas))
# IM_CAT_trend_estimates <- Present_2_Stage_Results("IM_CAT", Perform_Regression_2("IM_CAT", IM_CAT_sigmas))


# ----------------------------
# Quandt Likelihood Ratio Test
QLR_results <- rbind(Test_QLR('SPM'), Test_QLR('SO2'), Test_QLR('NO2'),
                     Test_QLR('BOD'), Test_QLR('LNFCOLI'), Test_QLR('DO'))
                     #,Test_QLR('IM_CAT'))


#####################
## 2.VISUALIZATION ##
#####################

# ----------------------------------------------------------
# All plots for main findings in Greenstone and Hanna (2014)
for(outcome in AllOutcomes){
  Plot_Event_Study_Estimates(outcome)
  Plot_Fstats(outcome)
}

