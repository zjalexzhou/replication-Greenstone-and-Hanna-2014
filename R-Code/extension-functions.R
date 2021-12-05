# ------------------------------------------------------------------------------------------------------
# File: extension-functions.R 
# Last Modified: Dec 5, 2021
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

# Objective: Prepare functions for the extension of Greenstone and Hanna (2014) pertaining to the effects of the 2008 Economic Crisis
#
# Structure:
#           Data Cleaning
#           Function1: Get_Crisis_DID_Effects(input_outcome)

# Extension: data cleaning
dat_ext_by_year <- dat_ext %>%
  mutate(date = mdy(date)) %>%
  group_by(location, date) %>% 
  # 1st step: group by location (city) and the same date
  summarize(so2 = mean(so2, na.rm = TRUE),
            no2 = mean(no2, na.rm = TRUE),
            spm = mean(spm, na.rm = TRUE), ) %>%
  mutate(year = year(date),
         city = location) %>% 
  # 2nd step: transfer the date form to year only then group by location & year
  group_by(city, year) %>%
  summarize(so2 = mean(so2, na.rm = TRUE),
            no2 = mean(no2, na.rm = TRUE),
            spm = mean(spm, na.rm = TRUE)) %>%
  mutate(breakYr = 2008)

treatmentCities_sc_adopt <- dat_air %>% filter(actionplan_sc == 1) %>% group_by(city) %>% summarise(count=n())
treatmentCities_cat_adopt <- dat_air %>% filter(catconverter == 1) %>% group_by(city) %>% summarise(count=n())

# organize data set & whether the particular city has adopted CAT/SCAP policies
dat_ext_final <- dat_ext_by_year %>%
  dplyr::select(city, year, so2, no2, spm, breakYr) %>%
  mutate(CAT = ifelse(city %in% treatmentCities_cat_adopt$city, 1,0),
         SCAP = ifelse(city %in% treatmentCities_sc_adopt$city, 1,0),
         IsAfterCrisis = year >= breakYr,
         SCAPtreated = SCAP == 1 & CAT == 0,
         SCAPdid = IsAfterCrisis * SCAPtreated,
         CATtreated = SCAP == 0 & CAT == 1,
         CATdid = IsAfterCrisis * CATtreated,
         bothtreated = SCAP == 1 & CAT == 1,
         bothdid = IsAfterCrisis * bothtreated,
         nontreated = SCAP == 0 & CAT == 0,
         nondid = IsAfterCrisis * nontreated)


Get_Crisis_DID_Effects <- function(input_outcome){
  
  outcome = tolower(input_outcome)
  
  didreg_two_way_scap_only = lm(get(outcome) ~ SCAPdid + as.factor(city) + as.factor(year), data = dat_ext_final)
  scaponlydid_estimates = as.data.frame(summary(didreg_two_way_scap_only)$coefficients)[2,]
  
  didreg_two_way_cat_only = lm(get(outcome) ~ CATdid + as.factor(city) + as.factor(year), data = dat_ext_final)
  catonlydid_estimates = as.data.frame(summary(didreg_two_way_cat_only)$coefficients)[2,]
  
  didreg_two_way_both = lm(get(outcome) ~ bothdid + as.factor(city) + as.factor(year), data = dat_ext_final)
  bothdid_estimates = as.data.frame(summary(didreg_two_way_both)$coefficients)[2,]
  
  didreg_two_way_non = lm(get(outcome) ~ nondid + as.factor(city) + as.factor(year), data = dat_ext_final)
  nondid_estimates = as.data.frame(summary(didreg_two_way_non)$coefficients)[2,]
  
  did_estimates = rbind(scaponlydid_estimates, catonlydid_estimates, bothdid_estimates, nondid_estimates)
  return(did_estimates)
}
