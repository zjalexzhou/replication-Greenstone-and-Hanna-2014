# ------------------------------------------------------------------------------------------------------
# File: main-functions.R 
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

# Objective: Prepare functions for replication.
#
# Structure:
#           Function1: Clean_Data_For_Regression(input_outcome)
#           Function2: Perform_Regression_1(input_outcome, input_tau_panel)
#           Function3: Present_Event_Study_Results(input_outcome, input_reg1_summary)
#           Function4: Perform_Regression_2(input_outcome, sigmas_outcome)
#           Function5: Present_2_Stage_Results(input_outcome, trends_outcome)
#           Function6: Test_QLR(input_outcome)


### --------------------------------------------------
### Function1: Clean_Data_For_Regression(input_outcome)
### --------------------------------------------------
### This function performs the data cleaning operations before stepping into the two-stage regressions.
###
### @ input: character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###          e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ output: pre-regression dataset for one of the outcome variables
###
Clean_Data_For_Regression <- function(input_outcome){
  
  if(input_outcome %in% air){
    column = paste0("e_", tolower(input_outcome), "_mean")
    
    # for air pollution policies (SCAP & Catalytic converter)
    # How many pollutant (so2) records does each city have? Summarize by count # of non-NA values of e_so2_mean
    dat_air_w_count <- dat_air %>%
      group_by(city_id) %>%
      summarize(count = sum(!(is.na(get(column))))) %>%
      right_join(dat_air, by=c("city_id" = "city_id"))
    
    # Generate Supreme Court Action Plan taus
    # The first year of action is the year of policy adoption
    SCAP_clean1 <- dat_air_w_count %>%
      mutate(temp = ifelse(actionplan_sc == 1, year, NA)) %>%
      #filter(is.na(temp)) # 2854 observations missing temp
      group_by(city_id) %>%
      summarise(temp2 = min(temp, na.rm = T),
                temp3 = min(year)) %>%
      mutate(temp2 = ifelse(temp2 > 9999, NA, temp2)) %>%
      right_join(dat_air_w_count, by=c("city_id"="city_id"))
    # filter(is.na(temp2)) # 2583 observations missing temp2
    
    SCAP_clean2 <- SCAP_clean1 %>%
      mutate(tauSC = ifelse(temp2 > temp3 & !(is.na(get(column))), year - temp2, NA)) %>%
      # filter(is.na(tauSC)) # 2643 observations missing tauSC
      mutate(neveradoptSC = is.na(temp2)) %>%
      # filter(neveradoptSC == 1) # 2583 observations have neveradoptSC == 1
      # filter(tauSC == 0) # 17 observations have tauSC == 0
      mutate(tauSC = ifelse(neveradoptSC == 1 & !(is.na(get(column))), 0, tauSC)) %>%
      # filter(tauSC == 0) # 1090 observations have tauSC == 0 (1073 real changes made)
      mutate(tempx = ifelse(actionplan_sc == 1 & !(is.na(get(column))), 1,NA))
    # filter(is.na(tempx)) # 2855 observations missing tempx
    # filter(!(is.na(temp2))) %>%
    # group_by(city_id) %>%
    # summarise(tempy = min(tempx))
    
    SCAP_clean3 <- SCAP_clean2 %>%
      mutate(temp4 = (tauSC >= 3 & !(is.na(tauSC))),
             temp5 = (tauSC <= -3 & !(is.na(tauSC)))) %>%
      group_by(city_id) %>%
      summarise(Mtemp4 = max(temp4),
                Mtemp5 = max(temp5)) %>%
      right_join(SCAP_clean2, by=c("city_id"="city_id")) %>%
      mutate(useSC = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptSC == 1 & count > 1)) %>%
      dplyr::select(-Mtemp4, -Mtemp5, -temp3, -temp2)
    
    CAT_clean1 <- SCAP_clean3 %>%
      mutate(temp = ifelse(catconverter == 1, year, NA)) %>%
      # filter(is.na(temp)) # 2618 observations missing temp
      group_by(city_id) %>%
      summarise(temp2 = min(temp, na.rm = T),
                temp3 = min(year)) %>%
      mutate(temp2 = ifelse(temp2 > 9999, NA, temp2)) %>%
      right_join(SCAP_clean3, by=c("city_id"="city_id"))
    # filter(is.na(temp2)) # 2289 observations missing temp2
    
    CAT_clean2 <- CAT_clean1 %>%
      mutate(tauCAT = ifelse(temp2 > temp3 & !(is.na(get(column))), year - temp2, NA)) %>%
      # filter(is.na(tauCAT)) # 2455 observations missing tauCAT
      mutate(neveradoptCAT = is.na(temp2)) %>%
      # filter(tauCAT == 0) # 23 tauCAT == 0
      mutate(tauCAT = ifelse(neveradoptCAT == 1 & !(is.na(get(column))), 0, tauCAT)) %>%
      # filter(tauCAT == 0) # 908 tauCAT == 0 (885 real changes made)
      mutate(tempx = ifelse(catconverter == 1 & !(is.na(get(column))), 1,NA))
    # filter(is.na(tempx)) # 2662 observations missing tempx
    
    CAT_clean3 <- CAT_clean2 %>%
      filter(!(is.na(temp2))) %>%
      group_by(city_id) %>%
      summarise(tempy = min(tempx, na.rm = T)) %>%
      mutate(tempy = ifelse(tempy != 1, NA, tempy)) %>%
      right_join(CAT_clean2, by=c("city_id"="city_id")) %>%
      # filter(tempy == 1) # 630 observations tempy == 1
      # filter(neveradoptCAT == 1) # 2289 observations neveradoptCAT == 1
      mutate(neveradoptCAT = ifelse(is.na(tempy) & !(is.na(temp2)), 1, neveradoptCAT)) %>%
      # filter(neveradoptCAT == 1) # 2310 observations neveradoptCAT == 1 (21 real changes)
      # filter(is.na(tauCAT)) # 1570 observations missing tauCAT
      mutate(tauCAT = ifelse(is.na(tempy) & !(is.na(temp2)), NA, tauCAT)) %>%
      # filter(is.na(tauCAT)) # 1577 observations missing tauCAT (7 real changes)
      mutate(temp4 = (tauCAT >= 3 & !(is.na(tauCAT))),
             temp5 = (tauCAT <= -3 & !(is.na(tauCAT))))
    
    CAT_clean4 <- CAT_clean3 %>%
      group_by(city_id) %>%
      summarize(Mtemp4 = max(temp4),
                Mtemp5 = max(temp5)) %>%
      right_join(CAT_clean3, by=c("city_id"="city_id")) %>%
      mutate(useCAT = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptCAT == 1 & count > 1)) %>%
      filter(!(is.na(get(column)))) %>% # 1570 observations removed
      filter(useSC == 1 & useCAT == 1) # further 48 observations removed 
    
    final_tau_panel_air <- CAT_clean4 %>%
      mutate(tauSCL = ifelse(tauSC < -7, 1,0),
             tauSCm7 = ifelse(tauSC == -7, 1,0),
             tauSCm6 = ifelse(tauSC == -6, 1,0),
             tauSCm5 = ifelse(tauSC == -5, 1,0),
             tauSCm4 = ifelse(tauSC == -4, 1,0),
             tauSCm3 = ifelse(tauSC == -3, 1,0),
             tauSCm2 = ifelse(tauSC == -2, 1,0),
             tauSCm1 = ifelse(tauSC == -1, 1,0),
             tauSC0 = ifelse(tauSC == 0, 1,0),
             tauSC1 = ifelse(tauSC == 1, 1,0),
             tauSC2 = ifelse(tauSC == 2, 1,0),
             tauSC3 = ifelse(tauSC == 3, 1,0),
             tauSCR = ifelse(tauSC > 3, 1,0)
      ) %>%
      mutate(tauCATL = ifelse(tauCAT < -7, 1,0),
             tauCATm7 = ifelse(tauCAT == -7, 1,0),
             tauCATm6 = ifelse(tauCAT == -6, 1,0),
             tauCATm5 = ifelse(tauCAT == -5, 1,0),
             tauCATm4 = ifelse(tauCAT == -4, 1,0),
             tauCATm3 = ifelse(tauCAT == -3, 1,0),
             tauCATm2 = ifelse(tauCAT == -2, 1,0),
             tauCATm1 = ifelse(tauCAT == -1, 1,0),
             tauCAT0 = ifelse(tauCAT == 0, 1,0),
             tauCAT1 = ifelse(tauCAT == 1, 1,0),
             tauCAT2 = ifelse(tauCAT == 2, 1,0),
             tauCAT3 = ifelse(tauCAT == 3, 1,0),
             tauCAT4 = ifelse(tauCAT == 4, 1,0),
             tauCAT5 = ifelse(tauCAT == 5, 1,0),
             tauCAT6 = ifelse(tauCAT == 6, 1,0),
             tauCAT7 = ifelse(tauCAT == 7, 1,0),
             tauCAT8 = ifelse(tauCAT == 8, 1,0),
             tauCAT9 = ifelse(tauCAT == 9, 1,0),
             tauCATR = ifelse(tauCAT > 9, 1,0)
      ) %>%
      mutate(tauSCL = ifelse(is.na(tauSC), 0,tauSCL),
             tauSCm7 = ifelse(is.na(tauSC), 0,tauSCm7),
             tauSCm6 = ifelse(is.na(tauSC), 0,tauSCm6),
             tauSCm5 = ifelse(is.na(tauSC), 0,tauSCm5),
             tauSCm4 = ifelse(is.na(tauSC), 0,tauSCm4),
             tauSCm3 = ifelse(is.na(tauSC), 0,tauSCm3),
             tauSCm2 = ifelse(is.na(tauSC), 0,tauSCm2),
             tauSCm1 = ifelse(is.na(tauSC), 0,tauSCm1),
             tauSC0 = ifelse(is.na(tauSC), 0,tauSC0),
             tauSC1 = ifelse(is.na(tauSC), 0,tauSC1),
             tauSC2 = ifelse(is.na(tauSC), 0,tauSC2),
             tauSC3 = ifelse(is.na(tauSC), 0,tauSC3),
             tauSCR = ifelse(is.na(tauSC), 0,tauSCR)
      ) %>% 
      mutate(tauCATL = ifelse(is.na(tauCAT), 0, tauCATL),
             tauCATm7 = ifelse(is.na(tauCAT), 0, tauCATm7),
             tauCATm6 = ifelse(is.na(tauCAT), 0, tauCATm6),
             tauCATm5 = ifelse(is.na(tauCAT), 0, tauCATm5),
             tauCATm4 = ifelse(is.na(tauCAT), 0, tauCATm4),
             tauCATm3 = ifelse(is.na(tauCAT), 0, tauCATm3),
             tauCATm2 = ifelse(is.na(tauCAT), 0, tauCATm2),
             tauCATm1 = ifelse(is.na(tauCAT), 0, tauCATm1),
             tauCAT0 = ifelse(is.na(tauCAT), 0, tauCAT0),
             tauCAT1 = ifelse(is.na(tauCAT), 0, tauCAT1),
             tauCAT2 = ifelse(is.na(tauCAT), 0, tauCAT2),
             tauCAT3 = ifelse(is.na(tauCAT), 0, tauCAT3),
             tauCAT4 = ifelse(is.na(tauCAT), 0, tauCAT4),
             tauCAT5 = ifelse(is.na(tauCAT), 0, tauCAT5),
             tauCAT6 = ifelse(is.na(tauCAT), 0, tauCAT6),
             tauCAT7 = ifelse(is.na(tauCAT), 0, tauCAT7),
             tauCAT8 = ifelse(is.na(tauCAT), 0, tauCAT8),
             tauCAT9 = ifelse(is.na(tauCAT), 0, tauCAT9),
             tauCATR = ifelse(is.na(tauCAT), 0, tauCATR)
      )
    return(final_tau_panel_air)
  } 
  else if(input_outcome %in% water){
    column = tolower(input_outcome)
    
    dta_clean1 <- dat_water %>%
      group_by(cityriver) %>%
      summarise(yap1city = max(yap1),
                gap1city = max(gap1),
                gap2city = max(gap2),
                nrcpcity = max(nrcp),
                dapcity = max(dap),
                gomtiap1city = max(gomtiap1)) %>%
      right_join(dat_water, by=c("cityriver"="cityriver")) %>%
      # filter(is.na(yap1city)) 149 observations missing 
      mutate(temp2 = ifelse(nrcpcity == 1, 1995, NA)) %>%
      # filter(is.na(temp2)) # 5578 observations with temp2 = NA
      mutate(temp2 = ifelse(yap1city == 1 | gap2city == 1 | dapcity == 1 | gomtiap1city == 1, 1993, temp2)) %>%
      # filter(temp2 == 1993) # 307 real changes made
      mutate(temp2 = ifelse(gap1city == 1, 1985, temp2))
    # filter(temp2 == 1985) # 156 real changes made
    
    dta_clean2 <- dta_clean1 %>%
      group_by(cityriver) %>%
      summarise(temp3 = min(year)) %>% 
      right_join(dta_clean1, by=c("cityriver"="cityriver")) %>%
      mutate(tau = ifelse(temp2 > temp3 & (!(is.na(get(column)))), year - temp2, NA)) %>%
      # filter(is.na(tau)) # 5420 observations missing tau (NA)
      mutate(neveradopt = is.na(temp2)) %>%
      # filter(neveradopt == 1) 5203 observations have neveradopt == 1
      # filter(neveradopt == 1 & !(is.na(get(column)))) 4993 observations has get(column) data & neveradopt == 1
      # code the neveradopt cities --- always in event year 0
      # filter(neveradopt == 1 & (!(is.na(get(column)))))
      # filter(tau == 0 & neveradopt != 1) # 41 observations have tau == 0 while aget(column)pted the NRCP policy 
      mutate(tau = ifelse(neveradopt == 1 & !(is.na(get(column))), 0, tau)) %>%
      mutate(tempx = ifelse(
        (yap1 == 1 | gap1 == 1 | gap2 == 1 | nrcp == 1 | dap == 1 | gomtiap1 == 1) & (!(is.na(get(column)))), 1, NA
      ))
    # filter(is.na(tempx)) # 5484 missing values tempx (NA)
    
    dta_clean3 <- dta_clean2 %>% 
      filter(!(is.na(temp2))) %>%
      group_by(cityriver) %>%
      summarise(tempy = min(tempx, na.rm = T)) %>% 
      right_join(dta_clean2, by=c("cityriver"="cityriver")) %>%
      # filter(is.na(tempy)) # 5203 missing value tempy (NA)
      mutate(temp4 = (tau >= 3 & (!(is.na(tau)))),
             temp5 = (tau <= -3 & (!(is.na(tau)))))
    
    dta_clean4 <- dta_clean3 %>%
      group_by(cityriver) %>%
      summarise(Mtemp4 = max(temp4),
                Mtemp5 = max(temp5)) %>%
      right_join(dta_clean3, by=c("cityriver"="cityriver"))
    
    dta_clean5 <- dta_clean4 %>%
      group_by(cityriver) %>%
      summarize(count = sum(!(is.na(get(column))))) %>%
      right_join(dta_clean4, by=c("cityriver"="cityriver")) %>%
      mutate(use = (Mtemp4 == 1 & Mtemp5 == 1) | (neveradopt == 1 & count > 1)) %>%
      filter(use == 1 & !(is.na(get(column))))
    # filter(is.na(get(column)))
    # filter(!(is.na(get(column))) & use == 1)
    
    final_tau_panel_water <- dta_clean5 %>%
      mutate(tauL = ifelse(tau < -7, 1,0),
             taum7 = ifelse(tau == -7, 1,0),
             taum6 = ifelse(tau == -6, 1,0),
             taum5 = ifelse(tau == -5, 1,0),
             taum4 = ifelse(tau == -4, 1,0),
             taum3 = ifelse(tau == -3, 1,0),
             taum2 = ifelse(tau == -2, 1,0),
             taum1 = ifelse(tau == -1, 1,0),
             tau0 = ifelse(tau == 0, 1,0),
             tau1 = ifelse(tau == 1, 1,0),
             tau2 = ifelse(tau == 2, 1,0),
             tau3 = ifelse(tau == 3, 1,0),
             tau4 = ifelse(tau == 4, 1,0),
             tau5 = ifelse(tau == 5, 1,0),
             tau6 = ifelse(tau == 6, 1,0),
             tau7 = ifelse(tau == 7, 1,0),
             tau8 = ifelse(tau == 8, 1,0),
             tau9 = ifelse(tau == 9, 1,0),
             tau10 = ifelse(tau == 10, 1,0),
             tauR = ifelse(tau > 10, 1,0)) %>%
      mutate(tauL = ifelse(is.na(tau), 0, tauL),
             taum7 = ifelse(is.na(tau), 0, taum7),
             taum6 = ifelse(is.na(tau), 0, taum6),
             taum5 = ifelse(is.na(tau), 0, taum5),
             taum4 = ifelse(is.na(tau), 0, taum4),
             taum3 = ifelse(is.na(tau), 0, taum3),
             taum2 = ifelse(is.na(tau), 0, taum2),
             taum1 = ifelse(is.na(tau), 0, taum1),
             tau0 = ifelse(is.na(tau), 0, tau0),
             tau1 = ifelse(is.na(tau), 0, tau1),
             tau2 = ifelse(is.na(tau), 0, tau2),
             tau3 = ifelse(is.na(tau), 0, tau3),
             tau4 = ifelse(is.na(tau), 0, tau4),
             tau5 = ifelse(is.na(tau), 0, tau5),
             tau6 = ifelse(is.na(tau), 0, tau6),
             tau7 = ifelse(is.na(tau), 0, tau7),
             tau8 = ifelse(is.na(tau), 0, tau8),
             tau9 = ifelse(is.na(tau), 0, tau9),
             tau10 = ifelse(is.na(tau), 0, tau10),
             tauR = ifelse(is.na(tau), 0, tauR)) 
    return(final_tau_panel_water)
    
  } 
  else if(input_outcome %in% IM) {
    
    column = "c_IM"
    
    dat_clean1 <- dat_IM_air %>%
      group_by(city_id) %>%
      summarize(IMtemp = max(get(column), na.rm = T)) %>% 
      filter(IMtemp < 0) %>%
      # 4 cities missing get(column) data at all
      right_join(dat_IM_air, by = c("city_id"="city_id")) %>%
      filter(is.na(IMtemp)) %>%
      dplyr::select(-IMtemp)
    # 84 observations removed (cities with NA get(column) data)
    
    dat_IM_count <- dat_clean1 %>%
      group_by(city_id) %>%
      summarize(count = sum(!(is.na(get(column))))) %>%
      right_join(dat_clean1, by=c("city_id"="city_id"))
    
    tau_panel <- dat_IM_count  %>%
      group_by(city_id) %>%
      summarise(minYr = min(year),
                maxYr = max(year)) %>% 
      right_join(dat_IM_count, by = c("city_id"="city_id")) %>%
      mutate(tau = ifelse(catyear > minYr & (!(is.na(get(column)))), year-catyear,NA)) %>% 
      # generated 1763 observations missing tau (NA) 
      # filter(is.na(tau))
      mutate(neveradoptCAT = is.na(catyear) | maxYr < catyear) %>%
      mutate(tempx = ifelse((year >= catyear) & (!(is.na(catyear))) & (!(is.na(get(column)))), 1,NA))
    # generated 2100 observations missing tempx (NA)
    # %>% filter(is.na(tempx))
    
    dat_clean2 <- tau_panel %>%
      # we have 518 observations with catyear == 1
      filter(!(is.na(catyear))) %>%
      group_by(city_id) %>%
      summarize(tempy = min(tempx, na.rm = T)) %>%
      filter(tempy == 1) %>%
      # summarize(tempy = min(tempx, na.rm = T), count=count) %>%
      # filter(tempy == 1) %>% # 408 observations with tempy == 1, 
      right_join(tau_panel, by = c("city_id"="city_id")) %>%
      mutate(tempy = ifelse(is.na(catyear) | tempy == 1 & is.na(catyear), NA, tempy)) %>%
      # generated 1839 observations missing tempy (NA)
      # %>% filter(is.na(tempy)) 
      # filter(neveradoptCAT == 1) # 1729 observations with neveradoptCAT == 1
      mutate(neveradoptCAT = ifelse(is.na(tempy) & !(is.na(catyear)), 1, neveradoptCAT)) %>%
      # filter(neveradoptCAT == 1) # 110 observations changed to neveradoptCAT = 1 (now 1839)
      # filter(is.na(tau)) # 1763 observations missing tau (NA)
      mutate(tau = ifelse(is.na(tempy) & !(is.na(catyear)), NA, tau))
    # %>% filter(is.na(tau)) # 82 observations changed to missing tau (NA) (now 1845)
    
    dat_clean3 <- dat_clean2 %>%
      mutate(temp4 = (tau>=3 & !(is.na(tau))),
             temp5 = (tau <= -3 & !(is.na(tau)))) %>%
      group_by(city_id) %>%
      summarise(Mtemp4 = max(temp4),
                Mtemp5 = max(temp5)) %>%
      right_join(dat_clean2, by = c("city_id"="city_id")) %>%
      mutate(useCAT = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptCAT == 1 & count > 1)) %>%
      filter(useCAT == 1 & !(is.na(get(column))))
    # filter(useCAT == 1) # 105 observations useCAT == 0 should not be included in the final data panel
    # filter(is.na(get(column)))
    # final panel of 1222 observations & 381 observations with tau values (non-NA)
    
    final_tau_panel_IM <- dat_clean3 %>%
      mutate(tauL = ifelse(tau < -10, 1,0),
             taum10 = ifelse(tau == -10, 1,0),
             taum9 = ifelse(tau == -9, 1,0),
             taum8 = ifelse(tau == -8, 1,0),
             taum7 = ifelse(tau == -7, 1,0),
             taum6 = ifelse(tau == -6, 1,0),
             taum5 = ifelse(tau == -5, 1,0),
             taum4 = ifelse(tau == -4, 1,0),
             taum3 = ifelse(tau == -3, 1,0),
             taum2 = ifelse(tau == -2, 1,0),
             taum1 = ifelse(tau == -1, 1,0),
             tau0 = ifelse(tau == 0, 1,0),
             tau1 = ifelse(tau == 1, 1,0),
             tau2 = ifelse(tau == 2, 1,0),
             tau3 = ifelse(tau == 3, 1,0),
             tau4 = ifelse(tau == 4, 1,0),
             tau5 = ifelse(tau == 5, 1,0),
             tauR = ifelse(tau > 5 & !(is.na(tau)), 1,0)) %>%
      mutate(tauL = ifelse(is.na(tauL), 0, tauL),
             taum10 = ifelse(is.na(taum10), 0, taum10),
             taum9 = ifelse(is.na(taum9), 0, taum9),
             taum8 = ifelse(is.na(taum8), 0, taum8),
             taum7 = ifelse(is.na(taum7), 0, taum7),
             taum6 = ifelse(is.na(taum6), 0, taum6),
             taum5 = ifelse(is.na(taum5), 0, taum5),
             taum4 = ifelse(is.na(taum4), 0, taum4),
             taum3 = ifelse(is.na(taum3), 0, taum3),
             taum2 = ifelse(is.na(taum2), 0, taum2),
             taum1 = ifelse(is.na(taum1), 0, taum1),
             tau0 = ifelse(is.na(tau0), 0, tau0),
             tau1 = ifelse(is.na(tau1), 0, tau1),
             tau2 = ifelse(is.na(tau2), 0, tau2),
             tau3 = ifelse(is.na(tau3), 0, tau3),
             tau4 = ifelse(is.na(tau4), 0, tau4),
             tau5 = ifelse(is.na(tau5), 0, tau5),
             tauR = ifelse(is.na(tauR), 0, tauR))
    return(final_tau_panel_IM)
    
  } 
  else {
    stop('The outcome specified is not valid. 
         \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
  }
}


### --------------------------------------------------------------
### Function2: Perform_Regression_1(input_outcome, input_tau_panel)
### --------------------------------------------------------------
### This function performs the first stage regression, i.e., the event study of policy effects on one of the outcomes.
###
### @ input(1): character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###             e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ input(2): the panel data (air/water/IM) obtained from Clean_Data_For_Regression(input_outcome)
### @ output: a summary of regression estimates
###
Perform_Regression_1 <- function(input_outcome, input_tau_panel){
  
  if(input_outcome %in% air){
    column = paste0("e_", tolower(input_outcome), "_mean")
    
    reg_event_study <- lm(get(column) ~ tauSCm7+tauSCm6+tauSCm5+tauSCm4+tauSCm3+tauSCm2+tauSCm1+
                       tauSC0+tauSC1+tauSC2+tauSC3+ tauSCL+tauSCR+
                       tauCATm7+tauCATm6+tauCATm5+tauCATm4+tauCATm3+tauCATm2+tauCATm1+
                       tauCAT0+tauCAT1+tauCAT2+tauCAT3+tauCAT4+tauCAT5+tauCAT6+tauCAT7+tauCAT8+tauCAT9+
                       tauCATL+tauCATR+
                       lit_urban + mean +
                       as.factor(year) + as.factor(city),
                     data = input_tau_panel, weights = pop_urban)
  } 
  else if(input_outcome %in% water){
    column = tolower(input_outcome)
    reg_event_study <- lm(get(column) ~ taum7+taum6+taum5+taum4+taum3+taum2+taum1+
                        tau0+tau1+tau2+tau3+tau4+tau5+tau6+tau7+tau8+tau9+tau10+
                        tauL+tauR +
                        lit_urban + pce + as.factor(cityriver) + as.factor(year),
                      data = input_tau_panel, weights = pop_urban)
  } 
  else if(input_outcome %in% IM){
    column = "c_IM"
    reg_event_study <- lm(get(column) ~ taum10+ taum9+ taum8+ taum7+ taum6+ taum5+ taum4+ taum3+ taum2 + taum1+ 
                   tau0+ tau1+ tau2+ tau3+ tau4+ tau5+ tauL+tauR+lit_urban+mean+
                   as.factor(city)+ as.factor(year),
                 data = input_tau_panel, weights = c_birth)
  }
  else{
    stop('The outcome specified is not valid. 
         \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
  }
  return(summary(reg_event_study))
}


### ------------------------------------------------------------------------
### Function3: Present_Event_Study_Results(input_outcome, input_reg1_summary)
### ------------------------------------------------------------------------
### This function prepare the event study results for 2nd stage regression and plotting.
###
### @ input(1): character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###             e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ input(2): regression summary from event study regression
### @ output: an organized dataset of event study results
###
Present_1_Stage_Results <- function(input_outcome, input_reg1_summary){
  matrix_coef <- as.data.frame(input_reg1_summary$coefficients)
  
  if(input_outcome %in% air){
    
    coef <- as.data.frame(matrix_coef$Estimate[2:30])
    colnames(coef)[1] <- 'taub'
    ste <- as.data.frame(matrix_coef$`Std. Error`[2:30])
    colnames(ste)[1] <- 'tause'
    
    tauCAT <- c(-7:9)
    tauSC <- c(-7:3)
    tauSC[12:17] <- NA
    sigmas_outcome <- as.data.frame(cbind(tauCAT, tauSC))%>%
      mutate(catconv = ifelse(tauCAT >=0, 1,0),
             tau_trend_CAT = tauCAT + 8,
             catconv_trend = catconv*tauCAT,
             scap = ifelse(tauSC >=0, 1, 0),
             tau_trend_SC = tauSC + 8,
             scap_trend = scap*tauSC) %>%
      mutate(scap_trend = ifelse(tauSC >=0, tauSC+1, 0),
             catconv_trend = ifelse(tauCAT >=0, tauCAT+1,0))
    
    sigmas_outcome$taubCAT <- coef$taub[13:29]
    sigmas_outcome$tauseCAT <- ste$tause[13:29]
    sigmas_outcome$taubSC <- NA
    sigmas_outcome$taubSC[1:11] <- coef$taub[1:11]
    sigmas_outcome$tauseSC <- NA
    sigmas_outcome$tauseSC[1:11] <- ste$tause[1:11]
    
    # normalize the estimates (take the estimate at tau = -1 as the standard "0")
    taubm1_CAT = sigmas_outcome$taubCAT[sigmas_outcome$tauCAT==-1]
    sigmas_outcome_cleaned <- sigmas_outcome %>% na.omit()
    taubm1_SC = sigmas_outcome_cleaned$taubSC[sigmas_outcome_cleaned$tauSC==-1]
    sigmas_outcome <- sigmas_outcome %>%
      mutate(norm_taubCAT = taubCAT - taubm1_CAT,
             norm_taubSC = taubSC - taubm1_SC) 
  }
  else if(input_outcome %in% water){
    
    coef <- as.data.frame(matrix_coef$Estimate[2:19])
    colnames(coef)[1] <- 'taub'
    ste <- as.data.frame(matrix_coef$`Std. Error`[2:19])
    colnames(ste)[1] <- 'tause'
    
    tau <- c(-7:10)
    sigmas_outcome <- as.data.frame(tau) %>%
      mutate(taub = 0,
             tause = 0,
             nrcp = (tau >=0),
             tau_trend = tau + 8,
             nrcp_trend = nrcp * tau) 
    sigmas_outcome <- sigmas_outcome %>%
      mutate(nrcp_trend = ifelse(tau >= 0, nrcp_trend + 1, nrcp_trend))
    
    sigmas_outcome$taub <- as.matrix(coef[1])
    sigmas_outcome$tause <- as.matrix(ste[1])
    
    # normalize the estimates (take the estimate at tau = -1 as the standard "0")
    taubm1= sigmas_outcome$taub[sigmas_outcome$tau==-1]
    
    sigmas_outcome_cleaned <- sigmas_outcome %>% na.omit()
    taubm1 = sigmas_outcome_cleaned$taub[sigmas_outcome_cleaned$tau==-1]
    
    sigmas_outcome <- sigmas_outcome %>%
      mutate(norm_taub = taub - taubm1,
             norm_taub = taub - taubm1)
  }
  else if(input_outcome %in% IM){
    coef <- as.data.frame(matrix_coef$Estimate[2:17])
    colnames(coef)[1] <- 'taub'
    ste <- as.data.frame(matrix_coef$`Std. Error`[2:17])
    colnames(ste)[1] <- 'tause'
    
    tau <- c(-10:5)
    sigmas_outcome <- as.data.frame(tau) %>%
      mutate(taub = 0,
             tause = 0,
             cat = (tau >= 0),
             tau_trend = tau + 10,
             cat_trend = ifelse(tau >= 0, cat * tau + 1, cat * tau)) 
    
    sigmas_outcome$taub <- coef$taub
    sigmas_outcome$tause <- ste$tause
    
    # normalize
    taubm1 = sigmas_outcome$taub[sigmas_outcome$tau==-1]
    sigmas_outcome <- sigmas_outcome %>%
      mutate(norm_taub = taub - taubm1)
  }
  else{
    stop('The outcome specified is not valid. 
         \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
  }
  return(sigmas_outcome)
}


### -------------------------------------------------------------
### Function4: Perform_Regression_2(input_outcome, sigmas_outcome)
### -------------------------------------------------------------
### This function performs the second stage regression, i.e., the mean shift and trend break of policy effects on one of the outcomes.
### 
### @ input(1): character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###             e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ input(2): an organized dataset of event study results
###
### @ output: an organized list of estimates for equation 2a 2b 2c and 5-year effects
###
Perform_Regression_2 <- function(input_outcome, sigmas_outcome){
  
  if(input_outcome %in% air){
    reg_outcome_2a_SCAP <- lm(taubSC ~ scap,
                         data = sigmas_outcome,
                         weights = 1/tauseSC)
    
    reg_outcome_2b_SCAP <- lm(taubSC ~ tauSC+scap,
                         data = sigmas_outcome,
                         weights = 1/tauseSC)
    
    reg_outcome_2c_SCAP <- lm(taubSC ~ tauSC+scap+scap_trend,
                         data = sigmas_outcome,
                         weights = 1/tauseSC)
    
    lincom_5yr_effect_SCAP <- lincom(reg_outcome_2c_SCAP, c("scap + 5*scap_trend"))
    
    reg_outcome_2a_CAT <- lm(taubCAT ~ catconv,
                              data = sigmas_outcome,
                              weights = 1/tauseCAT)
    
    reg_outcome_2b_CAT <- lm(taubCAT ~ tauCAT+catconv,
                              data = sigmas_outcome,
                              weights = 1/tauseCAT)
    
    reg_outcome_2c_CAT <- lm(taubCAT ~ tauCAT+catconv+catconv_trend,
                              data = sigmas_outcome,
                              weights = 1/tauseCAT)
    
    lincom_5yr_effect_CAT <- lincom(reg_outcome_2c_CAT, c("catconv + 5*catconv_trend"))
    
    outcome_2abc <- list(summary(reg_outcome_2a_SCAP), summary(reg_outcome_2b_SCAP), 
                         summary(reg_outcome_2c_SCAP), lincom_5yr_effect_SCAP,
                         summary(reg_outcome_2a_CAT), summary(reg_outcome_2b_CAT), 
                         summary(reg_outcome_2c_CAT), lincom_5yr_effect_CAT)
    names(outcome_2abc) <- c("2aSCAP", "2bSCAP","2cSCAP","5yrEffectSCAP",
                             "2aCAT", "2bCAT","2cCAT","5yrEffectCAT")
  }
  else if(input_outcome %in% water){
    reg_outcome_2a <- lm(taub ~ nrcp,
                         data = sigmas_outcome,
                         weights = 1/tause)
    
    reg_outcome_2b <- lm(taub ~ tau+nrcp,
                         data = sigmas_outcome,
                         weights = 1/tause)
    
    reg_outcome_2c <- lm(taub ~ tau+nrcp+nrcp_trend,
                         data = sigmas_outcome,
                         weights = 1/tause)
    
    lincom_5yr_effect <- lincom(reg_outcome_2c, c("nrcpTRUE + 5*nrcp_trend"))
    
    outcome_2abc <- list(summary(reg_outcome_2a), summary(reg_outcome_2b), summary(reg_outcome_2c), lincom_5yr_effect)
    names(outcome_2abc) <- c("2a", "2b","2c","5yrEffect")
  }
  else if(input_outcome %in% IM){
    reg_outcome_2a <- lm(taub ~ cat,
                             data = sigmas_outcome,
                             weights = 1/tause)
    
    reg_outcome_2b <- lm(taub ~ tau+cat,
                             data = sigmas_outcome,
                             weights = 1/tause)
    
    reg_outcome_2c <- lm(taub ~ tau+cat+cat_trend,
                             data = sigmas_outcome,
                             weights = 1/tause)
    
    lincom_5yr_effect <- lincom(reg_outcome_2c, c("catTRUE + 5*cat_trend"))
    
    outcome_2abc <- list(summary(reg_outcome_2a), summary(reg_outcome_2b), 
                         summary(reg_outcome_2c), lincom_5yr_effect)
    names(outcome_2abc) <- c("2a", "2b","2c","5yrEffect")
    
  }
  else{
    stop('The outcome specified is not valid. 
         \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
  }
  return(outcome_2abc)
}


### ----------------------------------------------------------------
### Function5: Present_2_Stage_Results(input_outcome, trends_outcome)
### ----------------------------------------------------------------
### This function shall tidy up the results (lists) from the 2nd stage regressions.
### 
### @ input(1): character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###             e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ input(2): an organized list of 2nd stage regression results
###
### @ output: an organized (tabulated) dataframe of 2nd stage regression results
###
Present_2_Stage_Results <- function(input_outcome, trend_outcome){
  
  tab <- as.data.frame(c("2a", "2b", "2c"))
  tab <- as.data.frame(rbind("2a", "2b", "2c")) %>% dplyr::select(EQ = V1)
  
  if(input_outcome %in% air){
    tab$pi_1_SCAP <- c(as.data.frame(trend_outcome$`2aSCAP`$coefficients)$Estimate[2], 
                       as.data.frame(trend_outcome$`2bSCAP`$coefficients)$Estimate[3], 
                       as.data.frame(trend_outcome$`2cSCAP`$coefficients)$Estimate[3])
    tab$pi_1_se_SCAP <- c(as.data.frame(trend_outcome$`2aSCAP`$coefficients)$`Std. Error`[2], 
                          as.data.frame(trend_outcome$`2bSCAP`$coefficients)$`Std. Error`[3], 
                          as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Std. Error`[3])
    tab$pi_1_p_SCAP <- c(as.data.frame(trend_outcome$`2aSCAP`$coefficients)$`Pr(>|t|)`[2], 
                         as.data.frame(trend_outcome$`2bSCAP`$coefficients)$`Pr(>|t|)`[3], 
                         as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Pr(>|t|)`[3])
    tab$pi_2_SCAP <- c(NA, 
                       as.data.frame(trend_outcome$`2bSCAP`$coefficients)$Estimate[2], 
                       as.data.frame(trend_outcome$`2cSCAP`$coefficients)$Estimate[2])
    tab$pi_2_se_SCAP <- c(NA, 
                          as.data.frame(trend_outcome$`2bSCAP`$coefficients)$`Std. Error`[2], 
                          as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Std. Error`[2])
    tab$pi_2_p_SCAP <- c(NA, 
                         as.data.frame(trend_outcome$`2bSCAP`$coefficients)$`Pr(>|t|)`[2], 
                         as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Pr(>|t|)`[2])
    tab$pi_3_SCAP <- c(NA, 
                       NA, 
                       as.data.frame(trend_outcome$`2cSCAP`$coefficients)$Estimate[4])
    tab$pi_3_se_SCAP <- c(NA, 
                          NA, 
                          as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Std. Error`[4])
    tab$pi_3_p_SCAP <- c(NA, 
                         NA, 
                         as.data.frame(trend_outcome$`2cSCAP`$coefficients)$`Pr(>|t|)`[4])
    tab$five_yr_effects_SCAP <- c(NA,
                                  NA,
                                  as.data.frame(trend_outcome$`5yrEffectSCAP`)$Estimate)
    tab$pi_1_CAT <- c(as.data.frame(trend_outcome$`2aCAT`$coefficients)$Estimate[2], 
                      as.data.frame(trend_outcome$`2bCAT`$coefficients)$Estimate[3], 
                      as.data.frame(trend_outcome$`2cCAT`$coefficients)$Estimate[3])
    tab$pi_1_se_CAT <- c(as.data.frame(trend_outcome$`2aCAT`$coefficients)$`Std. Error`[2], 
                         as.data.frame(trend_outcome$`2bCAT`$coefficients)$`Std. Error`[3], 
                         as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Std. Error`[3])
    tab$pi_1_p_CAT <- c(as.data.frame(trend_outcome$`2aCAT`$coefficients)$`Pr(>|t|)`[2], 
                        as.data.frame(trend_outcome$`2bCAT`$coefficients)$`Pr(>|t|)`[3], 
                        as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Pr(>|t|)`[3])
    tab$pi_2_CAT <- c(NA, 
                      as.data.frame(trend_outcome$`2bCAT`$coefficients)$Estimate[2], 
                      as.data.frame(trend_outcome$`2cCAT`$coefficients)$Estimate[2])
    tab$pi_2_se_CAT <- c(NA, 
                         as.data.frame(trend_outcome$`2bCAT`$coefficients)$`Std. Error`[2], 
                         as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Std. Error`[2])
    tab$pi_2_p_CAT <- c(NA, 
                        as.data.frame(trend_outcome$`2bCAT`$coefficients)$`Pr(>|t|)`[2], 
                        as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Pr(>|t|)`[2])
    tab$pi_3_CAT <- c(NA, 
                      NA, 
                      as.data.frame(trend_outcome$`2cCAT`$coefficients)$Estimate[4])
    tab$pi_3_se_CAT <- c(NA, 
                         NA, 
                         as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Std. Error`[4])
    tab$pi_3_p_CAT <- c(NA, 
                        NA, 
                        as.data.frame(trend_outcome$`2cCAT`$coefficients)$`Pr(>|t|)`[4])
    tab$five_yr_effects_CAT <- c(NA,
                                 NA,
                                 as.data.frame(trend_outcome$`5yrEffectCAT`)$Estimate)
  }
  else{ 
    if(input_outcome %in% water | input_outcome %in% IM){
      tab$pi_1 <- c(as.data.frame(trend_outcome$`2a`$coefficients)$Estimate[2], 
                        as.data.frame(trend_outcome$`2b`$coefficients)$Estimate[3], 
                        as.data.frame(trend_outcome$`2c`$coefficients)$Estimate[3])
      tab$pi_1_se <- c(as.data.frame(trend_outcome$`2a`$coefficients)$`Std. Error`[2], 
                           as.data.frame(trend_outcome$`2b`$coefficients)$`Std. Error`[3], 
                           as.data.frame(trend_outcome$`2c`$coefficients)$`Std. Error`[3])
      tab$pi_1_p <- c(as.data.frame(trend_outcome$`2a`$coefficients)$`Pr(>|t|)`[2], 
                          as.data.frame(trend_outcome$`2b`$coefficients)$`Pr(>|t|)`[3], 
                          as.data.frame(trend_outcome$`2c`$coefficients)$`Pr(>|t|)`[3])
      tab$pi_2 <- c(NA, 
                        as.data.frame(trend_outcome$`2b`$coefficients)$Estimate[2], 
                        as.data.frame(trend_outcome$`2c`$coefficients)$Estimate[2])
      tab$pi_2_se <- c(NA, 
                           as.data.frame(trend_outcome$`2b`$coefficients)$`Std. Error`[2], 
                           as.data.frame(trend_outcome$`2c`$coefficients)$`Std. Error`[2])
      tab$pi_2_p <- c(NA, 
                          as.data.frame(trend_outcome$`2b`$coefficients)$`Pr(>|t|)`[2], 
                          as.data.frame(trend_outcome$`2c`$coefficients)$`Pr(>|t|)`[2])
      tab$pi_3 <- c(NA, 
                        NA, 
                        as.data.frame(trend_outcome$`2c`$coefficients)$Estimate[4])
      tab$pi_3_se <- c(NA, 
                           NA, 
                           as.data.frame(trend_outcome$`2c`$coefficients)$`Std. Error`[4])
      tab$pi_3_p <- c(NA, 
                          NA, 
                          as.data.frame(trend_outcome$`2c`$coefficients)$`Pr(>|t|)`[4])
      tab$five_yr_effects <- c(NA,
                                   NA,
                                   as.data.frame(trend_outcome$`5yrEffect`)$Estimate)
    
    }
    else{
      stop('The outcome specified is not valid. 
         \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
    }
  }
  return(tab)
}


### ---------------------------------
### Function6: Test_QLR(input_outcome)
### ---------------------------------
### This function performs Quandt Likelihood Ratio (QLR) test on event-study (first-stage) regression estimates
###
### @ input: character of policy effect estimates on one of the six pollutants or on Infant mortality rate
###          e.g. "SPM" "SO2" "NO2" "BOD" "DO" "LNFCOLI" "IM_CAT"
### @ output: QLR statistic & the position of break-point in data-frame format
###          also save the Fstats for each outcome in the results folder (./Data-Replication/Results/)
Test_QLR <- function(input_outcome){
  
  sigmas_spm <- read.dta("./Data-Replication/Air/sigmas_spm.dta")
  sigmas_so2 <- read.dta("./Data-Replication/Air/sigmas_so2.dta")
  sigmas_no2 <- read.dta("./Data-Replication/Air/sigmas_no2.dta")
  sigmas_bod <- read.dta("./Data-Replication/Water/sigmas_bod.dta")
  sigmas_lnfcoli <- read.dta("./Data-Replication/Water/sigmas_lnfcoli.dta")
  sigmas_do <- read.dta("./Data-Replication/Water/sigmas_do.dta")
  load("./sigmas-IMCAT.RData")
  
  # For air policy outcomes
  sigmas_air <- sigmas_spm %>% mutate(outcome = 'SPM') %>% 
    rbind(sigmas_so2 %>% mutate(outcome = 'SO2')) %>%
    rbind(sigmas_no2 %>% mutate(outcome = 'NO2')) %>%
    dplyr::select(tau=tauCAT, outcome, taub, tause) %>%
    mutate(category = 'air')
  
  # For water policy outcomes
  sigmas_water <- sigmas_bod %>% mutate(outcome = 'BOD') %>%
    rbind(sigmas_lnfcoli %>% mutate(outcome = 'LNFCOLI')) %>%
    rbind(sigmas_do %>% mutate(outcome = "DO")) %>%
    dplyr::select(tau, outcome, taub, tause) %>%
    mutate(category = 'water')
  
  # For infant mortality rates with respect to CAT policy
  sigmas_IM <- sigmas_IMCAT %>% mutate(outcome = 'IM_CAT') %>%
    dplyr::select(tau, outcome, taub, tause) %>%
    mutate(category = 'IM')
  
  # Final organized dataset for QLR test
  sigmas_QLR <- rbind(sigmas_air, sigmas_water, sigmas_IM) %>%
    mutate(breakpt_temp = 0, breaktd_temp = 0)
  
  # Obtain the index range for the middle 50% taus
  
  # for air/water policy effect estimates (taum7 to tau9/tau10)
  pot_breakpt <- c(-3:6)
  # for CAT policy effect estimates on Infant Mortality (taum10 to tau5)
  if(input_outcome == "IMCAT"){
    pot_breakpt <- c(-6:3)
  }
  
  test_data = sigmas_QLR %>% filter(outcome == input_outcome)
  Fstats = numeric(length(pot_breakpt))
  
  for(i in 1:length(pot_breakpt)){
    test_data$breakpt_temp = test_data$tau >= pot_breakpt[i]
    test_data$breaktd_temp = ifelse(test_data$breakpt_temp == 1, test_data$tau+(1-pot_breakpt[i]), 0)
    reduced <- lm(taub ~ tau, 
                  data=test_data, weights = 1/tause)
    full <- lm(taub ~ breakpt_temp+tau+breaktd_temp, 
               data=test_data, weights = 1/tause)
    Fstats[i] = anova(reduced, full)$F[2]
  }
  
  # save F-statistics for plotting use
  save(Fstats, file = paste0("./Data-Replication/Results/", input_outcome, "_Fstats.RData"))
  
  # print(Fstats)
  QLR = round(max(Fstats), 1)
  # print(QLR)
  tau_break = which.max(Fstats) - (1 - (min(pot_breakpt)))
  
  return(as.data.frame(cbind(input_outcome, QLR, tau_break)))
}
