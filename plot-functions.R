# ------------------------------------------------------------------------------------------------------
# File: plot-functions.R 
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

# Objective: Prepare plotting functions for replication.
#
# Structure:
#           Plot Function1: plotTheme()
#           Plot Function2: PLot_Event_Study_Estimates(input_outcome)
#           Plot Function3: PLot_Fstats(input_outcome)


### --------------------------
### Plot Function1: plotTheme()
### --------------------------
### Set up basics for plotting
###
plotTheme <- function(base_size = 12, title_size = 16) {
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(size = title_size, colour = "black"), 
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}


### --------------------------------------------------------
### Plot Function2: PLot_Event_Study_Estimates(input_outcome)
### --------------------------------------------------------
### Plot event study estimates with respect to event years
###
Plot_Event_Study_Estimates <- function(input_outcome){
  
  if(input_outcome == "SPM"){
    data_for_plot = SPM_sigmas
    
    xlab_textSC = "Years since action plan mandated"
    ylab_textSC = "Effect on PM"
    x_limitsSC= c(-7,-6,-3,0,3)
    y_breaksSC = seq(-20, 40, 20)
    y_limitsSC = c(-20,40)
    
    xlab_textCAT = "Years since catalytic converters mandated"
    ylab_textCAT = "Effect on PM"
    x_limitsCAT= c(-7,-6,-3,0,3,6,9)
    y_breaksCAT = seq(-60, 20, 20)
    y_limitsCAT = c(-60,24)
    
    SC_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauSC, norm_taubSC),color='darkgreen') +
      geom_line(aes(tauSC, norm_taubSC),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textSC, limits = x_limitsSC) + 
      scale_y_continuous(name = ylab_textSC, breaks = y_breaksSC, limits = y_limitsSC) + 
      plotTheme()
  
    SC_plot <- SC_plot + labs(subtitle = 'Effects of Supreme Court Action Plan on Particulate Matter')
    print(SC_plot)
    
    CAT_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauCAT, norm_taubCAT),color='darkgreen') +
      geom_line(aes(tauCAT, norm_taubCAT),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textCAT, limits = x_limitsCAT) + 
      scale_y_continuous(name = ylab_textCAT, breaks = y_breaksCAT, limits = y_limitsCAT) + 
      plotTheme()
    
    CAT_plot <- CAT_plot + labs(subtitle = 'Effects of Catalytic Converters Policy on Particulate Matter')
    print(CAT_plot)
  }
  else if(input_outcome == "SO2"){
    data_for_plot = SO2_sigmas
    
    xlab_textSC = "Years since action plan mandated"
    ylab_textSC = "Effect on SO2"
    x_limitsSC= c(-7,-6,-3,0,3)
    y_breaksSC = seq(-1.5, 0.5, 0.5)
    y_limitsSC = c(-1.5,0.65)
    
    xlab_textCAT = "Years since catalytic converters mandated"
    ylab_textCAT = "Effect on SO2"
    x_limitsCAT= c(-7,-6,-3,0,3,6,9)
    y_breaksCAT = seq(-15, 0, 5)
    y_limitsCAT = c(-15,0.5)
    
    SC_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauSC, norm_taubSC),color='darkgreen') +
      geom_line(aes(tauSC, norm_taubSC),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textSC, limits = x_limitsSC) + 
      scale_y_continuous(name = ylab_textSC, breaks = y_breaksSC, limits = y_limitsSC) + 
      plotTheme()
    
    SC_plot <- SC_plot + labs(subtitle = 'Effects of Supreme Court Action Plan on Sulfur Dioxide')
    print(SC_plot)
    
    CAT_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauCAT, norm_taubCAT),color='darkgreen') +
      geom_line(aes(tauCAT, norm_taubCAT),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textCAT, limits = x_limitsCAT) + 
      scale_y_continuous(name = ylab_textCAT, breaks = y_breaksCAT, limits = y_limitsCAT) + 
      plotTheme()
    
    CAT_plot <- CAT_plot + labs(subtitle = 'Effects of Catalytic Converters Policy on Sulfur Dioxide')
    print(CAT_plot)
    
  }
  else if(input_outcome == "NO2"){
    data_for_plot = NO2_sigmas
    
    xlab_textSC = "Years since action plan mandated"
    ylab_textSC = "Effect on NO2"
    x_limitsSC= c(-7,-6,-3,0,3)
    y_breaksSC = seq(-10, 0, 2)
    y_limitsSC = c(-10,0.2)
    
    xlab_textCAT = "Years since catalytic converters mandated"
    ylab_textCAT = "Effect on NO2"
    x_limitsCAT= c(-7,-6,-3,0,3,6,9)
    y_breaksCAT = seq(-10, 5, 5)
    y_limitsCAT = c(-10,5)
    
    SC_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauSC, norm_taubSC),color='darkgreen') +
      geom_line(aes(tauSC, norm_taubSC),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textSC, limits = x_limitsSC) + 
      scale_y_continuous(name = ylab_textSC, breaks = y_breaksSC, limits = y_limitsSC) + 
      plotTheme()
    
    SC_plot <- SC_plot + labs(subtitle = 'Effects of Supreme Court Action Plan on Nitrogen Dioxide')
    print(SC_plot)
    
    CAT_plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tauCAT, norm_taubCAT),color='darkgreen') +
      geom_line(aes(tauCAT, norm_taubCAT),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_textCAT, limits = x_limitsCAT) + 
      scale_y_continuous(name = ylab_textCAT, breaks = y_breaksCAT, limits = y_limitsCAT) + 
      plotTheme()
    
    CAT_plot <- CAT_plot + labs(subtitle = 'Effects of Catalytic Converters Policy on Nitrogen Dioxide')
    print(CAT_plot)
  }
  else{
    if(input_outcome == "BOD"){
      data_for_plot = BOD_sigmas
      xlab_text = "Years since added to NRCP"
      ylab_text = "Effect on BOD"
      x_limits= c(-7,-6,-3,0,3,6,9,10)
      y_breaks = seq(-2, 8, 2)
      y_limits = c(-2,8)
      subtitle_ = "Effects of National River Conservation Plan on Biochemical Oxygen Demand"
    }
    else if(input_outcome == "LNFCOLI"){
      data_for_plot = LNFCOLI_sigmas
      xlab_text = "Years since added to NRCP"
      ylab_text = "Effect on ln(Fcoli)"
      x_limits= c(-7,-6,-3,0,3,6,9,10)
      y_breaks = seq(-1.5, 0.5, 0.5)
      y_limits = c(-1.5,0.5)
      subtitle_ = "Effects of National River Conservation Plan on Log-transformed Fecal Coliforms"
    }
    else if(input_outcome == "DO"){
      data_for_plot = DO_sigmas
      xlab_text = "Years since added to NRCP"
      ylab_text = "Effect on DO"
      x_limits= c(-7,-6,-3,0,3,6,9,10)
      y_breaks = seq(-0.4, 0.2, 0.2)
      y_limits = c(-0.43,0.24)
      subtitle_ = "Effects of National River Conservation Plan on Dissolved Oxygen"
    }
    else if(input_outcome == "IM_CAT"){
      data_for_plot = IM_CAT_sigmas
      xlab_text = "Years since catalytic converters mandated"
      ylab_text = "Effect on IM rate"
      x_limits= c(-10,-5,0,5)
      y_breaks = seq(-4, 4, 2)
      y_limits = c(-5,4)
      subtitle_ = "Effects of Catalytic Converters Policy on Infant Mortality Rate"
    }
    else{
      stop('The outcome specified is not valid. 
           \n Please use one of the following: "SPM" "NO2" "SO2" "BOD" "LNFCOLI" "DO" "IM_CAT"')
    }
    plot <- ggplot(data = data_for_plot) +
      geom_point(aes(tau, norm_taub),color='darkgreen') +
      geom_line(aes(tau, norm_taub),color='darkgreen') + 
      geom_hline(yintercept = 0, color='red', linetype=2) + 
      geom_vline(xintercept = 0, color='red',linetype=2) + 
      scale_x_discrete(name = xlab_text, limits = x_limits) + 
      scale_y_continuous(name = ylab_text, breaks = y_breaks, limits = y_limits) + 
      plotTheme()
    
    plot <- plot + labs(subtitle = subtitle_)
    print(plot)
  }
}


### -----------------------------------------
### Plot Function3: PLot_Fstats(input_outcome)
### -----------------------------------------
### Plot F-stats with respect to event years
###
Plot_Fstats <- function(input_outcome){
  load(paste0("./Data-Replication/Results/", input_outcome, "_Fstats.RData"))
  
  outcomeplot = input_outcome
  # for air/water policy effect estimates (taum7 to tau9/tau10)
  pot_breakpt <- c(-3:6)
  xlim_ <-c(-4,6)
  if(input_outcome == "LNFCOLI"){
    outcomeplot = "ln(Fcoli)"
  }
  # for CAT policy effect estimates on Infant Mortality (taum10 to tau5)
  if(input_outcome == "IM_CAT"){
    pot_breakpt <- c(-6:3)
    xlim_ <- c(-6,2)
    outcomeplot = "Infant Mortality Rate"
  }
  
  # png(file = paste0("./Data-Replication/Results/", input_outcome,'_Fstats.png'), width=400, height=300)
  Fplot <- as.data.frame(cbind(pot_breakpt, Fstats))
  a <- ggplot(data=Fplot) +
    geom_line(aes(pot_breakpt, Fstats), color="blue")+ 
    geom_hline(yintercept = max(Fstats), color="gray", linetype=5) + 
    geom_vline(xintercept = which.max(Fstats) - (1 - (min(pot_breakpt))),color="gray",linetype=5) + 
    xlab("Event time") +
    ylab("F-statistics") + xlim(xlim_)+
    plotTheme() + 
    labs(subtitle = outcomeplot)
  print(a)
  # dev.off()
}
