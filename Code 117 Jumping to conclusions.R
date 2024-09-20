library(dplyr)
library(Synth)
library(ggplot2)
library(openxlsx)
library(tidyr)

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("\014")  #Clearer console

# Prepare data --------------------------------------------
# Load data from (Ege et al. 2023)
load("SCMFun.Rdata")

# Rename df, Week to Sequence, and CAVG5c to cExcessMortalityTotalX100000 for easier comparison to my other data
SCMFun <- SCMFun %>% 
  rename(cExcessMortalityTotalX100000 = CAVG5c,
         Sequence = Week)

# Ad YearWeek for clarity
SCMFun$YearWeek <- ifelse(SCMFun$Sequence <= 9, SCMFun$Sequence + 201943, SCMFun$Sequence - 9 + 202000)


# Loop SCM ----------------------------------------------------------------

SCMFun_EM <- SCMFun

#Set lockdown/intervention sequence week
for (j in 1:2){

  if(j == 1){ad <- 22} #corresponds to slow in Ege et al.
  if(j == 2){ad <- 20} #corresponds to fast in Ege et al. (and the week Denmark locked down)
  EgeWeek <- ad
  
  for (i in 1:2){
  
  ## Define SCMFun -----------------------------------------------------------
    rm(SCMFun)
    
    if(i==1){
      desc <- "Ege et al original (excess mortality zero in w2019-43)"
      SCMFun <- SCMFun_EM
    } else if(i == 2){ #EM = 0 in week 202011
      desc <- "Ege w. excess mortality zero in w2020-11"
      SCMFun_adj <- SCMFun_EM %>%
        group_by(Country) %>%
        mutate(baseline = ifelse(YearWeek == 202001, cExcessMortalityTotalX100000, NA)) %>%
        fill(baseline, .direction = "updown") %>%
        ungroup() %>%
        mutate(cExcessMortalityTotalX100000 = cExcessMortalityTotalX100000 - baseline) %>%
        select(Country, Sequence, cExcessMortalityTotalX100000)
      SCMFun <- left_join(SCMFun_EM %>% select(-cExcessMortalityTotalX100000),
                          SCMFun_adj,
                          by=c("Country", "Sequence"))
    }
  
  ###Define donor pool --------------------------------------
    
    rm(controls, controlunits)
    
    controls <- unique(SCMFun[c("Country", "stateid")])
    
    controlunits <- unique(controls$stateid)[unique(controls$Country) != "SWE"]
  
  # Run SCM ---------------------------------------------------------------------
  
  ## Define necessary variables for the SCM --------------------------------------
  
  #number of weeks
  FinalWeek <- max(SCMFun$Sequence)
  #number of units
  #nunits <- length(unique(SCMFun$stateid))
  nunits <- nrow(controls)
  
  #number of control units
  ncontrol <- nunits-1
  #Select control variables for SCM
  predictors <- c("gdp_per_capita","cardiovasc_death_rate_100k", "life_expectancy", "POP_DEN",  "URB_POP")
  outcome <- "cExcessMortalityTotalX100000"
  
  ### Run the model -----------------------------------------------------------
  
  dataprep.out <-
    dataprep(
      foo = SCMFun
      ,special.predictors =   list(
        list(outcome,1:ad,c("mean"))
        ,list(outcome,ad,c("mean"))
        ,list(outcome,ad-1,c("mean"))
        ,list(outcome,ad-2,c("mean"))
        ,list(outcome,ad-3,c("mean"))
        ,list(outcome,ad-4,c("mean"))
        # ,list("gdp_per_capita",1:9,c("mean")),
        # list("URB_POP",1:22,c("mean")),
        # list("life_expectancy",1:9,c("mean")),
        # list("cardiovasc_death_rate_100k",1:9,c("mean")),
        #list("POP_DEN",1:9,c("mean")))
      )
      ,predictors= predictors
      ,predictors.op = c("mean")
      ,dependent     = outcome
      ,unit.variable = c("stateid")
      ,time.variable = c("Sequence")
      ,unit.names.variable   = c("Country")
      ,treatment.identifier  = 30
      ,controls.identifier   = controlunits
      ,time.predictors.prior = c(1:ad)
      ,time.optimize.ssr     = c(1:ad)
      ,time.plot            = c(1:FinalWeek)
    )
  
  numdesc <- paste0(j, i, "_", desc, "_", EgeWeek)
  
  # run synth
  synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")
  
  synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
  
  
  ### Create graphs data --------------------------------------------------------------
  
  MyText <- paste0("Lockdown", " (Ege's week ", EgeWeek, ")")
  
  # Graphs data
  plot_data <- data.frame(
    Sequence = 1:FinalWeek,
    Actual = SCMFun[[outcome]][SCMFun$stateid == 30],  # Use dynamic column name
    Synthetic = synth.SWE  # Assuming this is already correctly aligned
  )
  
  # Melting the data frame for ggplot2
  plot_data_long <- reshape2::melt(plot_data, id.vars = "Sequence", variable.name = "Type", value.name = "Outcome")
  
  #Fix the legends
  plot_data_long$Type <- factor(plot_data_long$Type,
                                levels = c("Actual", "w.weight"),
                                labels = c("Sweden", "Synthetic"))
  
  # Fix the x-axis
  week_to_yearweek <- unique(SCMFun[c("Sequence", "YearWeek")])
  week_to_yearweek <- week_to_yearweek %>%
    mutate(Year_Week = paste(substr(YearWeek, 1, 4), substr(YearWeek, 5, 6), sep = "-"),
           Year_w_Week = paste(substr(YearWeek, 1, 4), substr(YearWeek, 5, 6), sep = "w"))
  plot_data_long <- merge(plot_data_long, week_to_yearweek, by = "Sequence", all.x = TRUE)
  
  
  
  ### Plots with full pre-intervention period ---------------
  
  # Find the position of 'ad' within the sorted unique weeks
  all_weeks <- sort(unique(plot_data_long$Sequence))
  ad_position <- match(ad, all_weeks)
  
  # Generate sequences backwards and forwards from 'ad', including 'ad' itself
  myLabelCount <- ceiling(length(all_weeks)/12)
  backward_indices <- seq(ad_position, 1, by = -myLabelCount)
  forward_indices <- seq(ad_position, length(all_weeks), by = myLabelCount)
  
  # Combine the sequences and remove duplicates
  combined_indices <- unique(c(backward_indices, forward_indices))
  combined_indices <- sort(combined_indices)  # Ensure it's sorted
  
  # Now select the custom breaks and labels using the combined indices
  custom_breaks <- all_weeks[combined_indices]
  custom_labels <- unique(plot_data_long$Year_w_Week[match(custom_breaks, plot_data_long$Sequence)])
  

  # Plotting
  MyPlot <- 
    ggplot(plot_data_long, aes(x = Sequence, y = Outcome, linetype = Type)) +
    geom_line() +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"),  # Set panel background to white
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels
    scale_x_continuous(name = NULL, breaks = custom_breaks, labels = custom_labels) +
    scale_color_manual(values = rep("black", length(unique(plot_data_long$Type)))) +  # Ensure one color per Type
    scale_linetype_manual(values = c("solid", "dotted")) +
    labs(title = NULL,
         y = "Cumulated Excess Mortality/100,000",
         linetype  = NULL) +  #Removes default legend title
    geom_vline(xintercept = ad, linetype="dashed", color = "black", size = 1, show.legend = FALSE) +
    annotate("text", x = ad-myLabelCount/4, y = 0, label = MyText, size = 3, color = "black", angle = 90, hjust = 0) +
    ylim(-20, 60)
  
  # Add dotted line with limit of Ege et al's analysis period, if YearWeek > 202039
  if(max(plot_data_long$YearWeek) > 202039) {
    MyPlot <- MyPlot +
      geom_vline(xintercept = ad + 28, linetype = "dotted", color = "black", size = .3, show.legend = FALSE) +
      annotate("text", x = ad + 28 - 2.5, y = 75, label = "Limit of Ege et al.", size = 4, color = "black", angle = 90, hjust = 0)
  }
  MyPlot
  
  ggsave(paste0(numdesc, ".png"), plot = MyPlot, width = 5, height = 5, dpi = 300, bg = "white")
  
  }  #Next i
} #Next j