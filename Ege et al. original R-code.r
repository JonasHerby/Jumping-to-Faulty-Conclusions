library(Synth)
library(gapminder)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggplotify)
library(grid)
library(ggpubr)
library(gridGraphics)
library(scpi)
library(augsynth)
library(synthdid)
library(readxl)
library(xtable)


################################# Changes by Jonas Herby #################################
# Files downloaded from https://www.nature.com/articles/s41598-023-45934-2#Sec17 September 19, 2024
# This file is identical to "41598_2023_45934_MOESM4_ESM.r"
# The file "41598_2023_45934_MOESM5_ESM.rdata" has been renamed to "SCMFun.rdata"
# Save this file and SCMFun.Rdata in the same directory
# Set directory to source code easily:
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
##### End of changes by Jonas Herby. The rest of the files is as given by Ege et al. #####


#
#The final data have been created from the following data sources:
# Mortality data (Short-Term Mortality Fluctuations) https://www.mortality.org/Data/STMF
# GDP per capita 2020 (Worldbank) https://ourworldindata.org/grapher/gdp-per-capita-worldbank?tab=chart&country=AUT
# Age-standardized death rate from cardiovascular diseases 2019 (Our World in Data) https://ourworldindata.org/grapher/age-standardized-death-rate-cardiovascular-disease?tab=chart&country=AUT
# Life expectancy 2019 (Worldbank) https://ourworldindata.org/grapher/life-expectancy-at-birth-total-years?tab=chart&country=AUT
# Share of people living in urban agglomerations of more than 1 million 2020 (Our World in Data) https://ourworldindata.org/grapher/share-of-population-urban?tab=chart&time=latest&country=AUT
# Population density 2020 (Our World in Data) https://ourworldindata.org/grapher/population-density?tab=chart&time=1851..2022&country=~AUT
# Links accessed on 10-10-2023

#load the data
load("SCMFun.Rdata")

# Define treatment starting date 
fast<- 20
medium<- 21
slow<-22
#number of weeks
FinalWeek<-length(unique(SCMFun$Week))
#number of units
nunits<-length(unique(SCMFun$stateid))
#number of control units
ncontrol<-nunits-1
#Select control variables for SCM
predictors<-c("gdp_per_capita","cardiovasc_death_rate_100k", "life_expectancy", "POP_DEN",  "URB_POP")

###########  Results for over and under 65 population and week 22 as treatment starting date ########### 
ad<-slow
#over 65
#Select excess mortality as outcome 
outcome<- "CVG5c65p"

#Prepare data for SCM

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
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

#Run SCM, we use all possible optimization methods this is slower but more accurate
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))


pdf("CAVG5cPlot65p.pdf", width=6, heigh=4)
# plot results:
#this is the main plot 
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths per 100.000",
                  Xlab = "Weeks",
                  Ylim = c(-60,60),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)


dev.off()

#under 65


outcome<- "CVG5c0_64"






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
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))


pdf("CAVG5cPlot0_64.pdf", width=6, heigh=4)
# plot results:
#this is the main plot 
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths per 100.000",
                  Xlab = "Weeks",
                  Ylim = c(-60,60),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)


dev.off()









########### Week 20 as treatment starting date ########### 
ad<-fast

outcome<- "CAVG5c"


  
  


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
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))


pdf("CAVG5cPlot20.pdf", width=6, heigh=4)
# plot results:
#this is the main plot 
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths per 100.000",
                  Xlab = "Weeks",
                  Ylim = c(-60,60),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)


dev.off()

outcome<- "CAVG5r"
dataprep.out <-
  dataprep(
    foo = SCMFun
    ,special.predictors = list(
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
    ,dependent     = c(outcome)
    ,unit.variable = c("stateid")
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))

pdf("CAVG5rPlot20.pdf", width=6, heigh=4)
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths rates",
                  Xlab = "Weeks",
                  Ylim = c(-.05,.05),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-.03,ad-1,-.03,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-.03,"Lockdowns",cex=Cex.set)




dev.off()





########### Week 21 as treatment starting date ########### 
ad<-medium
outcome<- "CAVG5c"
dataprep.out <-
  dataprep(
    foo = SCMFun
    ,special.predictors = list(
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
    ,dependent     = c(outcome)
    ,unit.variable = c("stateid")
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))


pdf("CAVG5cPlot21.pdf", width=6, heigh=4)
# plot results:
#this is the main plot 
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths per 100.000",
                  Xlab = "Weeks",
                  Ylim = c(-60,60),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)


dev.off()

outcome<- "CAVG5r"
dataprep.out <-
  dataprep(
    foo = SCMFun
    ,special.predictors = list(
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
    ,dependent     = c(outcome)
    ,unit.variable = c("stateid")
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))

pdf("CAVG5rPlot21.pdf", width=6, heigh=4)
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths rates",
                  Xlab = "Weeks",
                  Ylim = c(-.05,.05),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-.03,ad-1,-.03,col="black",length=.1)
Cex.set <- 1#size of the text
# this add the text
text(ad-7,-.03,"Lockdowns",cex=Cex.set)




dev.off()





########### Week 22 as treatment starting date ########### 
ad<-slow
#counts 
outcome<- "CAVG5c"
dataprep.out <-
  dataprep(
    foo = SCMFun
    ,special.predictors = list(
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
    ,dependent     = c(outcome)
    ,unit.variable = c("stateid")
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")
#compute aviadable deaths

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
max22c<-(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))
deaths22<-sum(SCMFun$Deaths[SCMFun$stateid==30 & SCMFun$Week>ad ])
Pop_Swe<-unique(SCMFun$Population100000)[30]
avoidable_deaths<-round(Pop_Swe*max22c)





pdf("CAVG5cPlot22.pdf", width=6, heigh=4)
# plot results:
#this is the main plot 
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths per 100.000",
                  Xlab = "Weeks",
                  Ylim = c(-60,60),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)

dev.off()





# Creates table 1
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

# results tables:
print(synth.tables)

predw<-synth.tables$tab.v
predw<-predw
predw<-as.numeric(predw)*100
pred<-cbind(synth.tables$tab.pred,paste(predw, "\ %", sep=""))
rownames(pred)<-  c(
  "GDP per capita",
  "Cardiov. deaths",
  "Life   expectancy",
  "Population density",
  "Urban population",
  "Deaths (Average)",
  "Deaths (Week   22)",
  "Deaths (Week   21)",
  "Deaths (Week   20)",
  "Deaths (Week   19)",
  "Deaths (Week   18)")


xtable(pred, caption = "Covariate balance", digits = 2)
# Save  weights for the Mobility analysis and set weights to 0 for those receiving less than 1%
#predMobility <- transform(synth.tables$tab.w, w.weights = ifelse(w.weights < 0.01, 0, w.weights))


#leave-one-out




# Select units that received more than 0.9% weight

weights<-as.matrix(synth.tables$tab.w[,1])
rownames(weights)<-c(1:ncontrol)

units<-as.numeric(c(rownames(subset(weights,weights>.009))))
u<-c(1:ncontrol)
#I create a matrix to store the alternative synthetic units. 
Weeks<-1:FinalWeek
storey0lol<- 
  matrix(NA,
         length(1:FinalWeek),
         length(units)) 
#colnames(storey0lol) <- units
# I now estimate a new synthetic Sweden leaving out one of the control unit each time 


for(k in 1:length(units)){ 
  omit <- units[k]
  

  
  
  dataprep.out.lol <-
    dataprep(
      foo = SCMFun
      ,special.predictors = list(
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
      ,dependent     = c(outcome)
      ,unit.variable = c("stateid")
      ,time.variable = c("Week")
      ,unit.names.variable   = c("Country")
      ,treatment.identifier  = 30
      ,controls.identifier   = u[-which(u==units[k])]
      ,time.predictors.prior = c(1:ad)
      ,time.optimize.ssr     = c(1:ad)
      ,time.plot            = c(1:FinalWeek)
    )
  
  
  
  
  # run synth
  synth.out.lol <- synth(data.prep.obj = dataprep.out.lol, optimxmethod="All")
  
  
  
  
  storey0lol[,k] <- (dataprep.out.lol$Y0%*%synth.out.lol$solution.w) 
}
#plot
pdf("CAVG5cLOO22.pdf", width=6, heigh=4)

loo1 <- path.plot(synth.res = synth.out,
                            dataprep.res = dataprep.out,
                            Ylab = "Cumulative Excess Deaths per 100.000",
                            Xlab = "Weeks",
                            Ylim = c(-60,60),
                            Legend = c("Sweden","synthetic Sweden"),
                            Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)


#here I add the new synthetic units lines

for(i in 1:length(units)){
  lines(Weeks,storey0lol[,i],col="darkgrey",lty="dotted")  
#  text(ad-2+3*i, storey0lol[ad-2+3*i,i], unique(SCMFun$Country)[which(u==units[i])],cex=.5)
}


dev.off()




store <- matrix(NA,length(1:FinalWeek),nunits) #creates a matrix were the gaps for each units will be stored
colnames(store) <- unique(SCMFun$Country) #name each column

# estimate a fake effect using each unit in the donor pool as treated 
for(iter in c(1:nunits))
{
  print(iter)
  dataprep.out <-
    dataprep(
      foo = SCMFun
      ,special.predictors = list(
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
      ,dependent     = c(outcome)
      ,unit.variable = c("stateid")
      ,time.variable = c("Week")
      ,unit.names.variable   = c("Country")
      ,treatment.identifier  = iter
      ,controls.identifier   = u[-iter]
      ,time.predictors.prior = c(1:ad)
      ,time.optimize.ssr     = c(1:ad)
      ,time.plot            = c(1:FinalWeek)
    )
  
  
  
  
  # run synth
  synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

  
  # store gaps
  store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# now do figure
data <- store
rownames(data) <- 1:FinalWeek

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
Weeks         <- 1:FinalWeek
gap.end.pre  <- which(rownames(data)=="22")


pdf("CAVG5cPlacebo22.pdf", width=6, heigh=4)

# Plot placebo
#first plot the treated gaps
plot(Weeks,data[gap.start:gap.end,which(colnames(data)=="SWE")],
     ylim=c(-100,100),xlab="Weeks",
     xlim=c(1,FinalWeek),ylab="Cumulative Excess Deaths per 100.000",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")


u2<-u
# add gaps of the donor pool units
for (i in u2) { lines(Weeks,data[gap.start:gap.end,i],col="gray")
#  text(40, data[40,i], unique(SCMFun$Country)[i],cex=.5)
}

## Add Sweden Line
lines(Weeks,data[gap.start:gap.end,which(colnames(data)=="SWE")],lwd=2,col="black")

# Add lines
abline(v=ad,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)

# this add the arrow
arrows(ad-2,-24,ad-1,-24,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-24,"Lockdowns",cex=Cex.set)
#abline(v=50)
dev.off()

################################################################################
################################Rates###########################################
################################################################################

outcome<- "CAVG5r"
dataprep.out <-
  dataprep(
    foo = SCMFun
    ,special.predictors = list(
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
    ,dependent     = c(outcome)
    ,unit.variable = c("stateid")
    ,time.variable = c("Week")
    ,unit.names.variable   = c("Country")
    ,treatment.identifier  = 30
    ,controls.identifier   = c(1:(nunits-1))
    ,time.predictors.prior = c(1:ad)
    ,time.optimize.ssr     = c(1:ad)
    ,time.plot            = c(1:FinalWeek)
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

synth.SWE<-(dataprep.out$Y0plot%*%synth.out$solution.w)
(max(SCMFun$CAVG5c[SCMFun$stateid==30 & SCMFun$Week>ad ]-synth.SWE[(ad+1):FinalWeek,]))

pdf("CAVG5rPlot22.pdf", width=6, heigh=4)
scm1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths rates",
                  Xlab = "Weeks",
                  Ylim = c(-.05,.05),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-.03,ad-1,-.03,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-.03,"Lockdowns",cex=Cex.set)




dev.off()



synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

print(synth.tables)

predw<-synth.tables$tab.v
predw<-predw
predw<-as.numeric(predw)*100
pred<-cbind(synth.tables$tab.pred,paste(predw, "\ %", sep=""))
rownames(pred)<-  c(
  "GDP per capita",
  "Cardiov. deaths",
  "Life   expectancy",
  "Population density",
  "Urban population",
  "Rates (Average)",
  "Rates (Week   22)",
  "Rates (Week   21)",
  "Rates (Week   20)",
  "Rates (Week   19)",
  "Rates (Week   18)")


xtable(pred, caption = "Covariate balance", digits = 2)


#leave-one-out




# Select units that received more than 0.9% weight

weights<-as.matrix(synth.tables$tab.w[,1])
rownames(weights)<-c(1:ncontrol)

units<-as.numeric(c(rownames(subset(weights,weights>.009))))
u<-c(1:ncontrol)
#I create a matrix to store the alternative synthetic units. 
Weeks<-1:FinalWeek
storey0lol<- 
  matrix(NA,
         length(1:FinalWeek),
         length(units)) 
#colnames(storey0lol) <- units
# I now estimate a new synthetic Sweden leaving out one of the control unit each time 


for(k in 1:length(units)){ 
  omit <- units[k]
  
  
  
  
  dataprep.out.lol <-
    dataprep(
      foo = SCMFun
      ,special.predictors = list(
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
      ,dependent     = c(outcome)
      ,unit.variable = c("stateid")
      ,time.variable = c("Week")
      ,unit.names.variable   = c("Country")
      ,treatment.identifier  = 30
      ,controls.identifier   = u[-which(u==units[k])]
      ,time.predictors.prior = c(1:ad)
      ,time.optimize.ssr     = c(1:ad)
      ,time.plot            = c(1:FinalWeek)
    )
  
  
  
  
  # run synth
  synth.out.lol <- synth(data.prep.obj = dataprep.out.lol, optimxmethod="All")
  
  
  
  
  storey0lol[,k] <- (dataprep.out.lol$Y0%*%synth.out.lol$solution.w) 
}




pdf("CAVG5rLOO22.pdf", width=6, heigh=4)

loo1 <- path.plot(synth.res = synth.out,
                  dataprep.res = dataprep.out,
                  Ylab = "Cumulative Excess Deaths rates",
                  Xlab = "Weeks",
                  Ylim = c(-.05,.05),
                  Legend = c("Sweden","synthetic Sweden"),
                  Legend.position = "bottomright"
)
#this adds a vertical line on the treatment date



#here I add the new synthetic units lines

for(i in 1:length(units)){
  lines(Weeks,storey0lol[,i],col="darkgrey",lty="dotted")  
 # text(20+3*i, storey0lol[20+3*i,i], unique(SCMFun$Country)[which(u==units[i])],cex=.5)
}

#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-.03,ad-1,-.03,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-.03,"Lockdowns",cex=Cex.set)


dev.off()




store <- matrix(NA,length(1:FinalWeek),nunits) #creates a matrix were the gaps for each units will be stored
colnames(store) <- unique(SCMFun$Country) #name each column

# estimate a a fake effect using each unit in the donor pool as treated 
for(iter in c(1:nunits))
{
  print(iter)
  dataprep.out <-
    dataprep(
      foo = SCMFun
      ,special.predictors = list(
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
      ,dependent     = c(outcome)
      ,unit.variable = c("stateid")
      ,time.variable = c("Week")
      ,unit.names.variable   = c("Country")
      ,treatment.identifier  = iter
      ,controls.identifier   = u[-iter]
      ,time.predictors.prior = c(1:ad)
      ,time.optimize.ssr     = c(1:ad)
      ,time.plot            = c(1:FinalWeek)
    )
  
  
  
  
  # run synth
  synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod="All")

  
  # store gaps
  store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# now do figure
data <- store
rownames(data) <- 1:FinalWeek

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
Weeks         <- 1:FinalWeek
gap.end.pre  <- which(rownames(data)=="22")


pdf("CAVG5rPlacebo22.pdf", width=6, heigh=4)

# Plot placebo
#first plot the treated gaps
plot(Weeks,data[gap.start:gap.end,which(colnames(data)=="SWE")],
     ylim=c(-.05,.05),xlab="Weeks",
     xlim=c(1,FinalWeek),ylab="Cumulative Excess Deaths rates",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# add gaps of the donor pool units
for (i in 1:length(u)) { lines(Weeks,data[gap.start:gap.end,i],col="gray")
#  text(40, data[40,i], unique(SCMFun$Country)[which(u==u[i])],cex=.5)
}

## Add Sweden Line
lines(Weeks,data[gap.start:gap.end,which(colnames(data)=="SWE")],lwd=2,col="black")

# Add line at 0
abline(h=0,lty="dashed",lwd=2)
#this adds a vertical line on the treatment date
abline(v=ad,lty="dotted",lwd=2)
# this add the arrow
arrows(ad-2,-.03,ad-1,-.03,col="black",length=.1)
Cex.set <- 1 #size of the text
# this add the text
text(ad-7,-.03,"Lockdowns",cex=Cex.set)

dev.off()

##########SDiD and Augsynth####################

#create data for augmented augsynth

augdata<-SCMFun[,c("stateid","Week","CAVG5r","CAVG5c","AVG5r","AVG5c","gdp_per_capita", "POP_DEN", "cardiovasc_death_rate_100k", "life_expectancy", "URB_POP")]
augdata[,"d"]<-(augdata$stateid==30)*(augdata$Week>ad)


country_ida<-data.frame(cbind(unique(SCMFun$Country),unique(SCMFun$stateid)))


######counts######
#run augsynth 
asyn_cov <- augsynth(CAVG5c ~d|gdp_per_capita+POP_DEN+cardiovasc_death_rate_100k+life_expectancy+URB_POP
                ,stateid,Week,augdata, progfunc = "None")
#create plot with confidence intervals based on jackknife+
pdf("augsynth_covc.pdf", width=6, heigh=4)
plot(asyn_cov, inf_type = "jackknife+")
dev.off()

summary(asyn_cov)



#run Synth-DiD

#prepare data for Synthetic DiD
sddata<-augdata[,c("stateid","Week","CAVG5c","d")]
#sddata<-sddata[sddata$Week<40,]
setup = panel.matrices(sddata)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
#create plot with placebo test
pdf("sdid_placeboc.pdf", width=6, heigh=4)
synthdid_placebo_plot(tau.hat, overlay = T)
dev.off()


######rates######

#run augsynth 
asyn_cov_r <- augsynth(CAVG5r ~d|gdp_per_capita+POP_DEN+cardiovasc_death_rate_100k+life_expectancy+URB_POP
                     ,stateid,Week,augdata, progfunc = "None")
#create plot with confidence intervals based on jackknife+
pdf("augsynth_covr.pdf", width=6, heigh=4)
plot(asyn_cov_r, inf_type = "jackknife+")
dev.off()

#run Synth-DiD

#prepare data for Synthetic DiD
sddata_r<-augdata[,c("stateid","Week","CAVG5r","d")]
setupr = panel.matrices(sddata_r)
tau.hatr = synthdid_estimate(setupr$Y, setupr$N0, setupr$T0)
ser = sqrt(vcov(tau.hatr, method='placebo'))
sprintf('point estimate: %1.2f', tau.hatr)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hatr - 1.96 * ser, tau.hatr + 1.96 * ser)
#create plot with placebo test
pdf("sdid_placebor.pdf", width=6, heigh=4)
synthdid_placebo_plot(tau.hatr, overlay = T)
dev.off()


# Mobility data are constructed from Google Community Mobility Reports (https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip)
# Google does not provide mobility data for Iceland, so it's mobility data is missing despite being in our donor pool - yet Iceland has not recieved any weights when constructing our SCM models
#load cleaned data
load("plot_data_workplaces.RData")
load("plot_data_residential.RData")


# Specific dates corresponding to the weeks
start_date <- as.Date("2020-02-15")
end_date_week_50 <- max(plot_data_workplaces$date)
dates_for_labels <- as.Date(c("2020-02-24", "2020-05-25", "2020-08-31", "2020-12-07"))
week_labels <- c("20", "30", "40", "50")



# Open the PDF device
pdf("MobilityWorkplaces.pdf", width=6, height=4)

# Plotting with the coord_cartesian to adjust the visible x-axis range
ggplot(data = plot_data_workplaces, aes(x = date, y = mobility, group = group, colour = group, linetype = group)) + 
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  labs(title = "",
       x = "Week", 
       y = "Workplace (Percentage Change from Baseline)") + 
  scale_x_date(breaks = dates_for_labels,
               labels = week_labels) +
  scale_color_manual(values = c("Sweden" = "black", "Synthetic Sweden" = "black")) +
  scale_linetype_manual(values = c("Sweden" = "solid", "Synthetic Sweden" = "dashed")) +
  coord_cartesian(xlim = c(start_date, end_date_week_50)) +  # Adjust the visible x-axis range without removing data points
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = c(1, 0),     # Bottom right corner
        legend.justification = c(1, 0) # Justify to the bottom right corner
  )

# Close the PDF device
dev.off()

pdf("MobilityResidential.pdf", width=6, height=4)

# Plot the residential data
ggplot(data = plot_data_residential, aes(x = date, y = mobility, group = group, colour = group, linetype = group)) + 
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  labs(title = "",
       x = "Week", 
       y = "Residential (Percentage Change from Baseline)") + 
  scale_x_date(breaks = dates_for_labels,
               labels = week_labels) +
  scale_color_manual(values = c("Sweden" = "black", "Synthetic Sweden" = "black")) +
  scale_linetype_manual(values = c("Sweden" = "solid", "Synthetic Sweden" = "dashed")) +
  coord_cartesian(xlim = c(start_date, end_date_week_50)) +  # Adjust the visible x-axis range without removing data points
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = c(1, 0),     # Bottom right corner
        legend.justification = c(1, 0) # Justify to the bottom right corner
  )

# Close the PDF device
dev.off()


