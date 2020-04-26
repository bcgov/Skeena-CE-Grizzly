#develop relationship between bear density and amount of core
#use to estimate future bear densities in landscapes based on amount of core
#needed for ranking - estimating population size and trend
#By Don Morgan, MoE, 250.877.3199

#Summarize provincial LUs and GBPUs for bear density vs amount core security - 
#generate relationship and use to determine future bear populations with shifts in security

#Required packages
options(scipen=3)
gpclibPermit()
require(maptools)
require(shapefiles)
require(gpclib)
gpclibPermit()
require(RColorBrewer) # creates nice color schemes
require(e1071) #package needed for classInt
require(classInt) # finds class intervals for continuous variables
require(png) # next 3 for pulling in google maps
require(RJSONIO)
require(RgoogleMaps)
require(maps) # for scale bar
require(rgeos)
require(GISTools)
require(dismo)
require(spatialEco)
require(rgdal)
#Required Packages
require(dplyr)
require(tidyverse)
require(sf)
require(raster)
require(rgdal)
require(sp)
require(XML)
require(plyr)


LU_Summ_in <- data.frame(read.csv(header=TRUE, file=file.path(BearsCEDir, "GBear_LU_Summary_scores_v5_20160823.csv"), sep=",", strip.white=TRUE, ))

#Calculate number of bears using LU_gbear_pop_est_temp for each LU - could also use density LU_gbearDens
#cross reference with area of Core in unit - Core_BEI_cap_AreaKM2_wght
LUfields<-c('MAX_GBPU_POPULATION_NAME','LANDSCAPE_UNIT_NAME','Core_BEI_cap_AreaKM2_wght','LU_gbear_pop_est_temp')
LU<-data.frame(LU_Summ_in[(names(LU_Summ_in) %in% LUfields)])

#get rid of outliers for data checking
LUs<-subset(LU,LU$LU_gbear_pop_est_temp<50 & LU$Core_BEI_cap_AreaKM2_wght<1000)
plot(LUs$Core_BEI_cap_AreaKM2_wght,LUs$LU_gbear_pop_est_temp)

#amalgamate to GBPU
#Function for collapsing indicators to strata
#Collapse to strata and summarize, using reporting function
StratIndFn <- function(dataset, StratIN, IndsIN){
  dataset %>% 
    group_by_(.dots=StratIN) %>%
    summarise_at((.dot=IndsIN), funs(sum))
  #return(dataset)
}

#Set strata variables and pass in indicators to be collapsed and apply strata function
Strata<-c('MAX_GBPU_POPULATION_NAME')
Indicators<-c('Core_BEI_cap_AreaKM2_wght','LU_gbear_pop_est_temp')
StrataDF<-data.frame(Strata)
numStrats<-1
GB1<-StratIndFn(LU, Strata, Indicators)


#Plot GBPU popualation to area of core
plot(GB1$Core_BEI_cap_AreaKM2_wght,GB1$LU_gbear_pop_est_temp)



cor(data.frame(GB1[(names(GB1) %in% Indicators)]), use="all.obs", method="pearson") 
#Next do a linear regression and build a model to use to predict GBPU populations
#Could use all GBPUs expcept the ones in the Skeena then test the model and see if get same relationship against real data?
# https://www.r-bloggers.com/correlation-and-linear-regression/


