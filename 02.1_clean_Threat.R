# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source("header.R")

#Setup indicators - remove non-habitat, etc prior to aggregating to strata
#Some indicators get summarized (areas) others get passed through to strata for 
#final calculation (proportions)
#Threat 1 - Residential and Commercial Development
GrizzlyInd1$ResComm<-GrizzlyInd1$aLU_Urban+GrizzlyInd1$aLU_Industrial
#Threat 2 - Agriculture
GrizzlyInd1$Agg<-GrizzlyInd1$aLU_Agriculture + GrizzlyInd1$aLU_Range
#Threat 3.1 - Energy Production & Mining 
#passed directly require summarizing once stratified
# 'kmPipelines_existing','kmPipelines_proposed'
#Threat 3.2 - Energy Production & Mining
GrizzlyInd1$nMines<-GrizzlyInd1$nMines_Aggregate+GrizzlyInd1$nMines_Coal+GrizzlyInd1$nMines_Mineral
#Threat 3.3 - Energy Production & Mining
# 'nWindFarms'
#Threat 4 - Transportation & Service Corridors MULTIPLY by 2 till get new roads
GrizzlyInd1$aRdDensity<-(GrizzlyInd1$aRdDensityCls_4+GrizzlyInd1$aRdDensityCls_5)*2
#Threat 5.1a - Biological Resource Use- Mortality -Summarized in 03_analysis_Threat once aggregated
#Threat 5.1b - Biological Resource Use- Hunter Density - Summarized in 03_analysis_Threat once aggregated
#Threat 5.3 - 'aMidSeralConif', 'aConif' - Summarized in 03_analysis_Threat once aggregated
#Threat 6.1 Human Intrusion
GrizzlyInd1$aHumanPressure<-GrizzlyInd1$aHumanPressureCls_3+GrizzlyInd1$aHumanPressureCls_4+GrizzlyInd1$aHumanPressureCls_5
#Threat 6.2 Human Intrusion
# 'aSecure' DIVIDE by 3 to get better rep
GrizzlyInd1$aSecure<-GrizzlyInd1$aSecureFull_activeRds/3

#Setup some global variables
GrizzlyInd1$NonHab<-GrizzlyInd1$aTot-GrizzlyInd1$aBEIcls_1-GrizzlyInd1$aBEIcls_2-GrizzlyInd1$aBEIcls_3-GrizzlyInd1$aBEIcls_4-GrizzlyInd1$aBEIcls_5
GrizzlyInd1$AreaHa<-GrizzlyInd1$aTot-GrizzlyInd1$NonHab
GrizzlyInd1<- GrizzlyInd1 %>% mutate(LF = case_when(Landform %in% c('lf4Plain','lf4UshapeValleys') ~ "flat", TRUE ~ "slope"))



#Not sure if I need
#make a GB density map
GBdensity <- fasterize(GBPop, ProvRast, field = 'EST_POP_DENSITY_2018', background=NA) %>%
  crop(StudyArea_sp) %>%
  mask(StudyArea_sp)

#insert density into each strata based on LU density
LUwDensity <- raster::extract(GBdensity, LU, fun = mean, sp=TRUE) %>%
  as('sf') %>%
  dplyr::rename(gbDensity=layer)

#join scenario data to LU map
#sum indicators to LU strata
StratIndFn <- function(dataset, StratIN, IndsIN){
  dataset %>% 
    group_by_(.dots=StratIN) %>%
    summarise_at((.dot=IndsIN), funs(sum))
  #return(dataset)
}

#Set strata variables and pass in indicators to be collapsed and apply strata function
Strata<-c('Year','ScenName','GBPU','LU')
Strata<-c('Year','ScenName')
StrataDF<-data.frame(Strata)
numStrats<-length(Strata)

#Indicators to pass to strata summary
#Indicators<-c('ResComm','Agg','kmPipelines_existing','kmPipelines_proposed','nMines','nWindFarms',
#              'kmTransLine_existing','kmTransLine_potential',
Indicators<-c('ResComm','Agg',
              "aBEIcls_1", "aBEIcls_2", "aBEIcls_3", "aBEIcls_4","aBEIcls_5", "aBEIcls_6", 
              'kmRoad','aRdDensity','aMidSeralConif', 'aConif','aHumanPressure',
              'aSecure','NonHab','AreaHa') #need to include nonhabitat and habitat for calculations
num_indicators<-length(Indicators) 

GB1<-StratIndFn(GrizzlyInd1, Strata, Indicators) 

#Merge densities into scenario file based on LU
#GB2<-data.frame(LU=LUwDensity$LU,gbDensity=LUwDensity$gbDensity) %>%
#  right_join(GB1, by='LU')

GBB<-subset(GB1, ScenName == 'Engineers')# & Year == 2014)
plot(GBB$Year,GBB$aBEIcls_1)

GB2<-right_join(LU,GB1,by.x='LANDSCAP_2',by.y='LU')

GB2<-merge(LU,GB1,by='LU')

