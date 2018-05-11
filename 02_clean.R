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
#Some indicators get summarized (areas) others get passed throught to strata for final calculation (proportions)
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
#Threat 4 - Transportation & Service Corridors
# 'kmRoad'
#Threat 5.1 - Biological Resource Use
GrizzlyInd1$aRdDensity<-GrizzlyInd1$aRdDensityCls_4+GrizzlyInd1$aRdDensityCls_5
#Threat 5.3
# 'aMidSeralConif', 'aConif'
#Threat 6.1 Human Intrusion
GrizzlyInd1$aHumanPressure<-GrizzlyInd1$aHumanPressureCls_3+GrizzlyInd1$aHumanPressureCls_4+GrizzlyInd1$aHumanPressureCls_5
#Threat 6.2 Human Intrusion
# 'aSecure'
GrizzlyInd1$aSecure<-GrizzlyInd1$aSecureFull

#Indicators to pass to strata summary
Indicators<-c('ResComm','Agg','kmPipelines_existing','kmPipelines_proposed','nMines','nWindFarms','kmTransLine_existing','kmTransLine_potential','kmRoad','aRdDensity','aMidSeralConif', 'aConif','aHumanPressure','aSecure','NonHab','AreaHa') #need to include nonhabitat and habitat for calculations
num_indicators<-length(Indicators) 

#Setup some global variables
GrizzlyInd1$NonHab<-GrizzlyInd1$aTot-GrizzlyInd1$aBEIcls_1-GrizzlyInd1$aBEIcls_2-GrizzlyInd1$aBEIcls_3-GrizzlyInd1$aBEIcls_4-GrizzlyInd1$aBEIcls_5
GrizzlyInd1$AreaHa<-GrizzlyInd1$aTot-GrizzlyInd1$NonHab
GrizzlyInd1<- GrizzlyInd1 %>% mutate(LF = case_when(Landform %in% c('lf4Plain','lf4UshapeValleys') ~ "flat", TRUE ~ "slope"))


#Function for collapsing indicators to strata
#Collapse to strata and summarize, using reporting function
StratIndFn <- function(dataset, StratIN, IndsIN){
  dataset %>% 
    group_by_(.dots=StratIN) %>%
    summarise_at((.dot=IndsIN), funs(sum))
  #return(dataset)
}

#Set strata variables and pass in indicators to be collapsed and apply strata function
Strata<-c('Year','ScenName','GBPU','LF')
StrataDF<-data.frame(Strata)
numStrats<-4
GB1<-StratIndFn(GrizzlyInd1, Strata, Indicators)

#Assign indicator names and calcs for proportions
GB1$T1<-GB1$ResComm
GB1$T2<-GB1$Agg
GB1$T31<-round(GB1$kmPipelines_existing/(GB1$kmPipelines_existing+GB1$kmPipelines_proposed)*100,2) 
GB1$T31<-ifelse(is.nan(GB1$T31), 0, GB1$T31)
GB1$T32<-GB1$nMines
GB1$T33<-round(GB1$kmTransLine_existing/(GB1$kmTransLine_existing+GB1$kmTransLine_potential)*100,2)
GB1$T33<-ifelse(is.nan(GB1$T33), 0, GB1$T33)
GB1$T4<-GB1$kmRoad #needs changing
GB1$T51<-GB1$aRdDensity
GB1$T53<-round(GB1$aMidSeralConif/GB1$aConif*100,2)
GB1$T61<-GB1$aHumanPressure
GB1$T62<-GB1$aSecure

IndicatorNames<-c('T1','T2','T31','T32','T33','T4','T51','T53','T61','T62','NonHab','AreaHa')
YLabs<-c('T1-% Residential on Plains/U-Shaped Valleys','T2-% Agriculture & Range on Plains/U-Shaped Valleys','T3.1-% Pipelines possible in Plains/U-Shaped Valleys','T3.2-# all types of mines','T3.3-% Renewables possible','T4-% Roads possible in Plains/U-Shaped Valleys','T5.1-Road Density','T5.3-Mid Seral','T6.1-Human Pressure','T6.2-Core Security Area')

#Calculate %indicator area of total strata area for certain indicators
IndsPCArea<-c('T1','T2','T51','T61','T62','NonHab','AreaHa')
#Pass through count and proportion indicators
IndsCntsProps<-c('T31','T32','T33','T4','T53')

GBlist<-GB1[ , (names(GB1) %in% IndsPCArea)]
GB2<-data.frame(lapply(GBlist, function(x) round(x/GBlist$AreaHa*100,2)))
GB3<-cbind(data.frame(GB1[ , (names(GB1) %in% Strata)], GB2, GB1[ , (names(GB1) %in% IndsCntsProps)]))

