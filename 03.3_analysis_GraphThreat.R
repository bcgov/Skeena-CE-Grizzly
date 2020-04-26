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


#Clean up some of the indicators so allign with IUCN work up
#Secure seems OK, but roads are off?
source("header.R")


#Function for collapsing indicators to strata
#Collapse to strata and summarize, using reporting function
StratIndFn <- function(dataset, StratIN, IndsIN){
  dataset %>% 
    group_by_(.dots=StratIN) %>%
    summarise_at((.dot=IndsIN), funs(sum))
  #return(dataset)
}

#Set strata variables and pass in indicators to be collapsed and apply strata function
#Strata<-c('Year','ScenName','GBPU','LF')
Strata<-c('Year','ScenName','GBPU')
StrataDF<-data.frame(Strata)
numStrats<-length(Strata)
GB1<-StratIndFn(GrizzlyInd1, Strata, Indicators)

#some exploratory code
GBB<-subset(GrizzlyInd1, ScenName == 'Engineers' & GBPU == 'Babine')
plot(GBB$Year, GBB$aRdDensity)
GBB$aSecure/GBB$AreaHa*100

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

####
df<-subset(GB3, (GBPU %in% AOI_GBPUs[1]) & (ScenName %in% 'Engineers') )


