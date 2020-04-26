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

#Clean up some of the indicators so allign with IUCN analysis
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
Indicators<-c('ResComm','Agg',
              'nMines','kmRoad','aRdDensity','aMidSeralConif', 'aConif','aHumanPressure',
              'aSecure','NonHab','AreaHa','aTot') #need to include nonhabitat and habitat for calculations

numStrats<-length(Strata)
Threat1<-StratIndFn(GrizzlyInd1, Strata, Indicators)

#Generate a LUT to modify scenario indicators
RankResults$Residential_1b<-RankResults$Residential_1b+0.00001 #Add fraction since Skeena-Nass Zero
Threat_LUT<-subset(Threat1, Year==2014 & ScenName=='Re-Wilding') %>%
     left_join(RankResults, by='GBPU') %>%
     mutate(ResCommRatio=Residential_1b/ResComm) %>% #Human density indexed to Urban/Industry area
     mutate(AggRatio=Agriculture_2.3b/Agg) %>% #Ag Range livestock density indexed to Ag Range area
     mutate(PopRatio=PopnEst2018/aSecure) %>% 
     mutate(Bio51bRatio=BioUse_5.1b/Residential_1b) %>% #Hunters indexed to human populatin size
     dplyr::select(Year, ScenName, GBPU, PopnEst2018,
                   Residential_1b,ResCommRatio, Agriculture_2.3b,AggRatio, PopRatio,PopnEst2018,
                   BioUse_5.1a,Bio51bRatio)

Threat2<-Threat1 %>%
  left_join(Threat_LUT, by='GBPU')

Scenario_LUT <- data.frame(Scns=c('SSP1','SSP2','SSP3','SSP4','SSP5'),
ScenNames=c('Re-Wilding','Climate Refugees','Dystopia','1%-ers','Engineers'),
Roads=c(0,999,999,10,20), # 0 immediate closure, 999 never close, 10/20 close after 10 or 20 years
Hunt=c(0,0,1,1,1), #0 no hunt, 1 hunt
Hunters=c(0,1,1,0,1), #0 stable at current, 1 increase with population 
CC=c(0,1,2,1,1))

#Cast data into ThreatI format and value types for ranking
#Could rework so function do a call by GBPU or Scenario
ThreatI<-Threat2 %>%
  dplyr::rename(Year=Year.x) %>%
  dplyr::rename(ScenName=ScenName.x) %>%
  mutate(Residential_1b=ResComm*ResCommRatio) %>%
  mutate(Agriculture_2.3b=Agg*AggRatio) %>%
  mutate(Energy_3all=nMines) %>%
  mutate(Transport_4.1=aRdDensity/AreaHa) %>%
  #GBPU specific - modify
  mutate(Popn=aSecure*PopRatio) %>% 
  mutate(BioUse_5.1a=Popn/PopnEst2018*BioUse_5.1a) %>% #modify mortality based on population size
  mutate(BioUse_5.1b=Residential_1b*Bio51bRatio) %>%
  mutate(BioUse_5.3=aMidSeralConif/aConif*100) %>%
  #should be modified to only AreaHa -once indicator adjusted.
  mutate(HumanIntrusion_6=aHumanPressure/aTot*100) %>%
  mutate(ClimateChange_11=if_else(ScenName == 'Re-Wilding',0,
                            if_else(ScenName=='Climate Refugees'|ScenName=='1%-ers'|
                                    ScenName=='Engineers',1,2))) %>%
  dplyr::select(ScenName,Year,GBPU_Name=GBPU,Residential_1b,Agriculture_2.3b,Energy_3all,
                Transport_4.1,aSecure,BioUse_5.1a,BioUse_5.1b,BioUse_5.3,HumanIntrusion_6,ClimateChange_11)

WriteXLS(ThreatI, file.path(dataOutDir,paste('GBThreatsI.xls',sep='')))

