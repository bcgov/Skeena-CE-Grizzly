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

Strata<-c('Year','ScenName','GBPU')
#StrataDF<-data.frame(Strata)
numStrats<-length(Strata)
#Indicators to pass to strata summary
#Indicators<-c('ResComm','Agg','kmPipelines_existing','kmPipelines_proposed','nMines','nWindFarms',
#              'kmTransLine_existing','kmTransLine_potential',
#Indicators<-c("aBEIcls_1", "aBEIcls_2", "aBEIcls_3", "aBEIcls_4","aBEIcls_5", "aBEIcls_6", 
Indicators<-c('aSecure','NonHab','AreaHa') #need to include nonhabitat and habitat for calculations
#Indicators<-c('aSecure','NonHab','AreaHa','aSecureFull_activeRds','aSecurePartial_activeRds',
#              'aSecureNonHab_activeRds','aNonSecureHab_activeRds','aNonSecureNonHab_activeRds',
#              'aSecureFull_allRds','aSecurePartial_allRds','aSecureNonHab_allRds',
#              'aNonSecureHab_allRds','aNonSecureNonHab_allRds')
#Indicators<-c('aSecure','NonHab','AreaHa','kmRoad','kmRoad_open','kmRoad_maxFut')
num_indicators<-length(Indicators) 

#Data Check
#df2014<-subset(GrizzlyInd1, ScenName=='Engineers' & Year==2014)
#df2024<-subset(GrizzlyInd1, ScenName=='Engineers' & Year==2024)

StratIndFn <- function(dataset, StratIN, IndsIN){
  dataset %>% 
    group_by_(.dots=StratIN) %>%
    summarise_at((.dot=IndsIN), funs(sum))
  #return(dataset)
}

GBbxs<-StratIndFn(GrizzlyInd1, Strata, Indicators) %>%
  left_join(GBPopn, by='GBPU')

aSecurePop_LUT <- GBbxs %>%
  dplyr::filter(ScenName == 'Engineers' & Year == 2014) %>%
  mutate(aSecurePopRatio = pop2018/aSecure) %>%
  dplyr::select(GBPU,aSecurePopRatio) 

GBbxs1 <- GBbxs %>%
  left_join(aSecurePop_LUT, by='GBPU') %>%
  mutate(popEstT = aSecurePopRatio*aSecure) %>%
  dplyr::select(Year=Year.x, ScenName=ScenName.x, GBPU, aSecure,aSecurePopRatio,popEstT) %>%
  group_by(ScenName, GBPU) %>%
  group_modify(~ {
    .x %>%
      mutate(Ycomp=
               ifelse(Year>2034, Year-30,
                      ifelse(Year==2034, Year-20,
                             ifelse(Year==2024, Year-10,
                                    2014))))
    })

#df1 <- subset(GBbxs1, ScenName=='Engineers' & GBPU=='Babine')


#Make a LUT to pull population out for calculating trend
Trend_LUT<-GBbxs1 %>%
  dplyr::select(ScenName, GBPU, Year, popEstT) 

PopTrend<- Trend_LUT %>% 
 right_join(GBbxs1, by=c('ScenName','GBPU','Year' = 'Ycomp')) %>%
 dplyr::select(ScenName=ScenName, GBPU=GBPU, Year=Year.y, popEstT=popEstT.y, Pcomp=popEstT.x) %>%
  mutate(Trend=(1-popEstT/Pcomp)*100)
  

WriteXLS(PopTrend,file.path(dataOutDir,paste('PopTrend.xls',sep='')))


#Data Check
#GBB<-subset(GBbxs,  GBPU=='Babine' & ScenName == 'Dystopia')
#plot(GBB$Year,GBB$kmRoad/GBB$kmRoad_maxFut*100)  

#pull out a single scenario record since pop2018 and aSecure are all the same for year 2014
#GBB<-subset(GBbxs,  ScenName == 'Engineers' & Year == 2014) %>%
#  mutate(aSecurePopRatio = pop2018/aSecure) %>%
#  dplyr::select(GBPU,aSecurePopRatio,aSecure,pop2018) 

#Data check - secure drops in first period??? across all scenarios
#GBbxs1 <- GBbxs %>%
#  left_join(aSecurePop_LUT, by='GBPU') %>%
#  mutate(popEstT = aSecurePopRatio*aSecure)

#GBBB<-subset(GBbxs1,  ScenName == 'Engineers' & GBPU=='Babine')

#Merge densities into scenario file based on LU
#GB2<-data.frame(LU=LUwDensity$LU,gbDensity=LUwDensity$gbDensity) %>%
#  right_join(GB1, by='LU')

#GBB<-subset(GBbxs,  ScenName == 'Engineers' & Year == 2014) %>%
#  mutate(aSecurePopRatio == pop2018/aSecure)

#plot(GBB$aSecure,GBB$aBEIcls_1)


