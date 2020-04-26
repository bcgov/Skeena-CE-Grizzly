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

#
# Reads ThreatBench.xls from 03.4_analysis_ID_Thresholds.R and brings in GBThreatsI.xls
# Generates Threats_O.xls which has asssinged threat level for each GBPU
#

source("header.R")

#######
# Using numerically derived benchmarks re-calculate the Threats for each GBPU
#Modify Bench - all thresholds the same expect climate change which is a 0, 1, 2, or 3
#and Energy which runs from 0 to 41 units so used quartiles
ThreatBench <- data.frame(read_excel(path=file.path(dataOutDir,paste('ThreatBench.xls',sep='')))) 

ThreatI <- data.frame(read_excel(path=file.path(dataOutDir,paste('GBThreatsI.xls',sep=''))))

ThreatAssignFn <- function(DF, ThreatVal, ThreatCalc) {
  DF %>% 
    mutate(ReCalc = (DF[[ThreatVal]] >= ThreatCalc))
}

nThreatLevels<-3 #only Low, Medium, High
ThreatLevelsNames<-c('Low','Medium', 'High', 'VHigh')
nTvars<-length(ThreatAVars)# number of threats

j<-1
ThreatLevels<-list()
for (j in 1:nThreatLevels) {
  ThreatReCalc<-list()
  i<-1
  for (i in 1:nTvars) {
    ThreatValue<-paste(ThreatAVars[i],sep='')#ThreatVal
    ThreatCalculated<-ThreatBench[ThreatBench$Threat==ThreatValue,(j+1)]#ThreatCalc move to different column for each threat level
    ThreatReCalc[[i]] <- ThreatAssignFn(ThreatI, ThreatValue, ThreatCalculated)$ReCalc
  }  
  #ThreatLevels[[j]]<-ThreatReCalc
  #}
  
  # cast the ThreatReCalc list to a data.frame
  DFin<-data.frame(do.call(cbind, ThreatReCalc))
  cols <- sapply(DFin, is.logical)
  DFin[,cols] <- lapply(DFin[,cols], as.numeric)
  
  # set NA to 0
  DFin[is.na(DFin)] <- 0
  #bind GBPU names to data.frame and assign threat name to column
  Threat_DFF <- cbind(data.frame(ThreatI$ScenName, ThreatI$Year,ThreatI$GBPU), DFin)
  colnames(Threat_DFF)<-c('ScenName','Year','GBPU',paste(ThreatAVars,'_calc',sep=''))
  #colnames(Threat_DFF)<-c('GBPU',paste(ThreatAVars,'_calcL',sep=''))
  
  #Build Threat data base using calculated values
  #for multiple threat category threats set if any one of them is flagged
  Threat_1<-
    Threat_DFF %>%
    mutate(Threat_2 = ifelse((Agriculture_2.3b_calc) > 0, 1, 0)) %>%
    mutate(Threat_5 = ifelse((BioUse_5.1a_calc + BioUse_5.1b_calc + BioUse_5.3_calc)>0, 1, 0)) %>%
    mutate(Threat_1 = ifelse((Residential_1b_calc)>0, 1, 0)) %>%
    #dplyr::rename(Threat_1 = Residential_1_calc) %>%
    dplyr::rename(Threat_3 = Energy_3all_calc) %>%
    #dplyr::rename(Threat_4 = Transport_4all_calc) %>% # includes seismic lines
    dplyr::rename(Threat_4 = Transport_4.1_calc) %>%
    dplyr::rename(Threat_6 = HumanIntrusion_6_calc) %>%
    dplyr::rename(Threat_11 = ClimateChange_11_calc) %>%
    dplyr::select(ScenName, Year, GBPU, starts_with('Threat'))
  
  Threat_1$numT<-rowSums(Threat_1[4:10])
  
  ThreatLevels[[j]]<-Threat_1
}
names(ThreatLevels) <- ThreatLevelsNames[1:nThreatLevels]

#Generate a threat table showing threat by GBPU
ThreatAdd<-ThreatLevels[['Low']][4:10]+ThreatLevels[['Medium']][4:10]+ThreatLevels[['High']][4:10]

ThreatSummaryL <- ThreatAdd %>% 
  mutate(ScenName = ThreatLevels$Low$ScenName) %>%
  mutate(Year = ThreatLevels$Low$Year) %>%
  mutate(GBPU = ThreatLevels$Low$GBPU) %>%
  mutate(ResidentialCalc = ifelse(Threat_1==0, 'Negligible', 
                                  ifelse(Threat_1==1, 'Low',
                                         ifelse(Threat_1==2, 'Medium',
                                                ifelse(Threat_1==3, 'High',
                                                       'Unknown'))))) %>%
  mutate(AgricultureCalc = ifelse(Threat_2==0, 'Negligible', 
                                  ifelse(Threat_2==1, 'Low',
                                         ifelse(Threat_2==2, 'Medium',
                                                ifelse(Threat_2==3, 'High',
                                                       'Unknown'))))) %>%
  mutate(EnergyCalc = ifelse(Threat_3==0, 'Negligible', 
                             ifelse(Threat_3==1, 'Low',
                                    ifelse(Threat_3==2, 'Medium',
                                           ifelse(Threat_3==3, 'High',
                                                  'Unknown'))))) %>%
  mutate(TransportationCalc = ifelse(Threat_4==0, 'Negligible', 
                                     ifelse(Threat_4==1, 'Low',
                                            ifelse(Threat_4==2, 'Medium',
                                                   ifelse(Threat_4==3, 'High',
                                                          'Unknown'))))) %>%
  mutate(BioUseCalc = ifelse(Threat_5==0, 'Negligible', 
                             ifelse(Threat_5==1, 'Low',
                                    ifelse(Threat_5==2, 'Medium',
                                           ifelse(Threat_5==3, 'High',
                                                  'Unknown'))))) %>%
  mutate(HumanIntrusionCalc = ifelse(Threat_6==0, 'Negligible', 
                                     ifelse(Threat_6==1, 'Low',
                                            ifelse(Threat_6==2, 'Medium',
                                                   ifelse(Threat_6==3, 'High',
                                                          'Unknown'))))) %>%
  mutate(ClimateChangeCalc = ifelse(Threat_11==0, 'Negligible', 
                                    ifelse(Threat_11==1, 'Low',
                                           ifelse(Threat_11==2, 'Medium',
                                                  ifelse(Threat_11==3, 'High',
                                                         'Unknown'))))) %>%
  dplyr::select(ScenName, Year, GBPU, ends_with('Calc'))

WriteXLS(ThreatSummaryL, file.path(dataOutDir,paste('ThreatCalcSummary.xls',sep='')))

# Adjust lower threat levels to 0 if registering a higher level threat so calculator works and  
# recalculate the number of occurences of that threat level for each GBPU
# Low
ThreatLevels[[1]]<-data.frame(ScenName=ThreatLevels[[1]][1], Year=ThreatLevels[[1]][2], GBPU=ThreatLevels[[1]][3], 
                              (abs(ThreatLevels[[2]][4:10]-1)*ThreatLevels[[1]][4:10]))#,numT=rowSums(ThreatLevels[[1]][2:8]))

# Med
ThreatLevels[[2]]<-data.frame(ScenName=ThreatLevels[[2]][1], Year=ThreatLevels[[2]][2], GBPU=ThreatLevels[[2]][3], 
                              (abs(ThreatLevels[[3]][4:10]-1)*ThreatLevels[[2]][4:10]))#,numT=rowSums(ThreatLevels[[2]][2:8]))

# High
ThreatLevels[[3]]<-data.frame(ScenName=ThreatLevels[[3]][1], Year=ThreatLevels[[3]][2], GBPU=ThreatLevels[[3]][3], 
                              (ThreatLevels[[3]][4:10]))#,numT=rowSums(ThreatLevels[[3]][2:8]))

ThreatLevels[[1]]$numT=rowSums(ThreatLevels[[1]][4:10])
ThreatLevels[[2]]$numT=rowSums(ThreatLevels[[2]][4:10])
ThreatLevels[[3]]$numT=rowSums(ThreatLevels[[3]][4:10])

#Write out ThreatLevels to inspect
WriteXLS(ThreatLevels, file.path(dataOutDir,paste('GBThreatLevels.xls',sep='')),SheetNames=names(ThreatLevels))

#Overall threat level assignment
#Threat_O<- data.frame(GBPU_Name=ThreatLevels[[Low]]$GBPU_Name, NumLowT=ThreatLevels[[Low]]$numT, NumLowM=ThreatLevels[['Medium']]$numT)
#Generate overall threat class data base based on number of threat classes
Threat_O<-data.frame(matrix(0,ncol=0,nrow=nrow(ThreatLevels[[1]])))
for (j in 1:nThreatLevels) {
  Threat_O<-cbind(Threat_O, data.frame(Num=ThreatLevels[[j]]$numT))
}
#temp fix for medium threat from Threat_5 - data base error
#Threat_O[,2]<-0
#Threat_O[55,2]<-1 #for the Yahk

#append 0s for threats not included so logic is clean
BlankT<-data.frame(matrix(0,ncol=(4-nThreatLevels),nrow=nrow(Threat_O)))
Threat_O<-cbind(ScenName=ThreatLevels[[1]]$ScenName,Year=ThreatLevels[[1]]$Year,
                GBPU=ThreatLevels[[1]]$GBPU, Threat_O, BlankT)
colnames(Threat_O)<-c('ScenName','Year','GBPU',ThreatLevelsNames)

#Cacluate overall threat

Threat_LUT<-data.frame(Threat_Class = c('Negligible','Low','Medium','High','VHigh'),
                       #ThreatAdj = c(0,0,-1,-1,-2))
                        ThreatAdj = c(0,0,-1,-1.5,-2))# High category bumped to -1.5 see Proctor email Oct 10 2018

Threat_O <- Threat_O %>%
  mutate(Threat_Class =
           case_when(
             Low>0 & Low<4 & Medium==0 & High==0 ~ 'Low',
             (Low>3 & Medium==0 & High==0) | (Low<=3 & Medium==1 & High==0 ) ~ 'Medium',
             (Low>=0 & Medium<2 & High==1 ) | (Low>=0 & Medium>2 & High==0) | (Low>=2 & Medium==2 & High==0) |
             (Low>2 & Medium==1 & High==0)  ~ 'High',
             (Low>=0 & Medium>=0 & High>1) | (Low>=0 & Medium>1 & High==1) ~ 'VHigh',
             Low==0 & Medium==0 & High==0 ~ 'Negligible'
           )) %>%
  left_join(Threat_LUT, by='Threat_Class')


WriteXLS(Threat_O, file.path(dataOutDir,paste('Threat_O.xls',sep='')))
