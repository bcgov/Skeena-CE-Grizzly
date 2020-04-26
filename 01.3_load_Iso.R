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

#Read in Grizzly Current Condition and make a version for each scenario for join
GrizzlyIsoCC<-data.frame()
scenNum<-1
for (scenNum in 1:length(Scns)) {
  GrizzlyIsoCCi<-read.csv(file.path(DataDir,'ConnectivityIndGBPU_CurrCond.txt'), header=TRUE, strip.white=TRUE,sep="")
  GrizzlyIsoCCi$Scn<-Scns[scenNum]
  GrizzlyIsoCCi$ScenName<-ScenNames[scenNum]
  GrizzlyIsoCC<-rbind(GrizzlyIsoCC,GrizzlyIsoCCi)
  }  

#Read in Grizzly Scenarios and assign scenario names and set scenario params and add current condition
Scenfilenames<- list.files(path=file.path(DataDir), pattern='ConnectivityIndGBPU_S')
GrizzlyIsoii<-data.frame()
scenNum<-1
for (scenNum in 1:length(Scns)) {
  GrizzlyIsoi<-read.csv(file.path(DataDir,Scenfilenames[scenNum]), header=TRUE, strip.white=TRUE,sep="")
  GrizzlyIsoi$Scn<-Scns[scenNum]
  GrizzlyIsoi$ScenName<-ScenNames[scenNum]
  GrizzlyIsoii<-rbind(GrizzlyIsoii,GrizzlyIsoi)
}  
GrizzlyIso<-rbind(GrizzlyIsoCC,GrizzlyIsoii) %>%
  dplyr::rename(GBPU=gbpuName) %>%
  dplyr::rename(GBPUn=gbpu) %>%
  dplyr::rename(Year=CurrYear)
#Remove GBPUs from SELES files so match AOI
#First fix SELES GBPU names
GrizzlyIso$GBPU<-gsub("UpperSkeena_Nass","Upper Skeena-Nass",GrizzlyIso$GBPU)
GrizzlyIso$GBPU<-gsub("Bulkley_Lakes","Bulkley-Lakes",GrizzlyIso$GBPU)
GrizzlyIso$GBPU<-gsub("NorthCoast","North Coast",GrizzlyIso$GBPU)

GrizzlyIso<-subset(GrizzlyIso, GBPU %in% AOI_GBPUs)
SELES_LUs<-unique(GrizzlyIso$LU)
WriteXLS(GrizzlyIso, file.path(dataOutDir,paste('GrizzlyIso.xls',sep='')))

