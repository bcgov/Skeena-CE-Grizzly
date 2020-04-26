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

#Read in Grizzly Current Condition and make a version 
#for each scenario for join - dont need already included in scenario files
GrizzlyIndCC<-data.frame()
scenNum<-1
for (scenNum in 1:length(Scns)) {
  GrizzlyIndCCi<-read.csv(file.path(DataDir,'GrizzlyInd_CurrCond.txt'), header=TRUE, strip.white=TRUE,sep="")
  GrizzlyIndCCi$Scn<-Scns[scenNum]
  GrizzlyIndCC<-rbind(GrizzlyIndCC,GrizzlyIndCCi)
  }  

#Data Check
#dfCC<-subset(GrizzlyIndCC, Scn=='SSP2')

#Read in Grizzly Scenarios and assign scenario names and set scenario params and add current condition
Scenfilenames<- list.files(path=file.path(DataDir), pattern='GrizzlyInd_SSP')
GrizzlyInd <- do.call("rbind", lapply(file.path(DataDir,Scenfilenames), 
                       read.csv, header=TRUE, strip.white=TRUE,sep="")) %>%
              #rbind(GrizzlyIndCC) %>%
              merge(data.frame(Scn=Scns, 
                      ScenName=c('Re-Wilding','Climate Refugees','Dystopia','1%-ers','Engineers')),
                      by="Scn", all = TRUE)

#Clean up LU and GBPU names for future joining
GrizzlyInd$LU<-gsub("\\s", "", GrizzlyInd$LU)
GrizzlyInd$LU<-gsub("-", "", GrizzlyInd$LU)
GrizzlyInd$LU<-gsub("SkeenIsland","SkeenaIsland",GrizzlyInd$LU)

#Remove GBPUs from SELES files so match AOI
#First fix SELES GBPU names
GrizzlyInd$GBPU<-gsub("UpperSkeena_Nass","Upper Skeena-Nass",GrizzlyInd$GBPU)
GrizzlyInd$GBPU<-gsub("Bulkley_Lakes","Bulkley-Lakes",GrizzlyInd$GBPU)
GrizzlyInd$GBPU<-gsub("NorthCoast","North Coast",GrizzlyInd$GBPU)

GrizzlyInd1<-subset(GrizzlyInd, GBPU %in% AOI_GBPUs)
SELES_LUs<-unique(GrizzlyInd1$LU)

#Load Provincial results by GBPU to get starting condition for
#threats 1 and 2