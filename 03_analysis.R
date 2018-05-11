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


###


#fileOutDirS - dataOutDir
#Move all to DataDir
GISdir <- "/Users/donmorgan/Dropbox (BVRC)/Values/GBearsProv/GBAtlas/GISData/"
localGISdir <- paste("/Users/donmorgan/Dropbox (BVRC)/Values/GBearsProv/GBAtlas/ReportingAreas/",AOI,"/Data/", sep='')
RasterGBdir<- ("/Users/donmorgan/Dropbox (BVRC)/Library/ModelOutputs/baseGrids/")

#DataDir
fileInDir <- paste("/Users/Morgan/Dropbox (BVRC)/Library/ModelOutputs/GrizzlyBears/",RunDate,"/", sep='')
#dataOurDir
fileOutDirS<-"/Users/Morgan/Dropbox (BVRC)/SkeenaSalmon/SkeenaCE/Assessment/GBears/"


#Inputs for each area, use only main GBPUs that intersect
AOI<-'SkeenaWshd'
#based on results of GB_AOI_Overlap script determine which GBPUs to use for summarizing - not used here, just listed
AOI_GBPUs<-c('Babine', 'Bulkley-Lakes', 'Cranberry', 'Khutzeymateen','North Coast','Stewart','Francois','Upper Skeena-Nass')
num_GBPUs<-8

#Indiators selected for summarzing
Indicators<-c('aRdDensityCls_1','aSecureFull','aMidSeralConif','aHumanPressureCls_1','aHumanPressureCls_2','aLU_NaturalPermanent','aLU_Rural','aLU_Natural','aLU_Urban','aLU_Industrial','aTot')
IndicatorNames<-c('RoadDensity','CoreSercurityAreas')
IndicatorList<-c(0,0) #set indicator to 0
num_indicators<-2

#Strata
Strata<-c('Year','Scn','GBPU','LU','Landform')
StrataDF<-data.frame(Strata)
num_Strata<-5


nScen<-5
scenNum<-1
#GrizzlyInd1<-data.frame(read.csv(header=TRUE, file=paste(DataDir, "/GrizzlyInd_SSP",scenNum,".txt", sep=""), sep="", strip.white=TRUE, ))

