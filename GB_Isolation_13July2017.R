#Summarizes SELES GBPU Isolation indicators
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

#Set environment variables
RunDate<-"04July2017"
#BVRC
CompDir<-"/Users/donmorgan/Dropbox (BVRC)"
#LapTop
CompDir<-"/Users/Morgan/Dropbox (BVRC)"

fileInDir <- paste(CompDir,"/Library/ModelOutputs/GrizzlyBears/",RunDate,"/", sep='')
fileOutDirS<-paste(CompDir, "/SkeenaSalmon/SkeenaCE/Assessment/GBears/", sep='')

dir.create(file.path(paste(fileOutDirS,RunDate,"/", sep='')), showWarnings = FALSE)
fileOutDir<-paste(fileOutDirS,RunDate,"/", sep='')
R_Code<-paste(fileOutDirS,"/R_Code/", sep='')
setwd(fileOutDir)

#Read in Grizzly Scenarios and assign scenario names and set scenario params
#ConnectivityIndGBPU_SSP1
#ConnectivityIndLU_SSP1

GBPUScenfilenames<- list.files(path=paste(fileInDir,sep=''), pattern='ConnectivityIndGBPU')
#LUScenfilenames<- list.files(path=paste(fileInDir,sep=''), pattern='ConnectivityIndLU')

ScenN=c('Re-Wilding','Climate Refugees','Dystopia','1%-ers','Engineers')
nScen<-5
scenNum<-1

GBPU1<-read.csv(paste(fileInDir,GBPUScenfilenames[scenNum],sep=''), header=TRUE, strip.white=TRUE,sep="") %>%
  data.frame(Scn=paste('SSP',scenNum,sep=''), ScenName=ScenN[scenNum])

for (scenNum in 2:nScen) {
  GBPU1<-rbind(GBPU1, (read.csv(paste(fileInDir,GBPUScenfilenames[scenNum],sep=''), header=TRUE, strip.white=TRUE,sep="") %>% data.frame(Scn=paste('SSP',scenNum,sep=''),ScenName=ScenN[scenNum])))
}
GBPU1$connNetRatingInt<-1-GBPU1$connNetRatingInt
GBPU1$connNetRatingExt<-1-GBPU1$connNetRatingExt

#which indicators are being considered?
Indicators<- c('CurrYear','ScenName','gbpuName','connNetRatingInt','connNetRatingExt')
num_indicators<-2
Strata<-c('CurrYear','ScenName','gbpuName')

IndicatorNames<-c('connNetRatingInt','connNetRatingExt')
YLabs<-c('Internal Connectivity','Exeternal Connectivity')

#Graphing function
GraphIndFn <- function(dfIN, Ind, yLab, GBPUnum){
  ggplot(data=dfIN, 
         aes_string(x="CurrYear",
                    y=Ind,
                    color="ScenName")) +
    labs(y = yLab,col="Scenario Name",
         title=paste("Skeena Watershed:",AOI_GBPUs[GBPUnum],"GBPU"))+
    geom_line()+
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10))
}

#subset for each scenario, each year - start with one case - generate a graph and table
AOI_GBPUs<-c('Babine', 'Bulkley_Lakes', 'Cranberry', 'Khutzeymateen','NorthCoast','Stewart','Francois','UpperSkeena_Nass')
num_GBPUs<-length(AOI_GBPUs)
GBnum<-1
num_indicators<-2

for (Indnum in 1:num_indicators) {
  yvar<-IndicatorNames[Indnum]
  yLabel<-YLabs[Indnum]
  dir.create(file.path(paste(fileOutDir,yvar, sep='')), showWarnings = FALSE)
  
  for (GBnum in 1:num_GBPUs) {
    df<-subset(GBPU1, (gbpuName %in% AOI_GBPUs[GBnum]))
    
    #plot of Threat
    ggsave(filename=(paste(fileOutDir,yvar,"/GBPU_",AOI_GBPUs[GBnum],"_Graph.pdf",sep="")), 
           plot=GraphIndFn(df,yvar,yLabel,GBnum), width=8, height=10)
  }
}


