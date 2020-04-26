library(sf)
library(rgdal)
library(dplyr)
library(plyr)
library(readr)
library(raster)
library(bcmaps)
library(fasterize)
library(tidyr)
library(rio)
library(WriteXLS)
library(ggplot2)
library(readxl)

RunDate<-"25July2017"

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
StrataOutDir <- file.path(dataOutDir,'Strata')
tileOutDir <- file.path(dataOutDir,'tile')
figsOutDir <- file.path(OutDir,'figures')
SpatialDir <- file.path('data','spatial')
DataDir <- file.path('data',RunDate)
spatialOutDir <- file.path(OutDir,'spatial')
BearsCEDir <- file.path('../GB_Data/data/BearsCE')
BearsRankDir <- file.path('../grizzly-bear-IUCN-threats/out/data')


GBspatialDir <- file.path('../GB_Data/out/spatial')
GBdataOutDir <- file.path('../GB_Data/out/data')
#RasterGBdir<- ("/Users/Morgan/Dropbox (BVRC)/Library/ModelOutputs/baseGrids")
GBPDir <-file.path('../GB_Data/data/Population/Bear_Density_2018')

dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(StrataOutDir), showWarnings = FALSE)
dir.create(file.path(tileOutDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)



