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

AOI_GBPUs<-c('Babine', 'Bulkley-Lakes', 'Cranberry', 'Khutzeymateen','North Coast','Stewart','Francois','Upper Skeena-Nass')
num_GBPUs<-length(AOI_GBPUs)
Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

Scns=c('SSP1','SSP2','SSP3','SSP4','SSP5')
ScenNames=c('Re-Wilding','Climate Refugees','Dystopia','1%-ers','Engineers')

#Population Data
GBPopn<- data.frame(read_xls(path=file.path(GBdataOutDir,'GBPUpop.xls'))) %>%
  mutate(GBPU=POPULATION_NAME) %>%
  dplyr::filter(GBPU %in% AOI_GBPUs)
  
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                 ymn=173787.5, ymx=1748187.5,
                 crs=Prov_crs,
                 res = c(100,100), vals = 0)

#Read in the 2018 population data and filter to GBPUs being considered
GBPop_gdb <- list.files(file.path(GBPDir), pattern = ".gdb", full.names = TRUE)[1]
fc_list <- st_layers(GBPop_gdb)
#GBPop <- read_sf(GBPop_gdb, layer = "GBPU_MU_LEH_2015_2018_bear_density_DRAFT") - throws geometry errors
GBPop <- readOGR(dsn=GBPop_gdb, layer = "GBPU_MU_LEH_2015_2018_bear_density_DRAFT") %>%
  as('sf') %>%
  dplyr::filter(POPULATION_NAME %in% AOI_GBPUs)
st_crs(GBPop)=3005

GBPop_data<- GBPop
st_geometry(GBPop_data) <- NULL


RankResults<-read.csv(file.path(BearsRankDir,'GrizzlyBear_2019_ConservationRanking_Results.csv'),
                      header=TRUE, strip.white=TRUE,sep=",") %>%
            dplyr::filter(GBPU %in% AOI_GBPUs)

#make a clipping polygon
StudyArea<-st_union(GBPop)
StudyArea_sp<-st_union(GBPop) %>%
  as('Spatial')

#Read in BEI and clip
BEIr<-raster(file.path(GBspatialDir,"BEIr.tif")) %>%
  crop(StudyArea_sp) %>%
  mask(StudyArea_sp)
crs(BEIr) <- CRS('+init=EPSG:3005')

#Read in LU raster
LUr <-raster(file.path(SpatialDir,"lu.tif")) %>%
  crop(StudyArea_sp) %>%
  mask(StudyArea_sp)
crs(LUr) <- CRS('+init=EPSG:3005')

#Read in LU poly
LUi <-read_sf(file.path(SpatialDir,'Landscape_units/Landscape_unit.shp')) %>%
  mutate(LU=LANDSCAP_2)
  st_crs(LUi)<-3005
LU1<-dplyr::filter(st_intersection(LUi,StudyArea))
#Clean up LU names for future joining
LU1$LU<-gsub("\\s", "", LU1$LU)
LU1$LU<-gsub("-", "", LU1$LU)
LU<-subset(LU1, LU %in% SELES_LUs)
LU_LUs<-LU$LU

SELES_LUs  %in%  LU_LUs
LU_LUs  %in%  SELES_LUs
setdiff(SELES_LUs,LU_LUs)
setdiff(LU_LUs,SELES_LUs)
intersect(SELES_LUs,LU_LUs)

#Data check
#st_write(LU, file.path(spatialOutDir,'LU.shp'), delete_layer = TRUE)

############

NonHab<-raster(file.path(StrataDir,"NonHab.tif"))
GBPUr<-raster(file.path(StrataDir,"GBPUr.tif"))
WMUr<-raster(file.path(StrataDir,"WMUr.tif"))
WMUr_NonHab<-raster(file.path(StrataDir,"WMUr_NonHab.tif"))
GBPUr_NonHab<-raster(file.path(StrataDir,"GBPUr_NonHab.tif"))
GBPUr_BEI_1_2<-raster(file.path(StrataDir,"GBPUr_BEI_1_2.tif"))
GBPUr_BEI_1_5<-raster(file.path(StrataDir,"GBPUr_BEI_1_5.tif"))
GBPUr_LFormFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlat.tif"))
GBPUr_LFormFlatFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlatFlat.tif"))
GBPUr_Forest<-raster(file.path(StrataDir,"GBPUr_Forest.tif"))



#Population Data
GBPop<- data.frame(read_xls(path=file.path(GBdataOutDir,'GBPUpop.xls'))) %>%
  mutate(GBPU_Name=POPULATION_NAME) %>%
  mutate(PopnEst2018=pop2018)

WriteXLS(GB_GBPUdf, file.path(dataOutDir,paste('gb2018GBPUpop.xls',sep='')))


#Extract WMU/LEH/NPark data
st_geometry(GBPop)<-NULL
WMUpop<-GBPop %>%
  dplyr::select(Region=REGION_RESPONSIBLE_NAME, GBPU_MU_LEH_uniqueID,MU,LEH_Zone2_fix,
                MAX_ALLOW_MORT_PERC,
                GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME,STATUS,EST_POP_DENSITY_2018,
                EST_POP_2018,EST_POP_2015, AREA_KM2, AREA_KM2_noWaterIce) %>%
  mutate(EST_POP_DENSITY_2018=round(EST_POP_DENSITY_2018,2))

WriteXLS(WMUpop, file.path(dataOutDir,paste('WMUpop.xls',sep='')))

WMUtest <-WMUpop %>%
  dplyr::select(GBPU_MU_LEH_uniqueID,POPULATION_NAME,EST_POP_2018,EST_POP_DENSITY_2018,AREA_KM2,
                AREA_KM2_noWaterIce) %>%
  mutate(densityCalc = round(EST_POP_2018/AREA_KM2*1000,2)) %>%
  mutate(densityCalc_noWaterIce = round(EST_POP_2018/AREA_KM2_noWaterIce*1000,2))

#Extract GBPU scale population data
GBPUpop<-WMUpop %>%
  dplyr::group_by(GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME) %>%
  dplyr::summarise(pop2018 = sum(EST_POP_2018), pop2015=sum(EST_POP_2015),
                   Status=first(STATUS),Area_km2=sum(AREA_KM2),
                   Area_km2_noWaterIce=sum(AREA_KM2_noWaterIce)) %>%
  mutate(Density = round(pop2018/Area_km2*1000,2)) %>%
  mutate(Density_noWaterIce = round(pop2018/Area_km2_noWaterIce*1000,2))

WriteXLS(GBPUpop, file.path(dataOutDir,paste('GBPUpop.xls',sep='')))

