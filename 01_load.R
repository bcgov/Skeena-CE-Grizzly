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

#Read in Grizzly Scenarios and assign scenario names and set scenario params
Scenfilenames<- list.files(path=file.path(DataDir), pattern='GrizzlyInd_SSP')
GrizzlyInd1 <- do.call("rbind", lapply(file.path(DataDir,Scenfilenames), read.csv, header=TRUE, strip.white=TRUE,sep=""))
GrizzlyInd1 <- merge (GrizzlyInd1, data.frame(Scn=c('SSP1','SSP2','SSP3','SSP4','SSP5'), 
                                              ScenName=c('Re-Wilding','Climate Refugees','Dystopia','1%-ers','Engineers')),
                      by="Scn", all = TRUE)
