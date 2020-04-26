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
GrizzlyIso <- data.frame(read_excel(path=file.path(dataOutDir,paste('GrizzlyIso.xls',sep=''))))

#translate connNetRatingExt (0-1) into a letter code
GrizzlyIso <- GrizzlyIso %>%
  mutate(IsoCode=ifelse(connNetRatingExt<0.25 , 'A', 
       ifelse(connNetRatingExt<0.66, 'B',
              ifelse(connNetRatingExt<0.90, 'C', 'D'))))


WriteXLS(GrizzlyIso, file.path(dataOutDir,paste('GrizzlyIso.xls',sep='')))

#
Tiso<-subset(GrizzlyIso, ScenName=='Engineers' & Year==2014)

#Not sure want to do this 
GrizzlyIso$connNetRatingInt<-1-GrizzlyIso$connNetRatingInt
GrizzlyIso$connNetRatingExt<-1-GrizzlyIso$connNetRatingExt

#data check
G_Engineer<-subset(GrizzlyIso, ScenName == 'Engineers' & GBPU=='North Coast')
G_Engineer<-subset(GrizzlyIso, ScenName == 'Engineers' & GBPU=='Bulkley-Lakes')
plot(G_Engineer$Year,G_Engineer$connNetRatingExt)


