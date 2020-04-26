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

#Graphing function
GraphIndFn <- function(dfIN, Ind, yLab, GBPUnum){
  ggplot(data=dfIN, 
         aes_string(x="Year",
                    y=Ind,
                    color="ScenName")) +
    labs(y = yLab,col="Scenario Name",
         title=paste("Skeena Watershed:",AOI_GBPUs[GBPUnum],"GBPU"))+
    geom_line()+
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10))
}
#Table Function
TblIndFn <- function(dfIN, Ind, GBPUnum){
  data.frame(dfIN[,(names(dfIN) %in% TblStrata)], Ind=dfIN[[Ind]]) %>% 
    spread(ScenName, Ind) %>%
    write.table( file = paste(dataOutDir,Ind,"/GBPU_",AOI_GBPUs[GBPUnum],"_Tbl.csv",sep=""), 
                 quote = FALSE, row.names = FALSE, col.names = TRUE, sep=",")
}

#Loop through each Threat and each GBPU - generate a graph and table
AOI_GBPUs<-c('Babine', 'Bulkley_Lakes', 'Cranberry', 'Khutzeymateen','NorthCoast','Stewart','Francois','UpperSkeena_Nass')
num_GBPUs<-length(AOI_GBPUs)
GBnum<-1
TblStrata<-c('Year','ScenName','GBPU')
num_indicators<-length(IndicatorNames)-2 #removing place holders for Habitat and non-habitat

for (Indnum in 1:num_indicators) {
  yvar<-IndicatorNames[Indnum]
  yLabel<-YLabs[Indnum]
  dir.create(file.path(paste(dataOutDir,yvar, sep='')), showWarnings = FALSE)
  
  for (GBnum in 1:num_GBPUs) {
    df<-subset(GB3, (GBPU %in% AOI_GBPUs[GBnum]) & (LF=='flat'))
    
    #plot of Threat
    ggsave(filename=(paste(dataOutDir,yvar,"/GBPU_",AOI_GBPUs[GBnum],"_Graph.pdf",sep="")), 
           plot=GraphIndFn(df,yvar,yLabel,GBnum), width=8, height=10)
    #Table of Threat by GBPU
    TblIndFn(df,yvar,GBnum)
    #Concatenate table of all GBPUs for Threat processing
    ifelse(GBnum == 1, appendcolvar<-FALSE, appendcolvar<-TRUE)
    write.table(df, file = paste(dataOutDir,yvar,"/SkeenaGBPUs_Tbl.csv",sep=""),
                append = appendcolvar, quote = FALSE, row.names = FALSE, col.names = !appendcolvar, sep=",")
  }
}


