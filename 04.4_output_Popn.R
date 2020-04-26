#Summarizes SELES GBPU Isolation indicators
#By Don Morgan, MoE, 250.877.3199

#Summarize provincial LUs and GBPUs for bear density vs amount core security - 
#generate relationship and use to determine future bear populations with shifts in security

source("header.R")

#test data
#tt<-subset(GBbxs1, ScenName == 'Dystopia' & GBPU=='Babine')
#plot(tt$Year,tt$popEstT)

#which indicators are being considered?
Indicators<- c('CurrYear','ScenName','GBPU','popEstT')
Strata<-c('CurrYear','ScenName','GBPU')

IndicatorNames<-c('popEstT')
YLabs<-c('Population Estimate')

#By GBPU
#Graphing function
GraphIndFn <- function(dfIN, Ind, yLab, GBPUnum){
  ggplot(data=dfIN, 
         aes_string(x="Year",
                    y=Ind,
                    color="ScenName")) +
    labs(y = yLab,col="Scenario Name",
         title=paste("Skeena Watershed:",AOI_GBPUs[GBPUnum],"GBPU"))+
    geom_line()+
    #scale_y_continuous(expand = c(0, 0))+
    #scale_y_continuous(breaks=scales::pretty_breaks(n=10),expand = c(0, 0))+
    expand_limits(x = 2020, y = 20)+
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10))
}

#subset for each scenario, each year - start with one case - generate a graph and table
GBnum<-1
num_indicators<-length(IndicatorNames)

for (Indnum in 1:num_indicators) {
  yvar<-IndicatorNames[Indnum]
  yLabel<-YLabs[Indnum]
  dir.create(file.path(figsOutDir,yvar), showWarnings = FALSE)
  
  for (GBnum in 1:num_GBPUs) {
    df<-subset(GBbxs1, (GBPU %in% AOI_GBPUs[GBnum]))
    
    #plot of isolation
    ggsave(filename=(file.path(figsOutDir,yvar,paste("GBPU_",AOI_GBPUs[GBnum],"_Graph.pdf",sep=''))), 
                  plot=GraphIndFn(df,yvar,yLabel,GBnum), width=8, height=10)
  }
}

#By Scenario
#Graphing function
GraphIndFn2 <- function(dfIN, Ind, yLab, Scnum){
  ggplot(data=dfIN, 
         aes_string(x="Year",
                    y=Ind,
                    color="GBPU")) +
    labs(y = yLab,col="GBPU Name",
         title=paste("Skeena Watershed:",ScenNames[Scnum],"Scenario"))+
    expand_limits(x = 2010, y = 0)+
    geom_line()+
    scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
    scale_y_continuous(breaks=scales::pretty_breaks(n=10))
}

#subset for each scenario, each year - start with one case - generate a graph and table

ScenNum<-1
num_indicators<-length(IndicatorNames)
numScens<-length(ScenNames)

for (Indnum in 1:num_indicators) {
  yvar<-IndicatorNames[Indnum]
  yLabel<-YLabs[Indnum]
  #dir.create(file.path(paste(figsOutDir,yvar, sep='')), showWarnings = FALSE)
  
  for (ScenNum in 1:numScens) {
    df<-subset(GBbxs1, (ScenName %in% ScenNames[ScenNum]))
    
    #plot of isolation
    ggsave(filename=(file.path(figsOutDir,yvar,paste("Scn_",Scns[ScenNum],"_Graph.pdf",sep=''))), 
           plot=GraphIndFn2(df,yvar,yLabel,ScenNum), width=8, height=10)
  }
}

#Data check
#tdf<-subset(data.frame(CurrYear=GrizzlyIso$CurrYear, GBPU=GrizzlyIso$GBPU, connNetRatingExt=GrizzlyIso$connNetRatingExt,ScenName=GrizzlyIso$ScenName), CurrYear==2014)
