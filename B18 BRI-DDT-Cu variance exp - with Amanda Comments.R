library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)


#location on amanda's computer 
all<-read.csv("C:/Users/Amanda/Desktop/CSUF/MATH 539 - SCCWRP/Revisit - BRI-Cu-DDT.csv", stringsAsFactors = F)
head(all)
View(all)
dim(all)
#setwd("L:/Bight 18/B18 Survey Design/Revisit Study/Data")
#all<-read.csv("Revisit - BRI-Cu-DDT.csv", stringsAsFactors = F)


################################################################
##################### Temporal Variability #####################
################################################################

######################### BRI SCORE ############################

bri<-unique(all[, c(1,2,7)]) #only unique values for Station, Year, and Score
head(bri)
dim(bri)

#ifelse (test, yes, no)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear) #turns years 98 and 03 into 9803 for the first sample data
##98 and 03 data is NOT kept separately and the score from both are combined into one "data set" 
head(bri)


#ddply - applys function to variable or list of variables and turns it into a data frame
#mean, sd, var of Score by Station
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score)) 
head(bri_summary)

#changes the years to column names
bri2<-spread(bri,key = SampleYear,value = BRI_Score)  
head(bri2)

#added columns for the difference between first sample and second sample, 
## second sample and third sample, and first sample and third sample - in absolute value 
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
head(bri2)

#summary of data as well as differences in one data frame 
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
head(bri3)
dim(bri3)

#only one row omitted
bri3.2<-na.omit(bri3)
dim(bri3.2) 

#summary of the data without the station
bri3.2<-bri3.2[,c(2:7)]
head(bri3.2)

#takes the average of the summary of data - avg of mean, sd, var, and the differences from sample year
bri_summary2<-summarise_all(bri3.2, list(avg=mean)) #changed funs to list for update
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2

#calculates maximum average change and adds it to summary average
bri_summary2$maxDelta_avg = max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)

#final BRI summary includes average of summaries -> average of mean, sd, var, and the max average change 
bri_summary3<-bri_summary2[,c(1:3,7)]
bri_summary3

######################### DDT Result ############################

DDT<-unique(all[,c(1,2,3,5)])
DDT<-na.omit(DDT)
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)
DDT<-filter(DDT,Parameter=="Total_DDT")
DDT_summary<-ddply(DDT, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))
DDT2<-spread(DDT,key =SampleYear,value = MaxOfResult)
DDT2<-mutate(DDT2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2[,c(1,6:8)], by=c("OStationID"))
DDT3.2<-DDT3[,c(2:7)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7)]



######################### Copper Result ############################

Cu<-unique(all[,c(1,2,3,5)])
Cu<-na.omit(Cu)
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu<-filter(Cu,Parameter=="Copper")
Cu_summary<-ddply(Cu, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))
Cu2<-spread(Cu,key =SampleYear,value = MaxOfResult)
Cu2<-mutate(Cu2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2[,c(1,6:8)], by=c("OStationID"))
Cu3.2<-Cu3[,c(2:7)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7)]


######################### Final Result ##########################

TemporalVar<- rbind(Cu_summary2, DDT_summary2, bri_summary2)
TemporalVar

################################################################
###################### Spatial Variablity ######################
################################################################

###################### Spatial BRI ######################

Sbri<-unique(all[, c(1,2,7)]) #only unique values for Station, Year, and BRI Score
head(Sbri)
dim(Sbri)

#ddply - applys function to variable or list of variables and turns it into a data frame
#mean, sd, var, min, and max of Score by Sample Year
#omiting NAs
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                   avg=mean(BRI_Score,na.rm=TRUE),
                   stdev=sd(BRI_Score,na.rm=TRUE),
                   variance=var(BRI_Score,na.rm=TRUE),
                   minimum=min(BRI_Score,na.rm=TRUE),
                   maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary

#difference between the maximum and minimum for BRI 
Sbri_summary$maxDelta<-Sbri_summary$maximum-Sbri_summary$minimum 

Sbri2<-Sbri_summary[,c(2:7)] #removes year column

#takes the average of the summary of data - avg of mean, sd, var, and the differences from sample year
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2


###################### Spatial DDT ######################

SDDT<-unique(all[, c(1,2,7)])
SDDT<-filter(DDT,Parameter=="Total_DDT")
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT2<-SDDT_summary[,c(2:7)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"


###################### Spatial Copper ######################

SCu<-unique(all[, c(1,2,7)])
SCu<-filter(Cu,Parameter=="Copper")
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu2<-SCu_summary[,c(2:7)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"

###################### Final Spatial ########################

SpatialVar<- rbind(SCu_summary2, SDDT_summary2, Sbri_summary2)
SpatialVar


################ Final Temporal vs Spatial ####################

TotalVar<- rbind(SpatialVar[,c(1:3,6)], TemporalVar[,c(1:3,7)])
TotalVar


