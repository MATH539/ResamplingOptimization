library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#data location on Amanda's computer
all<-read.csv("C:/Users/Amanda/Desktop/CSUF/MATH 539 - SCCWRP/Revisit - moreparameters_allstrata.csv",
              stringsAsFactors = F)
levels(as.factor(all$Parameter))

#setwd("L:/Bight 18/B18 Survey Design/Revisit Study/Data")
#all<-read.csv("Revisit - moreparameters_allstrata.csv", stringsAsFactors = F)

#Separating the individual Strata
innershelf<-subset(all[all$Stratum == "Inner Shelf",])
midshelf<-subset(all[all$Stratum == "Mid Shelf",])
outershelf<-subset(all[all$Stratum == "Outer Shelf",])
lowerslope<-subset(all[all$Stratum == "Lower Slope",])
upperslope<-subset(all[all$Stratum == "Upper Slope",])
ports<-subset(all[all$Stratum == "Ports",])
marinas<-subset(all[all$Stratum == "Marinas",])
estuaries<-subset(all[all$Stratum == "Estuaries",])
bays<-subset(all[all$Stratum == "Bays",])
islands<-subset(all[all$Stratum =="Channel Islands",])


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         BRI          ###############
names(innershelf)

#unique values only with stationID, Year, BRI Score
bri<-unique(innershelf[, c(1,2,7)]) 
#omit NA
bri<-na.omit(bri) 
#collecting 03, 98 data into single variable
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear) 


#takes BRI InnerShelf data and gets mean, sd, var and puts into data frame  
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score)) 

#makes sample year into columns with each row a stationID
bri2<-spread(bri,key = SampleYear,value = BRI_Score)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
bri2<-mutate(bri2, d12 = abs(`9803`-`2008`), 
                   d23 = abs(`2008`-`2013`), 
                   d13 = abs(`9803`-`2013`)) 

#summary data and differences in year by station
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))


#coefficient of variation - allows us to compare variance when data has different means
bri3<-mutate(bri3,cv=((stdev/avg)*100)) 
 

bri3.2<-na.omit(bri3) #omit NA

bri3.2<-bri3.2[,c(2:8)] #does not have identifier (station ID)

#takes the average of all summarized data thus far
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame 
bri_summary2$maxDelta_avg = max(bri_summary2$d12_avg, 
                                bri_summary2$d23_avg, 
                                bri_summary2$d13_avg)

bri_summary3<-bri_summary2[,c(1:3,7,8)] #summary data and CV

#adding the following columns to data frame 
bri_summary3$type="time"
bri_summary3$parameter="BRI"
bri_summary3$strata="inner shelf"

#final summary 
bri_summary3


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         DDT          ###############        

#subsetting to only look at the innershelf with TOTAL DDT
DDT<-subset(innershelf[innershelf$Parameter=="Total_DDT",c(1,2,3,5)])
#collecting 03, 98 data into single variable
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0

#takes DDT InnerShelf data and gets mean, sd, var and puts into data frame
DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#changes data frame to years in columns by stationID
DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
DDT2.1<-mutate(DDT2.1, d12=abs(`9803`-`2008`), 
                       d23=abs(`2008`-`2013`),
                       d13=abs(`9803`-`2013`))

#summary data and differences in year by station
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
DDT3.2<-DDT3[,c(2:8)]

#takes the average of all summarized data thus far
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var:InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg,
                               DDT_summary2$d23_avg, 
                               DDT_summary2$d13_avg)

#summary data and CV
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="inner shelf"

#final summary
DDT_summary3

###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############        Copper        ###############

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE COPPER!!!!
#KAREN.Q
#Cu<-subset(innershelf[innershelf$Parameter=="Zinc",c(1,2,3,5)])

#subsetting to only look at the innershelf with Copper
Cu<-subset(innershelf[innershelf$Parameter=="Copper",c(1,2,3,5)])

#collecting 03, 98 data into single variable
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0

#takes Copper InnerShelf data and gets mean, sd, var and puts into data frame
Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
Cu2.1<-spread(Cu2,key = SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), 
                    d23=abs(`2008`-`2013`), 
                    d13=abs(`9803`-`2013`))

#summary data and differences in year by station
Cu3<-merge(Cu_summary ,Cu2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
Cu3.2<-Cu3[,c(2:8)]

#takes the average of all summarized data thus far
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, 
                              Cu_summary2$d23_avg, 
                              Cu_summary2$d13_avg)

#summary data and CV
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="inner shelf"

#final summary
Cu_summary3


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         TOC          ###############

#subsetting to only look at the innershelf with TOC
TOC<-subset(innershelf[innershelf$Parameter=="TOC",c(1,2,3,5)])

#collecting 03, 98 data into single variable
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q
TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0

#takes TOC InnerShelf data and gets mean, sd, var and puts into data frame
TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
TOC2.1<-mutate(TOC2.1, d12=abs(`9803`-`2008`), 
                       d23=abs(`2008`-`2013`), 
                       d13=abs(`9803`-`2013`))

#summary data and differences in year by station
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
TOC3.2<-TOC3[,c(2:8)]

#takes the average of all summarized data thus far
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, 
                               TOC_summary2$d23_avg, 
                               TOC_summary2$d13_avg)

#summary data and CV
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="inner shelf"

#final summary
TOC_summary3

###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############          TN          ###############

#subsetting to only look at the innershelf with TN
TN<-subset(innershelf[innershelf$Parameter=="TN",c(1,2,3,5)])

#collecting 03, 98 data into single variable
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0

#takes TN InnerShelf data and gets mean, sd, var and puts into data frame
TN_summary<-ddply(TN2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), 
                    d23=abs(`2008`-`2013`), 
                    d13=abs(`9803`-`2013`))

#summary data and differences in year by station
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
TN3<-mutate(TN3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
TN3.2<-TN3[,c(2:8)]

#takes the average of all summarized data thus far
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, 
                              TN_summary2$d23_avg, 
                              TN_summary2$d13_avg)

#summary data and CV
TN_summary3<-TN_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="inner shelf"

#final summary
TN_summary3


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         Zinc         ###############

#subsetting to only look at the innershelf with Zinc
zinc<-subset(innershelf[innershelf$Parameter=="Zinc",c(1,2,3,5)])

#collecting 03, 98 data into single variable
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0

#takes Zinc Result InnerShelf data and gets mean, sd, var and puts into data frame
zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
zinc2.1<-spread(zinc2, key = SampleYear, value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), 
                        d23=abs(`2008`-`2013`), 
                        d13=abs(`9803`-`2013`))

#summary data and differences in year by station
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
zinc3.2<-zinc3[,c(2:8)]

#takes the average of all summarized data thus fa
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var:InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, 
                                zinc_summary2$d23_avg, 
                                zinc_summary2$d13_avg)

#summary data and CV
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="inner shelf"

#final summary
zinc_summary3


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         PAH          ###############

#subsetting to only look at the innershelf with Total PAH
PAH<-subset(innershelf[innershelf$Parameter=="Total_PAH",c(1,2,3,5)])

#collecting 03, 98 data into single variable
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0

#takes Total PAH InnerShelf data and gets mean, sd, var and puts into data frame
PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
PAH2.1<-mutate(PAH2.1, 
               d12=abs(`9803`-`2008`), 
               d23=abs(`2008`-`2013`), 
               d13=abs(`9803`-`2013`))

#summary data and differences in year by station
PAH3<-merge(PAH_summary,PAH2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
PAH3.2<-PAH3[,c(2:8)]

#takes the average of all summarized data thus far
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"

#maximum of the differences averaged between sample year and added to summary data frame
PAH_summary2$maxDelta_avg= max(PAH_summary2$d12_avg, 
                               PAH_summary2$d23_avg, 
                               PAH_summary2$d13_avg)

#summary data and CV
PAH_summary3<-PAH_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="inner shelf"

#final summary
PAH_summary3


###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         PCB          ###############


#subsetting to only look at the innershelf with Total PCB
PCB<-subset(innershelf[innershelf$Parameter=="Total_PCB",c(1,2,3,5)])

#collecting 03, 98 data into single variable
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0

#takes PCB InnerShelf data and gets mean, sd, var and puts into data frame
PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
PCB2.1<-mutate(PCB2.1, d12=abs(`9803`-`2008`), 
                       d23=abs(`2008`-`2013`), 
                       d13=abs(`9803`-`2013`))

#summary data and differences in year by station
PCB3<-merge(PCB_summary,PCB2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
PCB3.2<-PCB3[,c(2:8)]

#takes the average of all summarized data thus far
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
PCB_summary2$maxDelta_avg= max(PCB_summary2$d12_avg, 
                               PCB_summary2$d23_avg, 
                               PCB_summary2$d13_avg)

#summary data and CV
PCB_summary3<-PCB_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="inner shelf"

#final summary
PCB_summary3

###############      INNER SHELF     ###############
############### Temporal Variability ###############
###############         Fines        ###############

#subsetting to only look at the innershelf with Percent Fines
fines<-subset(innershelf[innershelf$Parameter=="PercentFines",c(1,2,3,5)])

#collecting 03, 98 data into single variable
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

#turning empty values into NA and then turning NA into zero's - Karen.q 
fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


#takes Fines InnerShelf data and gets mean, sd, var and puts into data frame
fines_summary<-ddply(fines2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

#makes sample year into columns with each row a stationID
fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)

#Adds to DF difference in absolute value from sample year to sample year (1-2, 2-3, 1-3)
fines2.1<-mutate(fines2.1, d12=abs(`9803`-`2008`), 
                           d23=abs(`2008`-`2013`), 
                           d13=abs(`9803`-`2013`))

#summary data and differences in year by station
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))

#coefficient of variation - allows us to compare variance when data has different means 
fines3<-mutate(fines3,cv=((stdev/avg)*100))

#does not have identifier (station ID)
fines3.2<-fines3[,c(2:8)]

#takes the average of all summarized data thus far
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var: InShelf"

#maximum of the differences averaged between sample year and added to summary data frame
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)

#summary data and CV
fines_summary3<-fines_summary2[,c(1:3,7,8)]

#adding the following columns to data frame
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="inner shelf"

#final summary
fines_summary3

##########################################################################################
#################### Inner Shelf Temporal Summary - Each Parameter #######################
##########################################################################################

ISTemporalVar<- rbind(fines_summary3,
                      PCB_summary3, 
                      PAH_summary3, 
                      zinc_summary3,
                      TN_summary3,
                      TOC_summary3, 
                      Cu_summary3, 
                      DDT_summary3, 
                      bri_summary3)

##########################################################################################
##########################################################################################


###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         BRI          ###############

#unique values only with stationID, Year,  BRI Score
Sbri<-unique(innershelf[, c(1,2,7)])

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))

#difference in max and min
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
Sbri2<-Sbri_summary[,c(2:8)]

#takes the average of all summarized data thus far
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var: InShelf"

#adding the following columns to data frame
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="inner shelf"

#added to compare easily to temporal 
Sbri_summary3<-Sbri_summary2[,c(1:3,7,6,8:10)]

#final summarys
Sbri_summary3
Sbri_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         DDT          ###############

#subsetting to only look at the innershelf with Total DDT
SDDT<-subset(innershelf[innershelf$Parameter=="Total_DDT",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
SDDT[is.na(SDDT)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different mean
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
SDDT2<-SDDT_summary[,c(2:8)]

#takes the average of all summarized data thus far
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var:InShelf"

#adding the following columns to data frame
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="inner shelf"

#added to compare easily to temporal 
SDDT_summary3<-SDDT_summary2[,c(1:3,7,6,8:10)]

#final summarys
SDDT_summary3
SDDT_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         Copper       ###############

#subsetting to only look at the innershelf with Copper
SCu<-subset(innershelf[innershelf$Parameter=="Copper",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
SCu[is.na(SCu)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
SCu2<-SCu_summary[,c(2:8)]

#takes the average of all summarized data thus far
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var:InShelf"

#adding the following columns to data frame
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="inner shelf"

#added to compare easily to temporal 
SCu_summary3<-SCu_summary2[,c(1:3,7,6,8:10)]

#final summarys
SCu_summary3
SCu_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         TOC          ###############

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE COPPER!!!!
#KAREN.Q
#STOC<-subset(innershelf[innershelf$Parameter=="Total_DDT",c(1,2,3,5)])

#subsetting to only look at the innershelf with TOC
STOC<-subset(innershelf[innershelf$Parameter=="TOC",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
STOC[is.na(STOC)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
STOC2<-STOC_summary[,c(2:8)]

#takes the average of all summarized data thus far
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var: InShelf"

#adding the following columns to data frame
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="inner shelf"

#added to compare easily to temporal 
STOC_summary3<-STOC_summary2[,c(1:3,7,6,8:10)]

#final summarys
STOC_summary3
STOC_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         TN           ###############

#subsetting to only look at the innershelf with TN
STN<-subset(innershelf[innershelf$Parameter=="TN",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
STN[is.na(STN)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
STN_summary<-ddply(STN, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
STN2<-STN_summary[,c(2:8)]

#takes the average of all summarized data thus far
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var:InShelf"

#adding the following columns to data frame
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="inner shelf"

#added to compare easily to temporal 
STN_summary3<-STN_summary2[,c(1:3,7,6,8:10)]

#final summarys
STN_summary3
STN_summary2


###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         Zinc          ###############


#subsetting to only look at the innershelf with Zinc
SZinc<-subset(innershelf[innershelf$Parameter=="Zinc",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
SZinc[is.na(SZinc)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
SZinc2<-SZinc_summary[,c(2:8)]

#takes the average of all summarized data thus far
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var:InShelf"

#adding the following columns to data frame
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="inner shelf"

#added to compare easily to temporal 
SZinc_summary3<-SZinc_summary2[,c(1:3,7,6,8:10)]

#final summarys
SZinc_summary3
SZinc_summary2


###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         PAH          ###############

#subsetting to only look at the innershelf with Total PAH
SPAH<-subset(innershelf[innershelf$Parameter=="Total_PAH",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
SPAH[is.na(SPAH)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
SPAH2<-SPAH_summary[,c(2:8)]

#takes the average of all summarized data thus far
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var:InShelf"

#adding the following columns to data frame
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="inner shelf"


#added to compare easily to temporal 
SPAH_summary3<-SPAH_summary2[,c(1:3,7,6,8:10)]

#final summarys
SPAH_summary3
SPAH_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############         PCB          ###############

#subsetting to only look at the innershelf with Total PCB
SPCB<-subset(innershelf[innershelf$Parameter=="Total_PCB",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
SPCB[is.na(SPCB)]<-0


#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
SPCB2<-SPCB_summary[,c(2:8)]

#takes the average of all summarized data thus far
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var:InShelf"

#adding the following columns to data frame
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="inner shelf"


#added to compare easily to temporal 
SPCB_summary3<-SPCB_summary2[,c(1:3,7,6,8:10)]

#final summarys
SPCB_summary3
SPCB_summary2

###############      INNER SHELF     ###############
###############  Spatial Variability ###############
###############        Fines         ###############

#subsetting to only look at the innershelf with Percent Fines
Sfines<-subset(innershelf[innershelf$Parameter=="PercentFines",c(1,2,3,5)])

#turns NA into 0 - no need to complete data as missing points arent needed for spatial analys
Sfines[is.na(Sfines)]<-0

#Summary data calculated and turned into data frame - omit NA data included
#max, min, mean, sd, var
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))

#difference in max and min
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 

#coefficient of variation - allows us to compare variance when data has different means 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))

#does not have identifier (station ID)
Sfines2<-Sfines_summary[,c(2:8)]

#takes the average of all summarized data thus far
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var:InShelf"

#adding the following columns to data frame
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="inner shelf"

#added to compare easily to temporal 
Sfines_summary3<-Sfines_summary2[,c(1:3,7,6,8:10)]

#final summarys
Sfines_summary3
Sfines_summary2


##########################################################################################
################### Inner Shelf Temporal Summary - Each Parameter ########################
##########################################################################################

ISSpatialVar<- rbind(Sfines_summary2,
                     SPCB_summary2, 
                     SPAH_summary2, 
                     SZinc_summary2, 
                     STN_summary2, 
                     STOC_summary2, 
                     SCu_summary2,
                     SDDT_summary2, 
                     Sbri_summary2)


#added by me since didnt notice last bit does it anyways
ISSpatialVar3<- rbind(Sfines_summary3,
                     SPCB_summary3, 
                     SPAH_summary3, 
                     SZinc_summary3, 
                     STN_summary3, 
                     STOC_summary3, 
                     SCu_summary3,
                     SDDT_summary3, 
                     Sbri_summary3)


##########################################################################################
######################### Inner Shelf - Temporal vs Spatial ##############################
##########################################################################################

#same as if we used the variable I created instead
ISTotalVar<- rbind(ISSpatialVar[,c(1:3,7,6,8,9,10)], ISTemporalVar)

ISTotalVar
  
########################################################################################## 
########################################################################################## 
############### Note: Only added comments for inner shelf, the same process ##############
###############     is applied to all stratas separateyly so it would be    ############## 
###############     redundant to repeat this commentary for each strata     ############## 
########################################################################################## 
########################################################################################## 

#MID SHELF
##Temporal Variability

bri<-unique(midshelf[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="mid shelf"


DDT<-subset(midshelf[midshelf$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="mid shelf"


Cu<-subset(midshelf[midshelf$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="mid shelf"

TOC<-subset(midshelf[midshelf$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="mid shelf"


TN<-subset(midshelf[midshelf$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="mid shelf"


zinc<-subset(midshelf[midshelf$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="mid shelf"


PAH<-subset(midshelf[midshelf$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,6:8)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:8)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= max(PAH_summary2$d12_avg, PAH_summary2$d23_avg, PAH_summary2$d13_avg)
PAH_summary3<-PAH_summary2[,c(1:3,7,8)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="mid shelf"


PCB<-subset(midshelf[midshelf$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,6:8)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:8)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= max(PCB_summary2$d12_avg, PCB_summary2$d23_avg, PCB_summary2$d13_avg)
PCB_summary3<-PCB_summary2[,c(1:3,7,8)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="mid shelf"


fines<-subset(midshelf[midshelf$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="mid shelf"




MSTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(midshelf[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                   avg=mean(BRI_Score,na.rm=TRUE),
                   stdev=sd(BRI_Score,na.rm=TRUE),
                   variance=var(BRI_Score,na.rm=TRUE),
                   minimum= min(BRI_Score,na.rm=TRUE),
                   maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="mid shelf"


SDDT<-subset(midshelf[midshelf$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="mid shelf"


SCu<-subset(midshelf[midshelf$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="mid shelf"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(midshelf[midshelf$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(midshelf[midshelf$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="mid shelf"

STN<-subset(midshelf[midshelf$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="mid shelf"


SZinc<-subset(midshelf[midshelf$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="mid shelf"


SPAH<-subset(midshelf[midshelf$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="mid shelf"


SPCB<-subset(midshelf[midshelf$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="mid shelf"



Sfines<-subset(midshelf[midshelf$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="mid shelf"



MSSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

MSTotalVar<- rbind(MSSpatialVar[,c(1:3,7,6,8,9,10)], MSTemporalVar)



#OUTER SHELF
##Temporal Variability

bri<-unique(outershelf[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="outer shelf"


DDT<-subset(outershelf[outershelf$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="outer shelf"


Cu<-subset(outershelf[outershelf$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="outer shelf"

TOC<-subset(outershelf[outershelf$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="outer shelf"


TN<-subset(outershelf[outershelf$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="outer shelf"


zinc<-subset(outershelf[outershelf$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="outer shelf"


PAH<-subset(outershelf[outershelf$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1, d23=abs(`2008`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,5)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:6)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= PAH_summary2$d23_avg
PAH_summary3<-PAH_summary2[,c(1:3,5,6)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="outer shelf"


PCB<-subset(outershelf[outershelf$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1, d23=abs(`2008`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,5)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:6)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= PCB_summary2$d23_avg
PCB_summary3<-PCB_summary2[,c(1:3,5,6)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="outer shelf"


fines<-subset(outershelf[outershelf$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="outer shelf"




OSTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)




##Spatial Variablity
Sbri<-unique(outershelf[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="outer shelf"


SDDT<-subset(outershelf[outershelf$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="outer shelf"


SCu<-subset(outershelf[outershelf$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="outer shelf"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(outershelf[outershelf$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(outershelf[outershelf$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="outer shelf"

STN<-subset(outershelf[outershelf$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="outer shelf"


SZinc<-subset(outershelf[outershelf$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="outer shelf"


SPAH<-subset(outershelf[outershelf$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="outer shelf"


SPCB<-subset(outershelf[outershelf$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="outer shelf"



Sfines<-subset(outershelf[outershelf$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="outer shelf"



OSSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

OSTotalVar<- rbind(OSSpatialVar[,c(1:3,7,6,8,9,10)], OSTemporalVar)


#Lower Slope
##Temporal Variability

##No BRI on Lower Slope

DDT<-subset(lowerslope[lowerslope$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="lower slope"

Cu<-subset(lowerslope[lowerslope$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="lower slope"

TOC<-subset(lowerslope[lowerslope$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="lower slope"


TN<-subset(lowerslope[lowerslope$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="lower slope"


zinc<-subset(lowerslope[lowerslope$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="lower slope"


PAH<-subset(lowerslope[lowerslope$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1, d23=abs(`2008`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,5)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:6)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= PAH_summary2$d23_avg
PAH_summary3<-PAH_summary2[,c(1:3,5,6)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="lower slope"


PCB<-subset(lowerslope[lowerslope$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1, d23=abs(`2008`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,5)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:6)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= PCB_summary2$d23_avg
PCB_summary3<-PCB_summary2[,c(1:3,5,6)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="lower slope"


fines<-subset(lowerslope[lowerslope$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="lower slope"



LSTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity

#NO BRI on Lower Slope


SDDT<-subset(lowerslope[lowerslope$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="lower slope"


SCu<-subset(lowerslope[lowerslope$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="lower slope"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(lowerslope[lowerslope$Parameter=="Total_DDT",c(1,2,3,5)])
STOC<-subset(lowerslope[lowerslope$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="lower slope"

STN<-subset(lowerslope[lowerslope$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="lower slope"


SZinc<-subset(lowerslope[lowerslope$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="lower slope"


SPAH<-subset(lowerslope[lowerslope$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="lower slope"


SPCB<-subset(lowerslope[lowerslope$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="lower slope"



Sfines<-subset(lowerslope[lowerslope$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="lower slope"



LSSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

LSTotalVar<- rbind(LSSpatialVar[,c(1:3,7,6,8,9,10)], LSTemporalVar)



#UPPER SLOPE
##Temporal Variability

bri<-unique(upperslope[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="upper slope"


DDT<-subset(upperslope[upperslope$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="upper slope"


Cu<-subset(upperslope[upperslope$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="upper slope"

TOC<-subset(upperslope[upperslope$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="upper slope"


TN<-subset(upperslope[upperslope$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="upper slope"


zinc<-subset(upperslope[upperslope$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="upper slope"


PAH<-subset(upperslope[upperslope$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1, d23=abs(`2008`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,5)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:6)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= PAH_summary2$d23_avg
PAH_summary3<-PAH_summary2[,c(1:3,5,6)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="upper slope"


PCB<-subset(upperslope[upperslope$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1, d23=abs(`2008`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,5)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:6)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= PCB_summary2$d23_avg
PCB_summary3<-PCB_summary2[,c(1:3,5,6)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="upper slope"


fines<-subset(upperslope[upperslope$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="upper slope"



USTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity

Sbri<-unique(upperslope[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="upper slope"


SDDT<-subset(upperslope[upperslope$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="upper slope"


SCu<-subset(upperslope[upperslope$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="upper slope"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(upperslope[upperslope$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(upperslope[upperslope$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="upper slope"

STN<-subset(upperslope[upperslope$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="upper slope"


SZinc<-subset(upperslope[upperslope$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="upper slope"


SPAH<-subset(upperslope[upperslope$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="upper slope"


SPCB<-subset(upperslope[upperslope$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="upper slope"



Sfines<-subset(upperslope[upperslope$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="upper slope"



USSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

USTotalVar<- rbind(USSpatialVar[,c(1:3,7,6,8,9,10)], USTemporalVar)



#PORTS
##Temporal Variability

bri<-unique(ports[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="ports"


DDT<-subset(ports[ports$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="ports"


Cu<-subset(ports[ports$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,5)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:6)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= Cu_summary2$d13_avg
Cu_summary3<-Cu_summary2[,c(1:3,5,6)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="ports"

TOC<-subset(ports[ports$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="ports"


TN<-subset(ports[ports$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,5)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:6)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= TN_summary2$d13_avg
TN_summary3<-TN_summary2[,c(1:3,5,6)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="ports"


zinc<-subset(ports[ports$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,5)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:6)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= zinc_summary2$d13_avg
zinc_summary3<-zinc_summary2[,c(1:3,5,6)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="ports"


PAH<-subset(ports[ports$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,6:8)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:8)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= max(PAH_summary2$d12_avg, PAH_summary2$d23_avg, PAH_summary2$d13_avg)
PAH_summary3<-PAH_summary2[,c(1:3,7,8)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="ports"


PCB<-subset(ports[ports$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,6:8)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:8)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= max(PCB_summary2$d12_avg, PCB_summary2$d23_avg, PCB_summary2$d13_avg)
PCB_summary3<-PCB_summary2[,c(1:3,7,8)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="ports"


fines<-subset(ports[ports$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="ports"



PTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(ports[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="ports"


SDDT<-subset(ports[ports$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="ports"


SCu<-subset(ports[ports$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="ports"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(ports[ports$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(ports[ports$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="ports"

STN<-subset(ports[ports$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="ports"


SZinc<-subset(ports[ports$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="ports"


SPAH<-subset(ports[ports$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="ports"


SPCB<-subset(ports[ports$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="ports"



Sfines<-subset(ports[ports$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="ports"



PSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

PTotalVar<- rbind(PSpatialVar[,c(1:3,7,6,8,9,10)], PTemporalVar)



#Marinas
##Temporal Variability

bri<-unique(marinas[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="marinas"


DDT<-subset(marinas[marinas$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="marinas"


Cu<-subset(marinas[marinas$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="marinas"

TOC<-subset(marinas[marinas$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="marinas"


TN<-subset(marinas[marinas$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="marinas"


zinc<-subset(marinas[marinas$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="marinas"


PAH<-subset(marinas[marinas$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,6:8)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:8)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= max(PAH_summary2$d12_avg, PAH_summary2$d23_avg, PAH_summary2$d13_avg)
PAH_summary3<-PAH_summary2[,c(1:3,7,8)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="marinas"


PCB<-subset(marinas[marinas$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,6:8)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:8)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= max(PCB_summary2$d12_avg, PCB_summary2$d23_avg, PCB_summary2$d13_avg)
PCB_summary3<-PCB_summary2[,c(1:3,7,8)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="marinas"


fines<-subset(marinas[marinas$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="marinas"



MTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(marinas[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="marinas"


SDDT<-subset(marinas[marinas$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="marinas"


SCu<-subset(marinas[marinas$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="marinas"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(marinas[marinas$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(marinas[marinas$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="marinas"

STN<-subset(marinas[marinas$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="marinas"


SZinc<-subset(marinas[marinas$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="marinas"


SPAH<-subset(marinas[marinas$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="marinas"


SPCB<-subset(marinas[marinas$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="marinas"



Sfines<-subset(marinas[marinas$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="marinas"



MSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)

MTotalVar<- rbind(MSpatialVar[,c(1:3,7,6,8,9,10)], MTemporalVar)



#Estuaries
##Temporal Variability

bri<-unique(estuaries[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="estuaries"


DDT<-subset(estuaries[estuaries$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="estuaries"


Cu<-subset(estuaries[estuaries$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="estuaries"

TOC<-subset(estuaries[estuaries$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="estuaries"


TN<-subset(estuaries[estuaries$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="estuaries"


zinc<-subset(estuaries[estuaries$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="estuaries"


PAH<-subset(estuaries[estuaries$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1, d23=abs(`2008`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,5)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:6)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= PAH_summary2$d23_avg
PAH_summary3<-PAH_summary2[,c(1:3,5,6)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="estuaries"


PCB<-subset(estuaries[estuaries$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1, d23=abs(`2008`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,5)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:6)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= PCB_summary2$d23_avg
PCB_summary3<-PCB_summary2[,c(1:3,5,6)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="estuaries"


fines<-subset(estuaries[estuaries$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="estuaries"



ETemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(estuaries[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="estuaries"


SDDT<-subset(estuaries[estuaries$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="estuaries"


SCu<-subset(estuaries[estuaries$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="estuaries"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subsetestuaries[estuaries$Parameter=="Total_DDT",c(1,2,3,5)])
STOC<-subset(estuaries[estuaries$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="estuaries"

STN<-subset(estuaries[estuaries$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="estuaries"


SZinc<-subset(estuaries[estuaries$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="estuaries"


SPAH<-subset(estuaries[estuaries$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="estuaries"


SPCB<-subset(estuaries[estuaries$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="estuaries"



Sfines<-subset(estuaries[estuaries$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="estuaries"



ESpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)


ETotalVar<- rbind(ESpatialVar[,c(1:3,7,6,8,9,10)], ETemporalVar)


#Bays
##Temporal Variability

bri<-unique(bays[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
bri3<-merge(bri_summary,bri2[,c(1,5:7)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:8)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= max(bri_summary2$d12_avg, bri_summary2$d23_avg, bri_summary2$d13_avg)
bri_summary3<-bri_summary2[,c(1:3,7,8)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="bays"


DDT<-subset(bays[bays$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,6:8)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:8)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary2$maxDelta_avg= max(DDT_summary2$d12_avg, DDT_summary2$d23_avg, DDT_summary2$d13_avg)
DDT_summary3<-DDT_summary2[,c(1:3,7,8)]
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="bays"


Cu<-subset(bays[bays$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,6:8)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:8)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary2$maxDelta_avg= max(Cu_summary2$d12_avg, Cu_summary2$d23_avg, Cu_summary2$d13_avg)
Cu_summary3<-Cu_summary2[,c(1:3,7,8)]
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="bays"

TOC<-subset(bays[bays$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,6:8)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:8)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary2$maxDelta_avg= max(TOC_summary2$d12_avg, TOC_summary2$d23_avg, TOC_summary2$d13_avg)
TOC_summary3<-TOC_summary2[,c(1:3,7,8)]
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="bays"


TN<-subset(bays[bays$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
TN3<-merge(TN_summary,TN2.1[,c(1,6:8)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:8)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary2$maxDelta_avg= max(TN_summary2$d12_avg, TN_summary2$d23_avg, TN_summary2$d13_avg)
TN_summary3<-TN_summary2[,c(1:3,7,8)]
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="bays"


zinc<-subset(bays[bays$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,6:8)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:8)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary2$maxDelta_avg= max(zinc_summary2$d12_avg, zinc_summary2$d23_avg, zinc_summary2$d13_avg)
zinc_summary3<-zinc_summary2[,c(1:3,7,8)]
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="bays"


PAH<-subset(bays[bays$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,6:8)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:8)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary2$maxDelta_avg= max(PAH_summary2$d12_avg, PAH_summary2$d23_avg, PAH_summary2$d13_avg)
PAH_summary3<-PAH_summary2[,c(1:3,7,8)]
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="bays"


PCB<-subset(bays[bays$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,6:8)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:8)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary2$maxDelta_avg= max(PCB_summary2$d12_avg, PCB_summary2$d23_avg, PCB_summary2$d13_avg)
PCB_summary3<-PCB_summary2[,c(1:3,7,8)]
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="bays"


fines<-subset(bays[bays$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,d12=abs(`9803`-`2008`), d23=abs(`2008`-`2013`), d13=abs(`9803`-`2013`))
fines3<-merge(fines_summary,fines2.1[,c(1,6:8)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:8)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary2$maxDelta_avg= max(fines_summary2$d12_avg, fines_summary2$d23_avg, fines_summary2$d13_avg)
fines_summary3<-fines_summary2[,c(1:3,7,8)]
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="bays"



BTemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(bays[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="bays"


SDDT<-subset(bays[bays$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="bays"


SCu<-subset(bays[bays$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="bays"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(bays[bays$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(bays[bays$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="bays"

STN<-subset(bays[bays$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="bays"


SZinc<-subset(bays[bays$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="bays"


SPAH<-subset(bays[bays$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="bays"


SPCB<-subset(bays[bays$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="bays"



Sfines<-subset(bays[bays$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="bays"



BSpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)


BTotalVar<- rbind(BSpatialVar[,c(1:3,7,6,8,9,10)], BTemporalVar)


#CHANNEL ISLANDS
##Temporal Variability

bri<-unique(islands[, c(1,2,7)])
bri<-na.omit(bri)
bri$SampleYear<-ifelse(bri$SampleYear<=2003,9803,bri$SampleYear)
bri_summary<-ddply(bri, .(OStationID), summarise,
                   avg=mean(BRI_Score),
                   stdev=sd(BRI_Score),
                   variance=var(BRI_Score))
bri2<-spread(bri,key =SampleYear,value = BRI_Score )
bri2<-mutate(bri2,d12=abs(`9803`-`2008`))
bri3<-merge(bri_summary,bri2[,c(1,4)], by=c("OStationID"))
bri3<-mutate(bri3,cv=((stdev/avg)*100))
bri3.2<-na.omit(bri3)

bri3.2<-bri3.2[,c(2:6)]
bri_summary2<-summarise_all(bri3.2, funs(avg=mean))
row.names(bri_summary2)<-"BRI Temporal Var"
bri_summary2$maxDelta_avg= (bri_summary2$d12_avg)
bri_summary3<-bri_summary2[,c(1:3,5,6)]
bri_summary3$type="time" 
bri_summary3$parameter="BRI"
bri_summary3$strata="channel islands"


DDT<-subset(islands[islands$Parameter=="Total_DDT",c(1,2,3,5)])
DDT$SampleYear<-ifelse(DDT$SampleYear<=2003,9803,DDT$SampleYear)

DDT2<-DDT %>% complete(OStationID, SampleYear, Parameter)
DDT2[is.na(DDT2)]<-0


DDT_summary<-ddply(DDT2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

DDT2.1<-spread(DDT2,key =SampleYear,value = MaxOfResult)
DDT2.1<-mutate(DDT2.1,maxDelta=abs(`9803`-`2008`))
DDT3<-merge(DDT_summary,DDT2.1[,c(1,5)], by=c("OStationID"))
DDT3<-mutate(DDT3,cv=((stdev/avg)*100))
DDT3.2<-DDT3[,c(2:6)]
DDT_summary2<-summarise_all(DDT3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(DDT_summary2)<-"DDT Temporal Var"
DDT_summary3<-DDT_summary2
DDT_summary3$type="time"
DDT_summary3$parameter="DDT"
DDT_summary3$strata="channel islands"


Cu<-subset(islands[islands$Parameter=="Copper",c(1,2,3,5)])
Cu$SampleYear<-ifelse(Cu$SampleYear<=2003,9803,Cu$SampleYear)
Cu2<-Cu %>% complete(OStationID, SampleYear, Parameter)
Cu2[is.na(Cu2)]<-0


Cu_summary<-ddply(Cu2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

Cu2.1<-spread(Cu2,key =SampleYear,value = MaxOfResult)
Cu2.1<-mutate(Cu2.1,maxDelta=abs(`9803`-`2008`))
Cu3<-merge(Cu_summary,Cu2.1[,c(1,5)], by=c("OStationID"))
Cu3<-mutate(Cu3,cv=((stdev/avg)*100))
Cu3.2<-Cu3[,c(2:6)]
Cu_summary2<-summarise_all(Cu3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Cu_summary2)<-"Cu Temporal Var"
Cu_summary3<-Cu_summary2
Cu_summary3$type="time"
Cu_summary3$parameter="Copper"
Cu_summary3$strata="channel islands"

TOC<-subset(islands[islands$Parameter=="TOC",c(1,2,3,5)])
TOC$SampleYear<-ifelse(TOC$SampleYear<=2003,9803,TOC$SampleYear)

TOC2<-TOC %>% complete(OStationID, SampleYear, Parameter)
TOC2[is.na(TOC2)]<-0


TOC_summary<-ddply(TOC2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

TOC2.1<-spread(TOC2,key =SampleYear,value = MaxOfResult)
TOC2.1<-mutate(TOC2.1,maxDelta=abs(`9803`-`2008`))
TOC3<-merge(TOC_summary,TOC2.1[,c(1,5)], by=c("OStationID"))
TOC3<-mutate(TOC3,cv=((stdev/avg)*100))
TOC3.2<-TOC3[,c(2:6)]
TOC_summary2<-summarise_all(TOC3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TOC_summary2)<-"TOC Temporal Var"
TOC_summary3<-TOC_summary2
TOC_summary3$type="time"
TOC_summary3$parameter="TOC"
TOC_summary3$strata="channel islands"


TN<-subset(islands[islands$Parameter=="TN",c(1,2,3,5)])
TN$SampleYear<-ifelse(TN$SampleYear<=2003,9803,TN$SampleYear)

TN2<-TN %>% complete(OStationID, SampleYear, Parameter)
TN2[is.na(TN2)]<-0


TN_summary<-ddply(TN2, .(OStationID), summarise,
                  avg=mean(MaxOfResult),
                  stdev=sd(MaxOfResult),
                  variance=var(MaxOfResult))

TN2.1<-spread(TN2,key =SampleYear,value = MaxOfResult)
TN2.1<-mutate(TN2.1,maxDelta=abs(`9803`-`2008`))
TN3<-merge(TN_summary,TN2.1[,c(1,5)], by=c("OStationID"))
TN3<-mutate(TN3,cv=((stdev/avg)*100))
TN3.2<-TN3[,c(2:6)]
TN_summary2<-summarise_all(TN3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(TN_summary2)<-"TN Temporal Var"
TN_summary3<-TN_summary2
TN_summary3$type="time"
TN_summary3$parameter="TN"
TN_summary3$strata="channel islands"


zinc<-subset(islands[islands$Parameter=="Zinc",c(1,2,3,5)])
zinc$SampleYear<-ifelse(zinc$SampleYear<=2003,9803,zinc$SampleYear)

zinc2<-zinc %>% complete(OStationID, SampleYear, Parameter)
zinc2[is.na(zinc2)]<-0


zinc_summary<-ddply(zinc2, .(OStationID), summarise,
                    avg=mean(MaxOfResult),
                    stdev=sd(MaxOfResult),
                    variance=var(MaxOfResult))

zinc2.1<-spread(zinc2,key =SampleYear,value = MaxOfResult)
zinc2.1<-mutate(zinc2.1,maxDelta=abs(`9803`-`2008`))
zinc3<-merge(zinc_summary,zinc2.1[,c(1,5)], by=c("OStationID"))
zinc3<-mutate(zinc3,cv=((stdev/avg)*100))
zinc3.2<-zinc3[,c(2:6)]
zinc_summary2<-summarise_all(zinc3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(zinc_summary2)<-"Zinc Temporal Var"
zinc_summary3<-zinc_summary2
zinc_summary3$type="time"
zinc_summary3$parameter="Zinc"
zinc_summary3$strata="channel islands"


PAH<-subset(islands[islands$Parameter=="Total_PAH",c(1,2,3,5)])
PAH$SampleYear<-ifelse(PAH$SampleYear<=2003,9803,PAH$SampleYear)

PAH2<-PAH %>% complete(OStationID, SampleYear, Parameter)
PAH2[is.na(PAH2)]<-0


PAH_summary<-ddply(PAH2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PAH2.1<-spread(PAH2,key =SampleYear,value = MaxOfResult)
PAH2.1<-mutate(PAH2.1,maxDelta=abs(`9803`-`2008`))
PAH3<-merge(PAH_summary,PAH2.1[,c(1,5)], by=c("OStationID"))
PAH3<-mutate(PAH3,cv=((stdev/avg)*100))
PAH3.2<-PAH3[,c(2:6)]
PAH_summary2<-summarise_all(PAH3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PAH_summary2)<-"PAH Temporal Var"
PAH_summary3<-PAH_summary2
PAH_summary3$type="time"
PAH_summary3$parameter="PAH"
PAH_summary3$strata="channel islands"


PCB<-subset(islands[islands$Parameter=="Total_PCB",c(1,2,3,5)])
PCB$SampleYear<-ifelse(PCB$SampleYear<=2003,9803,PCB$SampleYear)

PCB2<-PCB %>% complete(OStationID, SampleYear, Parameter)
PCB2[is.na(PCB2)]<-0


PCB_summary<-ddply(PCB2, .(OStationID), summarise,
                   avg=mean(MaxOfResult),
                   stdev=sd(MaxOfResult),
                   variance=var(MaxOfResult))

PCB2.1<-spread(PCB2,key =SampleYear,value = MaxOfResult)
PCB2.1<-mutate(PCB2.1,maxDelta=abs(`9803`-`2008`))
PCB3<-merge(PCB_summary,PCB2.1[,c(1,5)], by=c("OStationID"))
PCB3<-mutate(PCB3,cv=((stdev/avg)*100))
PCB3.2<-PCB3[,c(2:6)]
PCB_summary2<-summarise_all(PCB3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(PCB_summary2)<-"PCB Temporal Var"
PCB_summary3<-PCB_summary2
PCB_summary3$type="time"
PCB_summary3$parameter="PCB"
PCB_summary3$strata="channel islands"


fines<-subset(islands[islands$Parameter=="PercentFines",c(1,2,3,5)])
fines$SampleYear<-ifelse(fines$SampleYear<=2003,9803,fines$SampleYear)

fines2<-fines %>% complete(OStationID, SampleYear, Parameter)
fines2[is.na(fines2)]<-0


fines_summary<-ddply(fines2, .(OStationID), summarise,
                     avg=mean(MaxOfResult),
                     stdev=sd(MaxOfResult),
                     variance=var(MaxOfResult))

fines2.1<-spread(fines2,key =SampleYear,value = MaxOfResult)
fines2.1<-mutate(fines2.1,maxDelta=abs(`9803`-`2008`))
fines3<-merge(fines_summary,fines2.1[,c(1,5)], by=c("OStationID"))
fines3<-mutate(fines3,cv=((stdev/avg)*100))
fines3.2<-fines3[,c(2:6)]
fines_summary2<-summarise_all(fines3.2, funs(avg=mean(.,na.rm=TRUE)))
row.names(fines_summary2)<-"Fines Temporal Var"
fines_summary3<-fines_summary2
fines_summary3$type="time"
fines_summary3$parameter="Percent Fines"
fines_summary3$strata="channel islands"



CITemporalVar<- rbind(fines_summary3,PCB_summary3, PAH_summary3, zinc_summary3,TN_summary3,TOC_summary3, Cu_summary3, DDT_summary3, bri_summary3)



##Spatial Variablity
Sbri<-unique(islands[, c(1,2,7)])
Sbri_summary<-ddply(Sbri, .(SampleYear), summarise,
                    avg=mean(BRI_Score,na.rm=TRUE),
                    stdev=sd(BRI_Score,na.rm=TRUE),
                    variance=var(BRI_Score,na.rm=TRUE),
                    minimum= min(BRI_Score,na.rm=TRUE),
                    maximum=max(BRI_Score,na.rm=TRUE))
Sbri_summary$maxDelta<- Sbri_summary$maximum-Sbri_summary$minimum 
Sbri_summary<-mutate(Sbri_summary,cv=((stdev/avg)*100))
Sbri2<-Sbri_summary[,c(2:8)]
Sbri_summary2<-summarise_all(Sbri2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sbri_summary2)<-"BRI Spatial Var"
Sbri_summary2$type="space"
Sbri_summary2$parameter="BRI"
Sbri_summary2$strata="channel islands"


SDDT<-subset(islands[islands$Parameter=="Total_DDT",c(1,2,3,5)])
SDDT[is.na(SDDT)]<-0
SDDT_summary<-ddply(SDDT, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SDDT_summary$maxDelta<- SDDT_summary$maximum-SDDT_summary$minimum 
SDDT_summary<-mutate(SDDT_summary,cv=((stdev/avg)*100))
SDDT2<-SDDT_summary[,c(2:8)]
SDDT_summary2<-summarise_all(SDDT2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SDDT_summary2)<-"DDT Spatial Var"
SDDT_summary2$type="space"
SDDT_summary2$parameter="DDT"
SDDT_summary2$strata="channel islands"


SCu<-subset(islands[islands$Parameter=="Copper",c(1,2,3,5)])
SCu[is.na(SCu)]<-0
SCu_summary<-ddply(SCu, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
SCu_summary$maxDelta<- SCu_summary$maximum-SCu_summary$minimum 
SCu_summary<-mutate(SCu_summary,cv=((stdev/avg)*100))
SCu2<-SCu_summary[,c(2:8)]
SCu_summary2<-summarise_all(SCu2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SCu_summary2)<-"Cu Spatial Var"
SCu_summary2$type="space"
SCu_summary2$parameter="Copper"
SCu_summary2$strata="channel islands"

#ORIGINAL LINE OF CODE BELOW - INCORRECT - THIS PARAMETER SHOULD BE TOC!!!!
#KAREN.Q
#STOC<-subset(islands[islands$Parameter=="Total_DDT",c(1,2,3,5)])

STOC<-subset(islands[islands$Parameter=="TOC",c(1,2,3,5)])
STOC[is.na(STOC)]<-0
STOC_summary<-ddply(STOC, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
STOC_summary$maxDelta<- STOC_summary$maximum-STOC_summary$minimum 
STOC_summary<-mutate(STOC_summary,cv=((stdev/avg)*100))
STOC2<-STOC_summary[,c(2:8)]
STOC_summary2<-summarise_all(STOC2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STOC_summary2)<-"TOC Spatial Var"
STOC_summary2$type="space"
STOC_summary2$parameter="TOC"
STOC_summary2$strata="channel islands"

STN<-subset(islands[islands$Parameter=="TN",c(1,2,3,5)])
STN[is.na(STN)]<-0
STN_summary<-ddply(STN, .(SampleYear), summarise,
                   avg=mean(MaxOfResult,na.rm=TRUE),
                   stdev=sd(MaxOfResult,na.rm=TRUE),
                   variance=var(MaxOfResult,na.rm=TRUE),
                   minimum= min(MaxOfResult,na.rm=TRUE),
                   maximum=max(MaxOfResult,na.rm=TRUE))
STN_summary$maxDelta<- STN_summary$maximum-STN_summary$minimum 
STN_summary<-mutate(STN_summary,cv=((stdev/avg)*100))
STN2<-STN_summary[,c(2:8)]
STN_summary2<-summarise_all(STN2, funs(avg=mean(.,na.rm=TRUE)))
row.names(STN_summary2)<-"TN Spatial Var"
STN_summary2$type="space"
STN_summary2$parameter="TN"
STN_summary2$strata="channel islands"


SZinc<-subset(islands[islands$Parameter=="Zinc",c(1,2,3,5)])
SZinc[is.na(SZinc)]<-0
SZinc_summary<-ddply(SZinc, .(SampleYear), summarise,
                     avg=mean(MaxOfResult,na.rm=TRUE),
                     stdev=sd(MaxOfResult,na.rm=TRUE),
                     variance=var(MaxOfResult,na.rm=TRUE),
                     minimum= min(MaxOfResult,na.rm=TRUE),
                     maximum=max(MaxOfResult,na.rm=TRUE))
SZinc_summary$maxDelta<- SZinc_summary$maximum-SZinc_summary$minimum 
SZinc_summary<-mutate(SZinc_summary,cv=((stdev/avg)*100))
SZinc2<-SZinc_summary[,c(2:8)]
SZinc_summary2<-summarise_all(SZinc2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SZinc_summary2)<-"Zinc Spatial Var"
SZinc_summary2$type="space"
SZinc_summary2$parameter="Zinc"
SZinc_summary2$strata="channel islands"


SPAH<-subset(islands[islands$Parameter=="Total_PAH",c(1,2,3,5)])
SPAH[is.na(SPAH)]<-0
SPAH_summary<-ddply(SPAH, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPAH_summary$maxDelta<- SPAH_summary$maximum-SPAH_summary$minimum 
SPAH_summary<-mutate(SPAH_summary,cv=((stdev/avg)*100))
SPAH2<-SPAH_summary[,c(2:8)]
SPAH_summary2<-summarise_all(SPAH2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPAH_summary2)<-"PAH Spatial Var"
SPAH_summary2$type="space"
SPAH_summary2$parameter="PAH"
SPAH_summary2$strata="channel islands"


SPCB<-subset(islands[islands$Parameter=="Total_PCB",c(1,2,3,5)])
SPCB[is.na(SPCB)]<-0
SPCB_summary<-ddply(SPCB, .(SampleYear), summarise,
                    avg=mean(MaxOfResult,na.rm=TRUE),
                    stdev=sd(MaxOfResult,na.rm=TRUE),
                    variance=var(MaxOfResult,na.rm=TRUE),
                    minimum= min(MaxOfResult,na.rm=TRUE),
                    maximum=max(MaxOfResult,na.rm=TRUE))
SPCB_summary$maxDelta<- SPCB_summary$maximum-SPCB_summary$minimum 
SPCB_summary<-mutate(SPCB_summary,cv=((stdev/avg)*100))
SPCB2<-SPCB_summary[,c(2:8)]
SPCB_summary2<-summarise_all(SPCB2, funs(avg=mean(.,na.rm=TRUE)))
row.names(SPCB_summary2)<-"PCB Spatial Var"
SPCB_summary2$type="space"
SPCB_summary2$parameter="PCB"
SPCB_summary2$strata="channel islands"



Sfines<-subset(islands[islands$Parameter=="PercentFines",c(1,2,3,5)])
Sfines[is.na(Sfines)]<-0
Sfines_summary<-ddply(Sfines, .(SampleYear), summarise,
                      avg=mean(MaxOfResult,na.rm=TRUE),
                      stdev=sd(MaxOfResult,na.rm=TRUE),
                      variance=var(MaxOfResult,na.rm=TRUE),
                      minimum= min(MaxOfResult,na.rm=TRUE),
                      maximum=max(MaxOfResult,na.rm=TRUE))
Sfines_summary$maxDelta<- Sfines_summary$maximum-Sfines_summary$minimum 
Sfines_summary<-mutate(Sfines_summary,cv=((stdev/avg)*100))
Sfines2<-Sfines_summary[,c(2:8)]
Sfines_summary2<-summarise_all(Sfines2, funs(avg=mean(.,na.rm=TRUE)))
row.names(Sfines_summary2)<-"Fines Spatial Var"
Sfines_summary2$type="space"
Sfines_summary2$parameter="Percent Fines"
Sfines_summary2$strata="channel islands"



CISpatialVar<- rbind(Sfines_summary2,SPCB_summary2, SPAH_summary2, SZinc_summary2, STN_summary2, STOC_summary2, SCu_summary2, SDDT_summary2, Sbri_summary2)


CITotalVar<- rbind(CISpatialVar[,c(1:3,7,6,8,9,10)], CITemporalVar)


##Summary Table

AllTemporalVar<-rbind(ISTemporalVar,MSTemporalVar,OSTemporalVar,LSTemporalVar,USTemporalVar,PTemporalVar,MTemporalVar,ETemporalVar,BTemporalVar,CITemporalVar)

AllSpatialVar<-rbind(ISSpatialVar,MSSpatialVar,OSSpatialVar,LSSpatialVar,USSpatialVar,PSpatialVar,MSpatialVar,ESpatialVar,BSpatialVar,CISpatialVar)

AllTotalVar<- rbind(AllSpatialVar[,c(1:3,7,6,8,9,10)], AllTemporalVar)

write.table(AllTotalVar, "L:/Bight 18/B18 Survey Design/Revisit Study/Results/AllTotalVar.txt")

#graphics

#BRI
BRIvar<-subset(AllTotalVar[AllTotalVar$parameter=="BRI", c(1:6,8)])

ggplot(BRIvar,aes(factor(strata), avg_avg, fill=type))+
      geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("BRI")

ggplot(BRIvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("BRI")

ggplot(BRIvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("BRI")

ggplot(BRIvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("BRI")

ggplot(BRIvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("BRI")

#Copper
Cuvar<-subset(AllTotalVar[AllTotalVar$parameter=="Copper", c(1:6,8)])

ggplot(Cuvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Copper")

ggplot(Cuvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Copper")

ggplot(Cuvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Copper")

ggplot(Cuvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Copper")

ggplot(Cuvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Copper")


#Zinc
Zincvar<-subset(AllTotalVar[AllTotalVar$parameter=="Zinc", c(1:6,8)])

ggplot(Zincvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Zinc")

ggplot(Zincvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Zinc")

ggplot(Zincvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Zinc")

ggplot(Zincvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Zinc")

ggplot(Zincvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Zinc")


#DDT
DDTvar<-subset(AllTotalVar[AllTotalVar$parameter=="DDT", c(1:6,8)])

ggplot(DDTvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("DDT")

ggplot(DDTvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("DDT")

ggplot(DDTvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("DDT")

ggplot(DDTvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("DDT")

ggplot(DDTvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("DDT")



#PAH
PAHvar<-subset(AllTotalVar[AllTotalVar$parameter=="PAH", c(1:6,8)])

ggplot(PAHvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PAH")

ggplot(PAHvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PAH")

ggplot(PAHvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PAH")

ggplot(PAHvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PAH")

ggplot(PAHvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PAH")



#PCB
PCBvar<-subset(AllTotalVar[AllTotalVar$parameter=="PCB", c(1:6,8)])

ggplot(PCBvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PCB")

ggplot(PCBvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PCB")

ggplot(PCBvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PCB")

ggplot(PCBvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PCB")

ggplot(PCBvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("PCB")



#Percent Fines
PerFinesvar<-subset(AllTotalVar[AllTotalVar$parameter=="Percent Fines", c(1:6,8)])

ggplot(PerFinesvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Percent Fines")

ggplot(PerFinesvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Percent Fines")

ggplot(PerFinesvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Percent Finess")

ggplot(PerFinesvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Percent Fines")

ggplot(PerFinesvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Percent Fines")


#TN
TNvar<-subset(AllTotalVar[AllTotalVar$parameter=="TN", c(1:6,8)])

ggplot(TNvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TN")

ggplot(TNvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TN")

ggplot(TNvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TN")

ggplot(TNvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TN")

ggplot(TNvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TN")


#TOC
TOCvar<-subset(AllTotalVar[AllTotalVar$parameter=="TOC", c(1:6,8)])

ggplot(TOCvar,aes(factor(strata), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TOC")

ggplot(TOCvar,aes(factor(strata), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TOC")

ggplot(TOCvar,aes(factor(strata), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TOC")

ggplot(TOCvar,aes(factor(strata), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TOC")

ggplot(TOCvar,aes(factor(strata), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("TOC")


#Strata
#Inner Shelf

ISvar<-subset(AllTotalVar[AllTotalVar$strata=="inner shelf", c(1:7)])

ggplot(ISvar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Inner Shelf")

ggplot(ISvar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Inner Shelf")

ggplot(ISvar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Inner Shelf")

ggplot(ISvar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Inner Shelf")

ggplot(ISvar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("Inner Shelf")

#Mid Shelf

MSVar<-subset(AllTotalVar[AllTotalVar$strata=="mid shelf", c(1:7)])

ggplot(MSVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("mid shelf")

ggplot(MSVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("mid shelf")

ggplot(MSVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("mid shelf")

ggplot(MSVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("mid shelf")

ggplot(MSVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("mid shelf")

#outer shelf

OSVar<-subset(AllTotalVar[AllTotalVar$strata=="outer shelf", c(1:7)])

ggplot(OSVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("outer shelf")

ggplot(OSVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("outer shelf")

ggplot(OSVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("outer shelf")

ggplot(OSVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("outer shelf")

ggplot(OSVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("outer shelf")

#upper slope

USVar<-subset(AllTotalVar[AllTotalVar$strata=="upper slope", c(1:7)])

ggplot(USVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("upper slope")

ggplot(USVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("upper slope")

ggplot(USVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("upper slope")

ggplot(USVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("upper slope")

ggplot(USVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("upper slope")

#lower slope

LSVar<-subset(AllTotalVar[AllTotalVar$strata=="lower slope", c(1:7)])

ggplot(LSVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("lower slope")

ggplot(LSVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("lower slope")

ggplot(LSVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("lower slope")

ggplot(LSVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("lower slope")

ggplot(LSVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("lower slope")

#bays

BVar<-subset(AllTotalVar[AllTotalVar$strata=="bays", c(1:7)])

ggplot(BVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("bays")

ggplot(BVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("bays")

ggplot(BVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("bays")

ggplot(BVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("bays")

ggplot(BVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("bays")

#marinas

MVar<-subset(AllTotalVar[AllTotalVar$strata=="marinas", c(1:7)])

ggplot(MVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("marinas")

ggplot(MVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("marinas")

ggplot(MVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("marinas")

ggplot(MVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("marinas")

ggplot(MVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("marinas")

#estuaries

EVar<-subset(AllTotalVar[AllTotalVar$strata=="estuaries", c(1:7)])

ggplot(EVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("estuaries")

ggplot(EVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("estuaries")

ggplot(EVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("estuaries")

ggplot(EVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("estuaries")

ggplot(EVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("estuaries")

#ports

PVar<-subset(AllTotalVar[AllTotalVar$strata=="ports", c(1:7)])

ggplot(PVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("ports")

ggplot(PVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("ports")

ggplot(PVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("ports")

ggplot(PVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("ports")

ggplot(PVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("ports")

#channel islands

CIVar<-subset(AllTotalVar[AllTotalVar$strata=="channel islands", c(1:7)])

ggplot(CIVar,aes(factor(parameter), avg_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("channel islands")

ggplot(CIVar,aes(factor(parameter), stdev_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("channel islands")

ggplot(CIVar,aes(factor(parameter), variance_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("channel islands")

ggplot(CIVar,aes(factor(parameter), cv_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("channel islands")

ggplot(CIVar,aes(factor(parameter), maxDelta_avg, fill=type))+
  geom_bar(stat="identity", position = "dodge")+scale_fill_brewer(palette = "Set1")+ggtitle("channel islands")

