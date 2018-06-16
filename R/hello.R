#Package for the mining of HESS data for gastroenterological purposes

library(readxl)
library(stringr)
library(dplyr)


ImportHES_Sheet<-function(FileName,sheetName){
  dfw <-read_excel(FileName,sheetName)
  field<-dfw$Field
  names(dfw)<- gsub(" ",".",names(dfw))
  fieldName<-dfw$Field.name
  value<-dfw$Value
  #Include Derivation field so can split the fields into raw (and populate these) and then derived fields (so populate these from the raw fields)
  Newdfw<-data.frame(field,value,dfw$Derivation)
  row.names(Newdfw)<-field
  Newdfw$field<-NULL
  Newdfw$value<-as.character(Newdfw$value)
  tNewdfw<-data.frame(t(Newdfw),stringsAsFactors = FALSE)
  tNewdfw[1,]<-as.character(tNewdfw[1,])
  return(tNewdfw)
}

#Import the Associated data:
library(readr)

#Postcode data:
PostcodesUKAll <- read_csv("~/HESSMineR/data/PostcodesUKAll.csv")
PostcodesUKAll<-data.frame(PostcodesUKAll)



#GP PRactice data:
GP_Practice_Data <- read_csv("~/HESSMineR/data/GP_Practice_Data.csv")
GP_Practice_Data<-data.frame(GP_Practice_Data)
#change postcode column name so can be merged with postcode
names(GP_Practice_Data)[names(GP_Practice_Data) == 'Postcode'] <- 'postcode'
#Lookup longitude and latitude of GP practice given postcode
GP_Practice_Data<-merge(PostcodesUKAll,GP_Practice_Data,by="postcode",all=F)
GP_Practice_Data$longitude2<-GP_Practice_Data$latitude
GP_Practice_Data$latitude<-GP_Practice_Data$longitude
GP_Practice_Data$longitude<-GP_Practice_Data$longitude2
GP_Practice_Data$longitude2<-NULL


#NHS Trust data  :
NHS_Trusts <- read_csv("~/HESSMineR/data/NHS_Trusts.csv")
NHS_Trusts<-data.frame(NHS_Trusts)
#change postcode column name so can be merged with postcode
names(NHS_Trusts)[names(NHS_Trusts) == 'Postcode'] <- 'postcode'
#Lookup longitude and latitude of GP practice given postcode
NHS_Trusts<-merge(PostcodesUKAll,NHS_Trusts,by="postcode",all=F)
NHS_Trusts$longitude2<-NHS_Trusts$latitude
NHS_Trusts$latitude<-NHS_Trusts$longitude
NHS_Trusts$longitude<-NHS_Trusts$longitude2
NHS_Trusts$longitude2<-NULL

# Import and clean the datasets, and split out the raw fields.
FileName<-"/home/rstudio/HESSMineR/data/HES+Data+Dictionary+25042018.xlsx"
sheetName<-"AE"
AE<-ImportHES_Sheet(FileName,sheetName)
#Grep out the columns with no derived rules
AE<-AE[!grepl("HES|TBC|SUS",AE[2,])]
#Get rid of the derivation row:
AE<-AE[-c(2), ]




sheetName<-"APC"
APC<-ImportHES_Sheet(FileName,sheetName)
#Grep out the columns with no derived rules
APC<-APC[!grepl("HES|TBC|SUS",APC[2,])]
#Get rid of the derivation row:
APC<-APC[-c(2), ]

sheetName<-"OP"
OP<-ImportHES_Sheet(FileName,sheetName)
OP<-OP[!grepl("HES|TBC|SUS",OP[2,])]
OP<-OP[-c(2), ]


#Creat the HESID and DOB for each patient and then use this in all the datasets.
myNum<-replicate(40,sample(100000:999999,1,replace=F))
MyHESID<-as.list(paste0(replicate(40,sample(LETTERS,1,replace=T)),myNum))
MyHESID<-as.character(replicate(100,sample(MyHESID,1,replace=T)))
MyfDOB<-as.list(as.character(sample(seq(as.Date('1940/01/01'), as.Date('1998/01/01'), by="day"), replace=T,40)))
MyfDOB<-as.character(replicate(100,sample(MyfDOB,1,replace=T)))



#Function that takes the column value,
#separates into a list of values and then
#populates a dataframe with a random selection of 1000 of those values for each column.

AE<-apply(AE, 2, function(x) {
  theList<-unlist(strsplit(x,"\n"))
  mylist<-replicate(1000,sample(theList,1,replace=F))
})
AE<-data.frame(AE,stringsAsFactors = FALSE)
AE<-data.frame(AE,MyHESID,MyfDOB,stringsAsFactors = FALSE)

APC<-apply(APC, 2, function(x) {
  theList<-unlist(strsplit(x,"\n"))
  mylist<-replicate(1000,sample(theList,1,replace=F))
})
APC<-data.frame(APC,stringsAsFactors = FALSE)
APC<-data.frame(APC,MyHESID,MyfDOB,stringsAsFactors = FALSE)


OP<-apply(OP, 2, function(x) {
  theList<-unlist(strsplit(x,"\n"))
  mylist<-replicate(1000,sample(theList,1,replace=F))
})
OP<-data.frame(OP,stringsAsFactors = FALSE)
OP<-data.frame(OP,MyHESID,MyfDOB,stringsAsFactors = FALSE)



######################### Create random fields for non derived fields. ###########################################################################
#Do this by going through the HES data dictionary and seeing if ther generated fields make sense. Highlight the fields that need further work on them
#There are three categories of fields that need to make sense
###### 1.Date fields######
#- these need to be ordered so they make sense. For AE it is:


# DOB
# MYDOB
#
#
# ARRIVALDATE
AE$ARRIVALDATE<-sample(seq(as.Date('2003/01/01'), as.Date('2018/01/01'), by="day"), 10,replace=T)
library(lubridate)
AE$FYEAR<-year(AE$ARRIVALDATE)
# FYEAR
#
#
hourTime<-(seq.POSIXt(as.POSIXct(Sys.Date()),
                      as.POSIXct(Sys.Date()+1),
                      by = "5 min"))
# Arrival time
AE$ARRIVALTIME <- sample(hourTime, 1000, replace=T)
#   |
#   |

#   Initial assessment time
INITTIME
AE$INITTIME <- AE$ARRIVALTIME +
  (hours(sample(1:3, 1000, replace=T)) +
     minutes(sample(1:60, 1000, replace=T)))
# |
#   |
#   |
#   Time seen for treatment
AE$TRETTIME <- AE$INITTIME +
  (hours(sample(1:3, 1000, replace=T)) +
     minutes(sample(1:60, 1000, replace=T)))
# |
#   |
#   |
#   Conclusion time
AE$CONCLTIME <- AE$TRETTIME +
  (hours(sample(1:3, 1000, replace=T)) +
     minutes(sample(1:60, 1000, replace=T)))
#   |
#   |
#   Departure time

AE$DEPTIME <- AE$CONCLTIME +
  (hours(sample(1:3, 1000, replace=T)) +
     minutes(sample(1:60, 1000, replace=T)))

#
# RTTPEREND
AE$RTTPEREND<-AE$DEPTIME
#
# CDSEXTDATE date of the update event that resulted in the need to exchange the data with the Secondary Uses Service.
#Get maximum date of the last RTT and add sample 1 month to it
AE$CDSEXTDATE<-AE$RTTPEREND+days(sample(1:28, 1000, replace=T))
# PARTYEAR year and month of the HES dataset in the format yyyymm.
AE$CDSEXTDATE<-AE$RTTPEREND
# PEREND  end date for the date range of the data.
AE$PEREND<-max(AE$CDSEXTDATE)
AE$SUBDATE<-AE$CDSEXTDATE

#Tidy up to get in the correct format
# RTTPEREND
AE$RTTPEREND<- format(AE$DEPTIME,"%d/%m/%Y", tz="GMT")



###### 2. NHS codes fields ######
###### 3. Geographical fields ######
temp<-paste0(PostcodesUKAll$postcode,"_",PostcodesUKAll$latitude,"_",PostcodesUKAll$longitude)
AE$HOMEADD_temp<-sample(temp,1000,replace=T)
#Add the longitude and latitude of the patient to the AE dataset
library(tidyr)
library(dplyr)
library(geosphere)
AE<-AE %>% separate(HOMEADD_temp, sep="_",c("postcode","long","lat"))
AE$long<-as.numeric(AE$long)
AE$lat<-as.numeric(AE$lat)

mat <- distm(AE[,c('long','lat')], GP_Practice_Data[,c('longitude','latitude')], fun=distVincentySphere)
# create distance matrix
# assign the name to the point in list1 based on shortest distance in the matrix. This won't always be the case but it suffices for now
AE$GPPRAC <- GP_Practice_Data$Address.Line.1[max.col(-mat)]

mat <- distm(AE[,c('long','lat')], NHS_Trusts[,c('longitude','latitude')], fun=distVincentySphere)
AE$PROCODE <- NHS_Trusts$Organisation.Code[max.col(-mat)]
#Add in the HRG for each trust
names(NHS_Trusts)[names(NHS_Trusts) == 'Organisation.Code'] <- 'PROCODE'

#Just select out the healthgrouping
NHS_Trusts2 <- NHS_Trusts %>%select(PROCODE, National.Grouping)
#Do a left join
AE<-left_join(AE, NHS_Trusts2, by = "PROCODE")
names(AE)[names(AE) == 'Organisation.Code'] <- 'HRGNHS'


#Create derived fields using pseudocode.
#Get externally derived paths.


#Functions for raw data generation

#Generate random hhmm

#Generate random dates for attendances between 2003-2018

#Generate random birth dates between 1930 to 2018



