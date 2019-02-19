install.packages("openxlsx")
install.packages("lubridate")


## TODO: fix data type issue with Pending Approved Date
library(tidyverse)
library(dplyr)
library(magrittr)
library(openxlsx)
library(lubridate)

##Set local directory
DATA_DIR<-"C:/Users/rcarder/Documents/Dev/GFC/Raw Data (dont edit)/"
WRITE_DIR<-"C:/Users/rcarder/Documents/Dev/GFC/New Data/"

##Read in all data export files
grants2000<-read.xlsx(paste(DATA_DIR,
                            "All grants data since 2000 September 20",
                            " - KHYS notes.xlsx",
                            # using modified filesince pattern of labelling seems inconsistent
                            sep=""),
                            detectDates = T)

oci<-read.xlsx(paste(DATA_DIR,
                               "OCI_Generator for Datakind.xlsm",
                               sep=""),
                         sheet = "GrantData", detectDates = T)

activityraw<-read.xlsx(paste(DATA_DIR,
                          "Data Export of Activity Inputs September 10.xlsx",
                          sep=""),
                          detectDates = TRUE)

rankActivities<-read.xlsx(paste(DATA_DIR,
                          "Ranking of GFC inputs.xlsx",
                          sep=""))

awards<-read.xlsx(paste(DATA_DIR,
                         "Major Awards Spreadsheet.xlsx",
                          sep=""),
                          sheet = "Awards_Data")

##Adjust Weighting for Activities. Change number after each activity type to adjust how it is weighted.
rankActivities$ActivityWeight[rankActivities$GFC.input=="Knowledge Exchange Participation"]<-6
rankActivities$ActivityWeight[rankActivities$GFC.input=="Grantee-Led Convening Participation"]<-6
rankActivities$ActivityWeight[rankActivities$GFC.input=="Leveraging"]<-5
rankActivities$ActivityWeight[rankActivities$GFC.input=="Monitoring Site Visit"]<-5
rankActivities$ActivityWeight[rankActivities$GFC.input=="Site Visit"]<-4
rankActivities$ActivityWeight[rankActivities$GFC.input=="Meeting that is not a site visit or at a KE"]<-2.5
rankActivities$ActivityWeight[rankActivities$GFC.input=="Phone Call"]<-1.75
rankActivities$ActivityWeight[rankActivities$GFC.input=="E-mail"]<-1
rankActivities$ActivityWeight[rankActivities$GFC.input=="Legal Referral"]<-1
rankActivities$ActivityWeight[rankActivities$GFC.input=="Additional Touch"]<-1

##Join Rankings to Activities
activity<-left_join(activityraw,rankActivities, by=c("Capacity.Building.Type"="GFC.input"))

##Get year of Activity End, and aggregate by Organization-Year
activity$Year<-year(activity$Done.Date)

OrgActivity<- activity %>%
  group_by(Org.Name,Year) %>%
  summarize(TotalWeightedActivity=sum(ActivityWeight))%>%
  mutate(Id=paste(Org.Name,Year,sep=''))

##Remove unnecessary columns
oci<-oci[,1:57]

##Make Id in OCI data to correspond with Id in activity data created above
oci$Id<-paste(oci$Org.Name,oci$Fiscal.Year,sep='')


master<-left_join(oci,OrgActivity, by=c("Id"))

setwd(WRITE_DIR)
write.csv(master,"master.csv", row.names = FALSE)
















