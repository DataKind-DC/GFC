
## Probably a good idea to uncomment these for the packages:


#install.packages("leaps")
install.packages("sqldf")



##Read in the flat data. You may need to change the filepath below to match your environment
MyData <- read.csv(file="~/R_Studio_Projects/GFC/New Data/OCI_file_flat.csv",header=TRUE)



names(MyData)


##this simplifies the OCI data set, focusing on variables of interest

## Includes outputs: Children Directly Served, OCI data, Outcome Actual, Outcome Target
## Includes inputs: Grant Amount, Grant Type, 
## Includes characteristics: "Region, Country, Org name, Unique ID

## Needs calculation outputs: Budget Increase

## Needs Join inputs: phone calls, emails, additional touchpoints, legal referrals

## Dropped from consideration for now: "Awards Data" (requires join), "Grant use" (NLP complexity) 

OCI_subset <- MyData[c(1:14,29,49,50,51,52,110:117)]



## Import and manipulate the activity data, make sure the filepath is correct
MyData2 <- read.csv(file="~/R_Studio_Projects/GFC/New Data/Activity_Flat.csv",header=TRUE)

names(MyData2)

## Need to total number of activities per year per organization
## I'm most comfortable doing that in SQL with nested where clauses
## Need to rename some columns first though:

colnames(MyData2)[colnames(MyData2)=="Org.Name"] <- "Org_Name"
colnames(MyData2)[colnames(MyData2)=="Capacity.Building.Type"] <- "Capacity_Building_Type"


names(MyData2)

library(sqldf)

## Get site visits
sqldf("SELECT Org_Name , Activity_Year , Count(*) AS 'Site_Visit' FROM MyData2 WHERE Capacity_Building_Type = 'Site Visit' GROUP BY Org_Name, Activity_Year")

## Need to change the above line to put to a dataframe, then repeat the logic for all of the other activity types to build the new dataframe

