
## Probably a good idea to uncomment these for the packages:


#install.packages("leaps")
install.packages("sqldf")
install.packages("pastecs")


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
colnames(MyData2)[colnames(MyData2)=="Leveraging.Amount"] <- "Leveraging_Amount"


names(MyData2)

library(sqldf)
library(pastecs)

MyData2$Done.Dateform <- as.Date(MyData2$Done.Date,format = "%m/%d/%Y")
names(MyData2)
head(MyData2)

MyData2$Fiscal_Year <- format(MyData2$Done.Dateform  + 184,"%Y")
head(MyData2)




## Get site visits
head(sqldf("SELECT Org_Name , Fiscal_Year , Count(*) AS 'Site_Visit' FROM MyData2 WHERE Capacity_Building_Type = 'Site Visit' GROUP BY Org_Name, Activity_Year"))


##Create first column
ActivityData <- sqldf("SELECT Org_Name , Fiscal_Year , Count(*) AS 'Site_Visit' FROM MyData2 WHERE Capacity_Building_Type = 'Site Visit' GROUP BY Org_Name, Fiscal_Year")
head(ActivityData)
##Create Kowledge Exchange Column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, b.Knowledge_Exchange from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Knowledge_Exchange' FROM MyData2 WHERE Capacity_Building_Type = 'Knowledge Exchange Participation' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create Leveraging Column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , b.Leveraging,  from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Leveraging' FROM MyData2 WHERE Capacity_Building_Type = 'Leveraging' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Total Leveraging amount for year - represented in thousands
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, b.Leveraging_Amount  from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, Sum(Leveraging_Amount) AS 'Leveraging_Amount' FROM MyData2 WHERE Capacity_Building_Type = 'Leveraging' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create Phone Call Column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, b.Phone_Call from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Phone_Call' FROM MyData2 WHERE Capacity_Building_Type = 'Phone Call' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")

##Create email column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, b.Email from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Email' FROM MyData2 WHERE Capacity_Building_Type = 'E-mail' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")

ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, a.Email, b.Additional from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Additional' FROM MyData2 WHERE Capacity_Building_Type = 'Additional Touch' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")

stat.desc(ActivityData)






## Need to change the above line to put to a dataframe, then repeat the logic for all of the other activity types to build the new dataframe

