
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

OCI_subset <- MyData[c(1:14,20,29,49,50,51,52,110:117)]

names(OCI_subset)

## Import and manipulate the activity data, make sure the filepath is correct

MyData2 <- read.csv(file="~/R_Studio_Projects/GFC/New Data/Activity_Flat.csv",header=TRUE)

names(MyData2)

## Need to total number of activities per year per organization
## I'm most comfortable doing that in SQL with Groupbys
## Need to rename some columns first though:

colnames(MyData2)[colnames(MyData2)=="Org.Name"] <- "Org_Name"
colnames(MyData2)[colnames(MyData2)=="Capacity.Building.Type"] <- "Capacity_Building_Type"
colnames(MyData2)[colnames(MyData2)=="Leveraging.Amount"] <- "Leveraging_Amount"


names(MyData2)

library(sqldf)
library(pastecs)


##Need to convert "done date" to fiscal year by shifting by 184 days (from jan 1 to june 30)
MyData2$Done.Dateform <- as.Date(MyData2$Done.Date,format = "%m/%d/%Y")
names(MyData2)
head(MyData2)
MyData2$Fiscal_Year <- format(MyData2$Done.Dateform  + 184,"%Y")
head(MyData2)






##Create list of orgs and fiscal years

ActivityData <- sqldf("SELECT Org_Name , Fiscal_Year FROM MyData2 GROUP BY Org_Name, Fiscal_Year")
head(ActivityData)
##Create site visit column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, b.Site_Visit from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Site_Visit' FROM MyData2 WHERE Capacity_Building_Type = 'Site Visit' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create Kowledge Exchange Column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, b.Knowledge_Exchange from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Knowledge_Exchange' FROM MyData2 WHERE Capacity_Building_Type = 'Knowledge Exchange Participation' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create Leveraging Column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , b.Leveraging  from ActivityData AS a LEFT JOIN 
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
##Create "aditional" column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, a.Email, b.Additional from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Additional' FROM MyData2 WHERE Capacity_Building_Type = 'Additional Touch' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create "Legal Referral" column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, a.Email, a.Additional,b.Legal_Refer from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Legal_Refer' FROM MyData2 WHERE Capacity_Building_Type = 'Legal Referral' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")



stat.desc(ActivityData)

##run a few data validation checks... took me a while to figure out some problems I had that led to me not counting certain things.-
sum(MyData2$Capacity_Building_Type == 'Knowledge Exchange Participation')
sum(ActivityData$Knowledge_Exchange, na.rm=TRUE)
sum(is.na(ActivityData$Knowledge_Exchange) == FALSE)

sum(MyData2$Capacity_Building_Type == 'Site Visit')
sum(ActivityData$Site_Visit, na.rm=TRUE)
sum(is.na(ActivityData$Site_Visit) == FALSE)




## Next steps: join OCI_subset and ActivityData, then run LEAPS data on new combined dataframe to find correlations

##rename the columns so they're easier to work with
names(OCI_subset)
colnames(OCI_subset)[colnames(OCI_subset)=="Organization.Name"] <- "Org_Name"
colnames(OCI_subset)[colnames(OCI_subset)=="Fiscal.Year"] <- "Fiscal_Year"
names(OCI_subset)


## do some data validation to check if the datasets match on the fields of interest
testdata <- sqldf("Select Org_Name, Fiscal_Year from OCI_subset group by Org_Name, Fiscal_Year")
summary(testdata)
summary(sqldf("Select Org_Name, Fiscal_Year from ActivityData group by Org_Name, Fiscal_Year"))


summary(sqldf("Select a.Org_Name, a.Fiscal_Year FROM ActivityData AS a INNER JOIN 
               OCI_subset AS b
               ON a.Org_Name = b.Org_Name AND a.Fiscal_Year = b.Fiscal_Year"))

summary(sqldf("Select a.Org_Name, a.Fiscal_Year FROM ActivityData AS a OUTER JOIN 
               OCI_subset AS b
               ON a.Org_Name = b.Org_Name AND a.Fiscal_Year = b.Fiscal_Year"))

MergedData <- merge(ActivityData,OCI_subset)

MergedData$ActivityCount <- with(MergedData, 
                                 ifelse(is.na(Site_Visit),0,Site_Visit) 
                                 + ifelse(is.na(Knowledge_Exchange),0,Knowledge_Exchange) 
                                 + ifelse(is.na(Leveraging),0,Leveraging)
                                 + ifelse(is.na(Phone_Call),0,Phone_Call)
                                 + ifelse(is.na(Email),0,Email)
                                 + ifelse(is.na(Additional),0,Additional)
                                 + ifelse(is.na(Legal_Refer),0,Legal_Refer))


summary(MergedData)
stat.desc(MergedData)
