if(!require(leaps))
  install.packages("leaps")
if(!require(sqldf))
  install.packages("sqldf")
if(!require(pastecs))
  install.packages("pastecs")

library(dplyr); library(tidyr)

##Read in the flat data. You may need to change the filepath below to match your environment
NEW_DATA_DIR<-"../New Data/"
MyData <- read.csv(file=paste(NEW_DATA_DIR, "OCI_file_flat.csv", sep=""),
                   header=TRUE)

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

MyData2 <- read.csv(paste(NEW_DATA_DIR, "Activity_Flat.csv", sep=""),
                    header=TRUE)

names(MyData2)

## Need to total number of activities per year per organization
## I'm most comfortable doing that in SQL with Groupbys
## Need to rename some columns first though:

MyData2<-MyData2 %>%
  rename(Org_Name = Org.Name,
         Capacity_Building_Type = Capacity.Building.Type,
         Leveraging_Amount = Leveraging.Amount)

names(MyData2)

##Need to convert "done date" to fiscal year by shifting by 184 days (from jan 1 to june 30)
MyData2$Done.Dateform <- as.Date(MyData2$Done.Date,format = "%m/%d/%Y")
names(MyData2)
head(MyData2)
MyData2$Fiscal_Year <- format(MyData2$Done.Dateform + 184,"%Y")
head(MyData2)

##Create list of orgs and fiscal years

ActivityData <- sqldf(
  paste("SELECT Org_Name, Fiscal_Year FROM MyData2",
        "GROUP BY Org_Name, Fiscal_Year", sep=" "))
head(ActivityData)
##Create site visit column
##Create site visit column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, b.Site_Visit",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, Count(*) AS 'Site_Visit'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Site Visit'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))
##Create Kowledge Exchange Column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "b.Knowledge_Exchange from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, Count(*) AS 'Knowledge_Exchange'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Knowledge Exchange Participation'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))
##Create Leveraging Column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange, b.Leveraging",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, Count(*) AS 'Leveraging'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Leveraging'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))

##Total Leveraging amount for year - represented in thousands
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange, a.Leveraging, b.Leveraging_Amount",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, Sum(Leveraging_Amount) AS",
        "'Leveraging_Amount'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Leveraging'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))
##Create Phone Call Column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange, a.Leveraging, a.Leveraging_Amount,",
        "b.Phone_Call",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, count(*) AS 'Phone_Call'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Phone Call'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))

##Create email column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange, a.Leveraging, a.Leveraging_Amount,",
        "a.Phone_Call, b.Email",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, count(*) AS 'Email'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'E-mail'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))
##Create "aditional" column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount,",
        "a.Phone_Call, a.Email, b.Additional",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, count(*) AS 'Additional'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Additional Touch'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))
##Create "Legal Referral" column
ActivityData <- sqldf(
  paste("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit,",
        "a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount,",
        "a.Phone_Call, a.Email, a.Additional, b.Legal_Refer",
        "from ActivityData AS a LEFT JOIN",
        "(SELECT Org_Name, Fiscal_Year, count(*) AS 'Legal_Refer'",
        "FROM MyData2",
        "WHERE Capacity_Building_Type = 'Legal Referral'",
        "GROUP BY Org_Name, Fiscal_Year) AS b",
        "ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year",
        sep=" "))

stat.desc(ActivityData)

##run a few data validation checks... took me a while to figure out some problems I had that led to me not counting certain things.-
stopifnot(sum(MyData2$Capacity_Building_Type ==
                'Knowledge Exchange Participation') ==
            sum(ActivityData$Knowledge_Exchange, na.rm=T))
sum(is.na(ActivityData$Knowledge_Exchange) == FALSE)

stopifnot(sum(MyData2$Capacity_Building_Type == 'Site Visit') ==
            sum(ActivityData$Site_Visit, na.rm=T))
sum(is.na(ActivityData$Site_Visit) == FALSE)

ActivityData[is.na(ActivityData)] <- 0

## Next steps: join OCI_subset and ActivityData, then run LEAPS data on new combined dataframe to find correlations

##rename the columns so they're easier to work with
# names(OCI_subset)
OCI_subset<-OCI_subset %>%
  rename(Org_Name = Organization.Name,
         Fiscal_Year = Fiscal.Year)
names(OCI_subset)

##Shifting actitivity data... assumption is that activities imapct following year funding

ActivityDataShift <- ActivityData
ActivityDataShift$Fiscal_Year <- as.integer(ActivityDataShift$Fiscal_Year) + 1

##Convert all of the merge datasets fiscal year to integer for matching purposes

ActivityData$Fiscal_Year <- as.integer(ActivityData$Fiscal_Year)
OCI_subset$Fiscal_Year <- as.integer(OCI_subset$Fiscal_Year)

MergedData_noshift <- merge(ActivityData,OCI_subset)

MergedData_shift <- merge(ActivityDataShift,OCI_subset)


##Create activity count calculated fields for regressions
MergedData_noshift<-MergedData_noshift %>%
  mutate_at(c("Site_Visit", "Knowledge_Exchange", "Leveraging", "Phone_Call",
              "Email", "Additional", "Legal_Refer"),
            replace_na,
            replace=0)
MergedData_noshift<-MergedData_noshift %>%
  mutate(ActivityCount=Site_Visit + Knowledge_Exchange + Leveraging +
           Phone_Call + Email + Additional + Legal_Refer)

MergedData_shift<-MergedData_shift %>%
  mutate_at(c("Site_Visit", "Knowledge_Exchange", "Leveraging", "Phone_Call",
              "Email", "Additional", "Legal_Refer"),
            replace_na,
            replace=0)
MergedData_shift<-MergedData_shift %>%
  mutate(ActivityCount=Site_Visit + Knowledge_Exchange + Leveraging +
           Phone_Call + Email + Additional + Legal_Refer)

##convert grant amounts to integers
MergedData_shift$Grant.Amount <- as.integer(MergedData_shift$Grant.Amount)
MergedData_noshift$Grant.Amount <- as.integer(MergedData_noshift$Grant.Amount)

summary(MergedData_noshift)
##noshift means unshifted, shift means shifted activity data 
stat.desc(MergedData_noshift)
stat.desc(MergedData_shift)

names(MergedData_noshift)
## Includes outputs: Children Directly Served, OCI data, Outcome Actual, Outcome Target
## Includes inputs: Grant Amount, Grant Type, Activity Data

##First attempt saving
Children_Served.out <-
  regsubsets(Children.Served.Directly ~
               ActivityCount + 
               Grant.Amount + 
               #Site_Visit + 
               Knowledge_Exchange + 
               Leveraging + 
               #Leveraging_Amount + 
               Phone_Call + 
               Email + 
               Additional + 
               Legal_Refer + 
               Grant.Amount + 
               Year.of.Funding.Number.Only
             ,data = MergedData_shift
             ,method='forward')

MergedData_shift$Grant.Amount <- as.numeric(MergedData_shift$Grant.Amount)

length(MergedData_shift$Email)
length(MergedData_shift$Phone_Call)
length(MergedData_shift$Children.Served.Directly)

Children_Served <- regsubsets( Children.Served.Directly ~  Grant.Amount + ActivityCount + Email + Phone_Call + Knowledge_Exchange
                               + Leveraging + as.integer(Leveraging_Amount) + Additional + Legal_Refer + Year.of.Funding.Number.Only
                               ,data=MergedData_shift,method='forward')

## "figure margins too large"
# plot(Children_Served,main="adjr2") #scale = "adjr2",main="adjr2")
## the adjusted R^2 for these variables are bad - not really a fit between the activities and the children directly served
summary(glm(Children.Served.Directly ~ ActivityCount + Leveraging, data=MergedData_shift))

##summary - children directly served not mondeled well linearly

Children_Served <- regsubsets( Children.Served.Directly ~  Grant.Amount + ActivityCount + Email + Phone_Call + Knowledge_Exchange
                               + Leveraging + as.integer(Leveraging_Amount) + Additional + Legal_Refer + Year.of.Funding.Number.Only
                               ,data=MergedData_shift,method='forward')

##summary - children directly served not mondeled well linearly

Outcome_Reg <- regsubsets( Outcome.Actual ~  Grant.Amount + ActivityCount + Email + Phone_Call + Knowledge_Exchange
                           + Leveraging + as.integer(Leveraging_Amount) + Additional + Legal_Refer + Year.of.Funding.Number.Only
                           ,data=MergedData_shift,method='forward')

## "figure margins too large"
# plot(Outcome_Reg,scale = "adjr2",main="adjr2")