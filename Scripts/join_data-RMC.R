#install.packages("openxlsx")
#install.packages("lubridate")
install.packages("zoo")
install.packages("GGally")
install.packages("rpart.plot")
install.packages("partykit")
install.packages("rattle")
install.packages("tidyverse")
install.packages('openxlsx')
install.packages('leaps')
install.packages('lubridate')

install.packages('rpart.plot',repos='http://cran.us.r-project.org')

## TODO: fix data type issue with Pending Approved Date
library(tidyverse)
library(dplyr)
library(magrittr)
library(openxlsx)
library(lubridate)
library(zoo)
library(rvest)
library(BAMMtools)
library(GGally)
library(leaps)
library(rpart)
library(partykit)
library(rattle)

options(scipen=999)#Disables scientific notation


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

sustainability<-read.xlsx(paste(DATA_DIR,
                        "Exit Impact Overview Answers Spreadsheet.xlsx",
                        sep=""),
                  sheet = "Search Results")

##Adjust Weighting for Activities. Change number after each activity type to adjust how it is weighted.
rankActivities$ActivityWeight[rankActivities$GFC.input=="Knowledge Exchange Participation"]<-15
rankActivities$ActivityWeight[rankActivities$GFC.input=="Grantee-Led Convening Participation"]<-15
rankActivities$ActivityWeight[rankActivities$GFC.input=="Leveraging"]<-5
rankActivities$ActivityWeight[rankActivities$GFC.input=="Monitoring Site Visit"]<-10
rankActivities$ActivityWeight[rankActivities$GFC.input=="Site Visit"]<-10
rankActivities$ActivityWeight[rankActivities$GFC.input=="Meeting that is not a site visit or at a KE"]<-5
rankActivities$ActivityWeight[rankActivities$GFC.input=="Phone Call"]<-2
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
  mutate(Id=paste(Org.Name,Year+1,sep=''),TotalWeightedActivity=replace_na(TotalWeightedActivity,0))

OrgActivityTotal<-OrgActivity %>%
  group_by(Org.Name) %>%
  summarise(TotalActivity=sum(TotalWeightedActivity))

##Remove unnecessary columns
oci<-oci[,1:57]

##Make Id in OCI data to correspond with Id in activity data created above
oci$Id<-paste(oci$Org.Name,oci$Fiscal.Year,sep='')


##Add delta and cumulative change columns
master<-left_join(oci,OrgActivity, by=c("Id")) %>%
  filter(Year.of.Funding.Number.Only!=0 )%>% 
  arrange(Org.Name.x,Year.of.Funding.Number.Only) %>%
  group_by(Org.Name.x) %>%
  mutate(deltaOCI=`OCI.-.Average`-first(`OCI.-.Average`),
         deltaExpenses=(Total.annual.expenses-first(Total.annual.expenses))/first(Total.annual.expenses),
         #otherBudget=Total.annual.expenses-Grant.Amount,
         deltaBudget=(Total.annual.expenses-first(Total.annual.expenses))/first(Total.annual.expenses),
         cumulActivity=cumsum(TotalWeightedActivity),
         OCIaverage=`OCI.-.Average`,
         cumulGrantAmount=cumsum(Grant.Amount),
         percentoutcome=Outcome.Actual/Outcome.Target,
         ChildrenServed=cumsum(Children.Served.Directly),
         Budget0=first(Total.annual.expenses),
         OCI0=first(OCIaverage)
         )


  

##Add budget and OCI classes based on first year, using jenks breaks to maximiza difference
BudgetBreaks<-getJenksBreaks(master$Budget0,11)
OCIBreaks<-tail(getJenksBreaks(master$OCI0,12),11)
master$BudgetClass<-cut(master$Budget0, breaks = BudgetBreaks, labels=as.character(1:10))
master$OCIClass<-cut(master$OCI0, breaks = OCIBreaks, labels=as.character(1:10))

##Supplemental Grants

supplemental<-grants2000 %>%
  group_by(Org.Name) %>%
  summarize(primaryGrants=sum(Grant.Amount[Grant.Type=="Primary Grant"]),
            developmentGrants=sum(Grant.Amount[Grant.Type=="Organizational Development Award"|Grant.Type=="z - Technology Grant"]),
            otherGrants=sum(Grant.Amount)-primaryGrants-developmentGrants,
            totalGrants=sum(Grant.Amount),
            numGrants=length(Request.ID),
            numNonPrimary=length(Request.ID[Grant.Type!="Primary Grant"])) %>%
  mutate(OnlyPrimary=ifelse((primaryGrants!=0 & developmentGrants==0 & otherGrants==0),1,0),
         PrimaryPercent=primaryGrants/(totalGrants))


masterorgs<-master %>%
  group_by(Org.Name.x) %>%
  mutate(Portfolio=na.locf(Portfolio, na.rm=FALSE), 
         Region=na.locf(Region, na.rm=FALSE),
         Country=na.locf(Country, na.rm=FALSE))%>%
  summarize(Region=last(Region),
            Portfolio=last(Portfolio),
            Country=last(Country),
            YearsFunded=last(Year.of.Funding.Number.Only),
            Budget0=first(Total.annual.expenses),
            BudgetClass=as.numeric(first(BudgetClass)),
            BudgetChange=last(Total.annual.expenses)-first(Total.annual.expenses),
            BudgetChangePercent=last(Total.annual.expenses)/first(Total.annual.expenses),
            OCIfirst=first(OCIaverage),
            OCIClass=as.numeric(first(OCIClass)),
            OCIlast=last(OCIaverage),
            OCIchange=last(OCIaverage)-first(OCIaverage),
            OCIchangetarget=ifelse(OCIchange>=.5,1,0),
            OCItarget=ifelse(OCIlast>=3.5,1,0),
            lastOutcome=last(percentoutcome),
            Outcometarget=ifelse(lastOutcome>=1,1,0),
            BudgetTarget=ifelse(BudgetChangePercent>=1.5,1,0),
            served=sum(Children.Served.Directly)
            )%>%
  left_join(OrgActivityTotal, by=c("Org.Name.x"="Org.Name"))%>%
  left_join(supplemental,by=c("Org.Name.x"="Org.Name")) %>%
  mutate(OCIper10k=OCIchange/(totalGrants)*10000,
         GrantsPerYear=numGrants/YearsFunded) %>%
  left_join(sustainability,by=c("Org.Name.x"="Name"))%>%
  filter(primaryGrants!=0)%>%
  filter(!is.na(totalGrants))%>%
  filter(totalGrants!=0)%>%
  filter(Budget0!=0)



setwd(WRITE_DIR)
write.csv(master,"master.csv", row.names = FALSE)
write.csv(masterorgs,"masterorgs.csv", row.names = FALSE)


##Calc by All Groups
GroupTypes<-c("Country.x","Region","Portfolio","OCIClass","BudgetClass")

for (i in GroupTypes) {
Byx<-masterorgs %>%
  filter(primaryGrants!=0|Budget0!=0)%>%
  filter(!is.na(totalGrants))%>%
  group_by(get(i))%>%
  summarize(orgs=n_distinct(Org.Name.x),
            AveTotalAmountperOrg=sum(totalGrants)/orgs,
            OrgActivityTotalAve=mean(OrgActivityTotal),
            GrantsPerYearAve=mean(GrantsPerYear),
            percentOnlyPrimary=length(OnlyPrimary[OnlyPrimary==1])/orgs,
            OCIAve=mean(OCIchange),
            BudgetChangePercentAve=mean(BudgetChangePercent),
            lastOutcomeAve=mean(Outcometarget),
            percentOutcometarget=length(Outcometarget[Outcometarget==1])/orgs,
            percentOOCIchangetarget=length(OCIchangetarget[OCIchangetarget==1])/orgs,
            percentOOCItarget=length(OCItarget[OCItarget==1])/orgs,
            percentBudgetTarget=length(BudgetTarget[BudgetTarget==1])/orgs,
            OCIchangeper10k=sum(OCIchange)/sum(totalGrants)*10000) %>%
  mutate(type=paste(i))
colnames(Byx)[1]<-c("Name")
Byx$Name<-as.character(Byx$Name)
assign(paste(i),Byx)

}
BudgetBreaks<-as.data.frame(BudgetBreaks)
OCIBreaks<-as.data.frame(OCIBreaks)
ByGroup<-bind_rows(Country.x,Region,Portfolio,OCIClass,BudgetClass)
setwd(WRITE_DIR)
write.csv(ByGroup,"ByGroup.csv", row.names = FALSE)


masterorgs$served[masterorgs$served>2500]<-25000
masterorgs$BudgetChangePercent[masterorgs$BudgetChangePercent>10]<-10
##Correlations

ggally<-ggpairs(masterorgs[c(7,11,20,24,9,12,13,17,19)],lower=list(continuous=wrap("smooth_lm",alpha=.5,color="#f8766d")),
                diag=list(continuous="bar",fill="#6c8cc7"), 
                upper=list(axisLabels='show'))+plottheme

ggsave("Correlations.pdf", plot = ggally, device = NULL, path = NULL,scale = 1, height = 11,width=17)
embed_fonts("Correlations.pdf", outfile="Correlations.pdf")


##DecsionTrees

OCIchange = rpart(OCIchangetarget ~ totalGrants+Budget0+OCIfirst+GrantsPerYear+developmentGrants+Region+Portfolio, data=masterorgs, method="class",minbucket=16)
summary(OCIchange)
fancyRpartPlot(OCIchange)


OCIfinal = rpart(OCItarget ~ totalGrants+Budget0+GrantsPerYear+developmentGrants+Region+Portfolio, data=masterorgs, method="class",minbucket=20)
summary(OCIfinal)
fancyRpartPlot(OCIfinal)

budgetTarget = rpart(BudgetTarget ~ Budget0+OCIfirst+developmentGrants+Region+Portfolio, data=masterorgs, method="class",minbucket=23)
summary(budgetTarget)
fancyRpartPlot(budgetTarget)

lastOutcome = rpart(Outcometarget ~ totalGrants+Budget0+OCIfirst+GrantsPerYear+developmentGrants+Region+Portfolio,  data=masterorgs, method="class",minbucket=20)
summary(lastOutcome)
fancyRpartPlot(lastOutcome)





##Plots
setwd("C:/Users/rcarder/Documents/Dev/GFC/plots")


plottheme<-  theme(
  text = element_text(size=9, family='Montserrat Light',color='#000000' ),
  axis.text = element_text(size=7, family='Montserrat',color='#000000'),
  strip.text = element_text(size=10, family='Montserrat SemiBold',color='#6c8cc7'),
  axis.title = element_text(size=9, family='Montserrat',color='#000000'),
  panel.background = element_blank(),
  panel.border = element_blank(),
  legend.position = "none",
  panel.grid.major = element_line(color = "#C0C0C0", linetype = "dotted", size = .15),
  #panel.grid.minor = element_blank(),
  #panel.grid.major.x = element_blank(),
  #panel.grid.minor.x = element_blank(),
  strip.background=element_blank())


classcorrelation<-ggplot(master, aes(x = OCIClass, y = BudgetClass)) + 
  geom_point(alpha=.0075,size=8, color="Red") +
  plottheme

ggally<-ggpairs(master[,61:72], lower= list(continuous = "smooth"))


##Overall Plot


mastermean<-master %>%
  group_by(Year.of.Funding.Number.Only) %>%
  summarise(meanOCI=mean(OCIaverage))
  
ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
  geom_line(alpha=.05,size=1)+
  geom_line(data=mastermean, aes(x=Year.of.Funding.Number.Only,y=meanOCI,group=FALSE), size=1,color="Red")+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average") +
  plottheme


ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=deltaOCI, group=Organization.Name)) + 
  geom_line(alpha=.05,size=1)+#facet_wrap(~as.factor(master$Region[master$OCIaverage!=0]))+
  geom_line(data=mastermean, aes(x=Year.of.Funding.Number.Only,y=meandeltaOCI,group=FALSE), size=1,color="Red")+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("Change in OCI Average from First Year of Funding") +
  plottheme

##OCI by Region





OCIAverageRegion<-ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
  geom_line(alpha=.1,size=.3)+facet_wrap(~as.factor(master$Region[master$OCIaverage!=0]),ncol=2)+
  stat_summary(aes(group=Region), fun.y=mean, geom="line", size=1,color="Red")+
  stat_summary(aes(label=round(..y..,1),group=Region), fun.y=mean, geom="text", size=2, color="Black",vjust=-1,family='Montserrat')+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average by Region") +
  plottheme
ggsave("OCIAveragebyRegion.pdf", plot = OCIAverageRegion, device = NULL, path = NULL,scale = 1, height = 10,width=8)
embed_fonts("OCIAveragebyRegion.pdf", outfile="OCIAveragebyRegion.pdf")



PlotGroupTypes<-c("Region","Portfolio","OCIClass","BudgetClass")

for (i in PlotGroupTypes) {
  
  
  
  OCIAveragePortfolio<-ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
    geom_line(alpha=.05,size=.3)+facet_wrap(~as.factor(master$i[master$OCIaverage!=0]),ncol=2)+
    stat_summary(aes_string(group=i), fun.y=mean, geom="line", size=1,color="Red")+
    stat_summary(aes(label=round(..y..,1),group=Portfolio), fun.y=mean, geom="text", size=2, color="Black",vjust=-1,family='Montserrat')+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
    xlab("Year of Funding")+ ylab("OCI Average")+ggtitle(paste("OCI Average by",i,sep=' ')) +
    plottheme
  ggsave("OCIAveragebyPortfolio.pdf", plot = OCIAveragePortfolio, device = NULL, path = NULL,scale = 1, height = 10,width=8)
  embed_fonts("OCIAveragebyPortfolio.pdf", outfile="OCIAveragebyPortfolio.pdf")
  
  
  
  
  
}




##OCI by Portfolio
OCIAveragePortfolio<-ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
  geom_line(alpha=.05,size=.3)+facet_wrap(~as.factor(master$Portfolio[master$OCIaverage!=0]),ncol=2)+
  stat_summary(aes(group=Portfolio), fun.y=mean, geom="line", size=1,color="Red")+
  stat_summary(aes(label=round(..y..,1),group=Portfolio), fun.y=mean, geom="text", size=2, color="Black",vjust=-1,family='Montserrat')+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average by Portfolio") +
  plottheme
ggsave("OCIAveragebyPortfolio.pdf", plot = OCIAveragePortfolio, device = NULL, path = NULL,scale = 1, height = 10,width=8)
embed_fonts("OCIAveragebyPortfolio.pdf", outfile="OCIAveragebyPortfolio.pdf")


##OCI by Budget Class
OCIAverageBudget<-ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
  geom_line(alpha=.1,size=.3)+facet_wrap(~as.factor(master$budgetClass[master$OCIaverage!=0]),ncol=2)+
  stat_summary(aes(group=budgetClass), fun.y=mean, geom="line", size=1,color="Red")+
  stat_summary(aes(label=round(..y..,1),group=budgetClass), fun.y=mean, geom="text", size=2, color="Black",vjust=-1,family='Montserrat')+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average by Budget Class") +
  plottheme
ggsave("OCIAveragebyPortfolio.pdf", plot = OCIAveragePortfolio, device = NULL, path = NULL,scale = 1, height = 10,width=8)
embed_fonts("OCIAveragebyPortfolio.pdf", outfile="OCIAveragebyPortfolio.pdf")

##OCI by OCI Class
OCIAverageBudget<-ggplot(master[master$OCIaverage!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name)) + 
  geom_line(alpha=.1,size=.3)+facet_wrap(~as.factor(master$OCIClass[master$OCIaverage!=0]),ncol=2)+
  stat_summary(aes(group=OCIClass), fun.y=mean, geom="line", size=1,color="Red")+
  stat_summary(aes(label=round(..y..,1),group=OCIClass), fun.y=mean, geom="text", size=2, color="Black",vjust=-1,family='Montserrat')+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average by Budget Class") +
  plottheme
ggsave("OCIAveragebyPortfolio.pdf", plot = OCIAveragePortfolio, device = NULL, path = NULL,scale = 1, height = 10,width=8)
embed_fonts("OCIAveragebyPortfolio.pdf", outfile="OCIAveragebyPortfolio.pdf")





ggplot(mastermelt[mastermelt$Score!=0,], aes(x=Year.of.Funding.Number.Only, y=OCIaverage, group=Organization.Name))+
  geom_line(alpha=.05,size=1) + facet_wrap(~OCItype)+
  geom_line(data=mastermean, aes(x=Year.of.Funding.Number.Only,y=meanOCI,group=FALSE), size=1,color="Red")+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))+
  xlab("Year of Funding")+ ylab("OCI Average")+ggtitle("OCI Average") +
  plottheme



ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


ggplotRegression(lm(OCIaverage ~ , data = master))
ggplotRegression(lm(OCIaverage ~ Year.of.Funding.Number.Only, data = master))
ggplotRegression(lm(deltaOCI ~ cumActivity, data = master))
ggplotRegression(lm(deltaOCI ~ Year.of.Funding.Number.Only, data = master))

ggplotRegression(lm(OCIaverage ~ cumActivity, data = master))
ggplotRegression(lm(OCIaverage ~ Year.of.Funding.Number.Only, data = master))
ggplotRegression(lm(deltaOCI ~ cumActivity, data = master))
ggplotRegression(lm(deltaOCI ~ Year.of.Funding.Number.Only, data = master))

setwd(WRITE_DIR)
write.csv(master,"master.csv", row.names = FALSE)



























NEW_DATA_DIR<-"C:/Users/rcarder/Documents/Dev/GFC/New Data/"

MyData<-master


names(MyData)

##this simplifies the OCI data set, focusing on variables of interest

## Includes outputs: Children Directly Served, OCI data, Outcome Actual, Outcome Target
## Includes inputs: Grant Amount, Grant Type, 
##Add weighted activity
## Includes characteristics: "Region, Country, Org name, Unique ID

## Needs calculation outputs: Budget Increase

## Needs Join inputs: phone calls, emails, additional touchpoints, legal referrals

## Dropped from consideration for now: "Awards Data" (requires join), "Grant use" (NLP complexity) 

OCI_subset <- MyData[c(1:14,20,29,49,50,51,52)]

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

ActivityData <- sqldf("SELECT Org_Name , Fiscal_Year FROM MyData2 GROUP BY Org_Name, Fiscal_Year")
head(ActivityData)
##Create site visit column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, b.Site_Visit from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Site_Visit' FROM MyData2 WHERE Capacity_Building_Type = 'Site Visit' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# ##Create Knowledge Exchange Column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, b.Knowledge_Exchange from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Knowledge_Exchange' FROM MyData2 WHERE Capacity_Building_Type = 'Knowledge Exchange Participation' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# ##Create Leveraging Column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , b.Leveraging  from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, Count(*) AS 'Leveraging' FROM MyData2 WHERE Capacity_Building_Type = 'Leveraging' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# 
# ##Total Leveraging amount for year - represented in thousands
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, b.Leveraging_Amount  from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, Sum(Leveraging_Amount) AS 'Leveraging_Amount' FROM MyData2 WHERE Capacity_Building_Type = 'Leveraging' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# ##Create Phone Call Column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, b.Phone_Call from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, count(*) AS 'Phone_Call' FROM MyData2 WHERE Capacity_Building_Type = 'Phone Call' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# 
# ##Create email column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, b.Email from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, count(*) AS 'Email' FROM MyData2 WHERE Capacity_Building_Type = 'E-mail' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
# ##Create "aditional" column
# ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, a.Email, b.Additional from ActivityData AS a LEFT JOIN 
#                       (SELECT Org_Name, Fiscal_Year, count(*) AS 'Additional' FROM MyData2 WHERE Capacity_Building_Type = 'Additional Touch' GROUP BY Org_Name, Fiscal_Year) AS b
#                       ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")
##Create "Legal Referral" column
ActivityData <- sqldf("SELECT a.Org_Name, a.Fiscal_Year, a.Site_Visit, a.Knowledge_Exchange , a.Leveraging, a.Leveraging_Amount, a.Phone_Call, a.Email, a.Additional,b.Legal_Refer from ActivityData AS a LEFT JOIN 
                      (SELECT Org_Name, Fiscal_Year, count(*) AS 'Legal_Refer' FROM MyData2 WHERE Capacity_Building_Type = 'Legal Referral' GROUP BY Org_Name, Fiscal_Year) AS b
                      ON  b.Org_Name = a.Org_Name  AND b.Fiscal_Year = a.Fiscal_Year")

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
names(OCI_subset)
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

# MergedData_noshift$ActivityCount <- with(MergedData_noshift,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#                                          ifelse(is.na(Site_Visit),0,Site_Visit) 
#                                          + ifelse(is.na(Knowledge_Exchange),0,Knowledge_Exchange) 
#                                          + ifelse(is.na(Leveraging),0,Leveraging)
#                                          + ifelse(is.na(Phone_Call),0,Phone_Call)
#                                          + ifelse(is.na(Email),0,Email)
#                                          + ifelse(is.na(Additional),0,Additional)
#                                          + ifelse(is.na(Legal_Refer),0,Legal_Refer))

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

plot(Children_Served,scale = "adjr2",main="adjr2")
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

plot(Outcome_Reg,scale = "adjr2",main="adjr2")







###For Each Program

####MAP OF ALL PAYMENTS



#!!!!!PAYMENTS!!!!!!!

countryapproved<-payments[payments$Program!="IFT",]
sum(countryapproved$SplitPaymentAmount)




programgrouped<-countryapproved %>%
  filter(Statuses_Status.x=="Approved") %>%
  group_by(Year,Geography_Description,A, Category, AddressCat, AddressRegion,Program, Theme) %>%
  summarize(Amount=sum(SplitPaymentAmount),
            TotalGrants=n())

write.csv(programgrouped,"programgrouped.csv")

US2<-filter(programgrouped, grepl('USA Domestic', Geography_ParentPath))
US2<-US2[US2$Geography_Description!="US General",]
US2$Geography_Description<-"US General"

US1<-programgrouped[programgrouped$Geography_Description=="US General"|
                      programgrouped$Geography_Description=="International General"|
                      programgrouped$Geography_Description=="Africa General"|
                      programgrouped$Geography_Description=="Latin America General"|
                      programgrouped$Geography_Description=="Europe General"|
                      programgrouped$Geography_Description=="Asia General"|
                      programgrouped$Geography_Description=="Middle East General"|
                      programgrouped$Geography_Description=="Australia & Pacific"|
                      programgrouped$Geography_Description=="SoCal"|
                      programgrouped$Geography_Description=="Eastern Europe General"|
                      programgrouped$Geography_Description=="Caribbean General",]




##Remove generals
programgrouped<-filter(programgrouped, !grepl('USA Domestic', A))


programgrouped<-programgrouped[programgrouped$Geography_Description!="US General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="International General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Africa General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Latin America General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Europe General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Middle East General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Asia General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Eastern Europe General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Southeast General",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Australia & Pacific",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="SoCal",]
programgrouped<-programgrouped[programgrouped$Geography_Description!="Caribbean General",]
programgrouped$Geography_Description[programgrouped$Geography_Description=="D.R. of Congo"]<-"Democratic Republic of the Congo"
programgrouped$Geography_Description[programgrouped$Geography_Description=="Burma"]<-"Myanmar"
programgrouped$label<-paste(programgrouped$Geography_Description," - ",currency(programgrouped$Amount, digits = 0L),sep='')



programgrouped<-programgrouped %>%
  filter(Year==2015|Year==2016|Year==2017|Year==2018|Year==2019) %>%
  filter(Program=="SOGI")%>%
  group_by(Geography_Description, Theme)%>%
  summarise(Amount=sum(Amount))

###Quantile Breaks
no_classes <- 9
labels <- c()



###Manual Breaks

pretty_breaks <- c(50000,100000,250000,500000,750000,1000000,2500000,5000000)
# find the extremes
minVal <- min(programgrouped$Amount, na.rm = T)
maxVal <- max(programgrouped$Amount, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
programgrouped$brks <- cut(programgrouped$Amount, 
                           breaks = brks, 
                           include.lowest = TRUE, 
                           labels = labels)

brks_scale <- levels(programgrouped$brks)
labels_scale <- rev(brks_scale)

##





programgrouped$brks <- cut(programgrouped$Amount, 
                           breaks = brks, 
                           labels = labels, 
                           include.lowest = T)


#See Regions
#Regions$Description[is.na(Regions$RegionType)]


##Make Map
world <- map_data("world") %>% filter(region!="Antarctica")
world<-fortify(world)

map_data_fortified <- fortify(world) 
# now we join the thematic data
map_data <- map_data_fortified %>% left_join(programgrouped, by = c("region" = "Geography_Description"))

labels<-map_data%>%
  group_by(region)%>%
  summarise(lat=mean(lat),lon=mean(long))

labels<-programgrouped %>% left_join(labels, by= c("Geography_Description"="region"))


theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}




for (i in unique(programgrouped$Theme)){
  dat<-programgrouped[programgrouped$Theme==i,]
  
  
  
  
  # now we join the thematic data
  mapdata <- map_data_fortified %>% left_join(dat, by = c("region" = "Geography_Description"))
  
  programgrouped$label<-paste(programgrouped$Geography_Description," - ",currency(programgrouped$Amount, digits = 0L),sep='')
  
  
  
  labs<-mapdata%>%
    group_by(region)%>%
    summarise(lat=mean(lat),lon=mean(long))
  
  labs<-dat %>% left_join(labs, by= c("Geography_Description"="region"))
  
  
  labs$mil<-paste(format(round(labs$Amount / 1e6, 2), trim = TRUE), "M", sep='')
  labs$per<-paste(format(round(labs$percent * 100, 1), trim = TRUE), "%", sep='')
  labs$label<-paste(labs$Geography_Description," - $",labs$mil,sep="")
  
  
  
  
  map<-ggplot() +
    geom_polygon(data = mapdata, aes(x=long, y = lat,fill=Amount, group = group),  color="#A9A9A9", size=0.1) +
    scale_fill_distiller(palette =  "OrRd", direction = 1,limits=c(minVal,maxVal+50000), na.value = "#A9A9A9", name = "",breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000),labels=c("$500k","$1M","$1.5M","$1.75M","$2M","$2.5M","3M"))+
    
    geom_label_repel(data=labs, aes(x=lon, y=lat, label= label), size=2.5, family="Montserrat",color = 'white',fill=rgb(0,0,0,1),
                     box.padding = 0.2, point.padding = 0,
                     label.padding = 0.1,
                     segment.size = .15,
                     segment.alpha = .7,
                     label.size =NA,
                     segment.color = 'grey50')+
    ggtitle("") +
    guides(fill=guide_colorbar(barheight = 5))+
    theme_map()+
    theme(
      legend.position = c(0.1, 0.25),
      text = element_text(color = "#000000",family="Montserrat"),
      plot.background = element_blank(),
      #  legend.position="bottom",
      legend.text = element_text(size=10),
      legend.box = "vertical",
      legend.box.margin =  margin(0,0,0,0),
      legend.spacing.y = unit(0,'cm')
      
    )
  
  ggsave(paste(i,"HeatMap.pdf",sep=''), plot = map, device = NULL, path = NULL,
         scale = 1, width = 13, height = 7)
  embed_fonts(paste(i,"HeatMap.pdf",sep=''), outfile=paste(i,"HeatMap.pdf",sep=''))
  
}






