library(tidyverse)
library(dplyr)
library(plyr) # for mapvalues
library(magrittr)
library(openxlsx)
library(lubridate)
DATA_DIR<-"../Raw Data (dont edit)/"
grants2000<-read.xlsx(paste(DATA_DIR,
                            "All grants data since 2000 September 20.xlsx",
                            sep=""),
                      detectDates = T)
oci_generator<-read.xlsx(paste(DATA_DIR,
                               "OCI_Generator for Datakind.xlsm",
                               sep=""),
                         sheet = "GrantData", detectDates = T)
activity<-read.xlsx(paste(DATA_DIR,
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
joinThenClean<-function(df1, df2) {
  # df1 is read from All grants data since 2000 September 20.xlsx
  # df2 is read from the GrantData/2nd sheet of OCI_Generator for Datakind.xlsm
  firstJoin<-full_join(df1, df2, by=c("Request.ID"="Request.ID"))
  
  digitCols<-apply(firstJoin, MARGIN=2, function(x)
    any(grepl("[0-9,\\$]+,[0-9]{3}$", x)) &
      !any(grepl("^[A-z]", x))
  )
  idxDigitCols<-which(digitCols)
  dfDigitCols<-firstJoin[,idxDigitCols]
  firstJoin[,idxDigitCols]<-apply(dfDigitCols, MARGIN=2,
                                  function(x) gsub(",","", x) %>%
                                    trimws() %>%
                                    as.numeric())
  duplicateNames<-Filter(function(x) grepl("\\.[xy]{1}", x), names(firstJoin))
  duplicatePairs<-gsub("\\.[xy]{1}", "", duplicateNames) %>% unique()
  colDiffs<-function(colName) {
    stopifnot(colName %in% duplicatePairs)
    dfX<-firstJoin[paste(colName, ".x", sep="")] %>% unlist()
    if(is.character(dfX)) {
      dfX<-trimws(dfX)
    }
    dfY<-firstJoin[paste(colName, ".y", sep="")] %>% unlist()
    if(is.character(dfY)) {
      dfY<-trimws(dfY)
    }
    
    idxDisagree<-which(dfX != dfY)
    idxFillFromY<-which(is.na(dfX) & !is.na(dfY))
    idxBothNA<-which((is.na(dfX) | dfX=="") & (is.na(dfY) | dfY==""))
    final<-dfX
    final[idxDisagree]<-NA
    final[idxFillFromY]<-dfY[idxFillFromY]
    final[idxBothNA]<-NA
    vDisagree<-rep(F, length(dfX))
    vDisagree[idxDisagree]<-T
    # the resulting data frame has a vector noting where the input for df1 and
    # that for df2 disagree; the resolved data will be set to NA
    res<-data.frame(final=final, disagrees=vDisagree)
    names(res)<-paste(c("", "DISAGREES_"), colName, sep="")
    return(res)
  }
  lstDiffDf<-lapply(duplicatePairs, colDiffs)
  mergedDiffDf<-do.call(cbind, lstDiffDf)
  idxDiffCounts<-lapply(lstDiffDf, function(elt) sum(elt[,2])) %>% unlist()
  
  secondJoin<-cbind(firstJoin %>% select(-duplicateNames), mergedDiffDf)
  row.names(secondJoin)<-NULL
  orgCheck<-secondJoin %>%
    mutate(orgNameChars=nchar(Organization.Name))
  stopifnot(
    orgCheck %>%
      mutate(rawUniqueID=substr(Unique_ID,
                                orgNameChars+1,
                                nchar(Unique_ID))) %>%
      mutate(isValidUniqueID=is.na(rawUniqueID) | grepl("[0-9]+",
                                                        rawUniqueID)) %>%
      select(isValidUniqueID) %>%
      unlist() %>%
      all())
  
  secondJoin<-orgCheck %>%
    mutate(orgRequestID=ifelse(is.na(orgNameChars),
                               NA,
                               substr(Unique_ID,
                                      orgNameChars+1,
                                      nchar(Unique_ID)))) %>%
    mutate(orgRequestID=as.numeric(orgRequestID)) %>%
    select(-orgNameChars)
  return(secondJoin)
}

rankActivities<-rankActivities %>%
  mutate(GFC.input = trimws(GFC.input),
         activityWeight=rev(seq(nrow(rankActivities))))
rankActivities$activityWeight[nrow(rankActivities)]<-2

spread_activity_data= function(dfActivity){
  dfActivity$Capacity.Building.Type<-dfActivity$Capacity.Building.Type %>%
    mapvalues(c("E-mail", "Grantee-Led Convening Participation",
                "Site Visit", "Additional Touch"),
              c("Email", "Grantee Partner-Led Convening Participation",
                "Monitoring Site Visit",
                "Meeting that is not a site visit or at a KE"))
  dfActivity<-full_join(dfActivity,
                        rankActivities %>% select(-Rank),
                        by=c("Capacity.Building.Type"=
                               "GFC.input"))
  if(is.na(dfActivity$activityWeight) %>% any)
    dfActivity$activityWeight<-dfActivity$activityWeight %>% mapvalues(NA, 1)
  
  detach("package:plyr", unload=TRUE) # required or row_number will not work
  withVisitNums<-dfActivity %>%
    arrange(Done.Date) %>%
    group_by(Org.Name) %>%
    mutate(yearDone=lubridate::year(Done.Date),
           visit_num = row_number())
  
  dfAnnualPts<-withVisitNums %>%
    ungroup() %>%
    arrange(Org.Name, visit_num) %>%
    group_by(Org.Name, yearDone) %>%
    dplyr::summarise(annualActivityPts = sum(activityWeight))
    # required to specify dplyr since dplyr has a summarise, too
  withVisitNums<-full_join(withVisitNums, dfAnnualPts,
                           by=c("Org.Name", "yearDone"))
  withVisitNums<-withVisitNums %>%
    select(-c(activityWeight, yearDone))
  
  listByVisitNum<-withVisitNums %>% split(.$visit_num)
  df<-listByVisitNum[[1]]
  for(i in 2:length(listByVisitNum)){
    df<-df%>%
      left_join(listByVisitNum[[i]]%>%
                  select(-c(visit_num)),
                by = "Org.Name",
                suffix = c("", sprintf(".%i", i)))
  }
  names(df)[2:6]<-names(df)[2:6]%>%paste0(.,".1")
  df<-df %>%
    select(-visit_num)
  
  return(df)
}

spread_awards_data= function(awards){
  awards%>%
    arrange(Awarded.By)%>%
    group_by(Grantee.Organization)%>%
    mutate(award_num = row_number())%>%
    split(.$award_num)->list
  
  df<-list[[1]]%>%select(-award_num)
  for(i in 2:length(list)){
    df<-df%>%left_join(list[[i]]%>%select(-award_num), by = "Grantee.Organization", suffix = c("", sprintf(".%i", i)))
  }
  df%<>%select(Grantee.Organization, everything())
  names(df)[2:8]<-names(df)[2:8]%>%paste0(.,".1")
  
  return(df)
}

activity_data<-spread_activity_data(activity)
awards_data<-spread_awards_data(awards)

joined_data<-joinThenClean(grants2000, oci_generator)

joined_data%<>%left_join(activity_data, by = "Org.Name")%>%
  left_join(awards_data, by = c("Org.Name" = "Grantee.Organization"))

# missing_awards<-awards_data%>%filter(! Grantee.Organization %in% grants2000$Org.Name)
# missing_visits<-activity_data%>%filter(! Org.Name %in% grants2000$Org.Name)

write_csv(joined_data, path = "../New Data/joined_data.csv")