library(tidyverse)
library(dplyr)
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
                        "Major Awards Spreadsheet Normalized.xlsx",
                        sep=""),
                  sheet = "Awards_Data")
fnPBS<-function(x)  {
  return(grepl("New Heroes",x,ignore.case=T) & grepl("PBS", x,ignore.case=T))
}
pbsLarger<-Filter(fnPBS, awards$Award.Name) %>% unique()
pbsLarger<-pbsLarger[1]

topTierAwards<-read.xlsx(paste(DATA_DIR,
                               "Top Awards.xlsx",
                               sep=""))
pbsTop<-Filter(fnPBS, topTierAwards %>% unlist())
pbsTop<-pbsTop[1]
topTierAwards<-topTierAwards %>% unlist() %>% trimws() %>% tolower()

awards$Award.Name<-gsub(pbsLarger, pbsTop, awards$Award.Name)

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
  # bypassing plyr::mapvalues; plyr can complicate row_numbers()
  keyValReplace<-data.frame(
    exportNames=c("E-mail","Grantee-Led Convening Participation",
                  "Site Visit", "Additional Touch"),
    rankNames=c("Email","Grantee Partner-Led Convening Participation",
                "Monitoring Site Visit",
                "Meeting that is not a site visit or at a KE")
  )
  stopifnot(!any(is.na(keyValReplace)))
  eventsUpdated<-dfActivity$Capacity.Building.Type
  for(k in seq(nrow(keyValReplace)))  {
    eventsUpdated<-gsub(keyValReplace$exportNames[k],
                        keyValReplace$rankNames[k],
                        eventsUpdated)
  }
  dfActivity$Capacity.Building.Type<-eventsUpdated
  dfActivity<-full_join(dfActivity,
                        rankActivities %>% select(-Rank),
                        by=c("Capacity.Building.Type"=
                               "GFC.input"))
  # assign any activity labels that do not appear in
  # Ranking of GFC inputs.xlsx to a value of 1
  if(is.na(dfActivity$activityWeight) %>% any)  {
    idxNA<-which(is.na(dfActivity$activityWeight))
    dfActivity$activityWeight[idxNA]<-1
  }
  
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
  df<-df %>% select(-visit_num)
  for(i in 2:length(listByVisitNum)){
    df<-df%>%
      left_join(listByVisitNum[[i]]%>%
                  select(-c(visit_num)),
                by = "Org.Name",
                suffix = c("", sprintf(".%i", i)))
  }
  idxLower<-which(names(df)=="Org.Name")
  idxUpper<-which(names(df)=="Capacity.Building.Type.2")
  namesFirst<-names(df)[seq(idxLower+1, idxUpper-1)]
  names(df)[seq(idxLower+1, idxUpper-1)]<-namesFirst%>%paste0(.,".1")
  return(df)
}

spread_awards_data= function(dfAwards){
  dfAwards%>%
    arrange(Awarded.By)%>%
    group_by(Grantee.Organization)%>%
    mutate(award_num = row_number())%>%
    split(.$award_num)->list
  
  df<-list[[1]]%>%select(-award_num)
  for(i in 2:length(list)){
    df<-df%>%left_join(list[[i]]%>%select(-award_num),
                       by = "Grantee.Organization",
                       suffix = c("", sprintf(".%i", i)))
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

## Process top award counts
countFromTopTier<-awards %>%
  mutate(modName = Award.Name %>% trimws() %>% tolower()) %>%
  mutate(isTopAward = modName %in% topTierAwards) %>%
  group_by(Grantee.Organization) %>%
  dplyr::summarise(TopAwards = sum(isTopAward),
                   AllAwards = n())
allOrgs<-joined_data$Org.Name
unAwarded<-subset(allOrgs, !is.na(allOrgs) &
       !(allOrgs %in% countFromTopTier$Grantee.Organization)) %>%
  unique()
dfUnawarded<-data.frame(unAwarded, top=0, all=0)
names(dfUnawarded)<-names(countFromTopTier)
allOrgsCounts<-rbind(countFromTopTier, dfUnawarded)

stopifnot(all(allOrgsCounts$TopAwards <= allOrgsCounts$AllAwards &
                allOrgsCounts$TopAwards >= 0))
write_csv(joined_data, path = "../New Data/award_counts_by_org.csv")