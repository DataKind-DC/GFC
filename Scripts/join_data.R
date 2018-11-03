


library(tidyverse)
library(magrittr)
library(openxlsx)
grants2000 = read.xlsx("../Raw Data (dont edit)/All grants data since 2000 September 20.xlsx")
oci_generator<-read.xlsx("../Raw Data (dont edit)/OCI_Generator for Datakind.xlsm", sheet = "GrantData", detectDates = TRUE)
activity = read.xlsx("../Raw Data (dont edit)/Data Export of Activity Inputs September 10.xlsx", detectDates = TRUE)
awards = read.xlsx("../Raw Data (dont edit)/Major Awards Spreadsheet.xlsx", sheet = "Awards_Data")
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

spread_activity_data= function(activity){
   activity%>%
     arrange(Done.Date)%>%
     group_by(Org.Name)%>%
     mutate(visit_num = row_number())%>%
    split(.$visit_num)->list
  
  df<-list[[1]]
  for(i in 2:length(list)){
    df<-df%>%left_join(list[[i]]%>%select(-visit_num), by = "Org.Name", suffix = c("", sprintf(".%i", i)))
  }
  names(df)[2:6]<-names(df)[2:6]%>%paste0(.,".1")

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

awards_data<-spread_awards_data(awards)
activity_data<-spread_activity_data(activity)

joined_data<-joinThenClean(grants2000, oci_generator)

joined_data%<>%left_join(activity_data, by = "Org.Name")%>%
               left_join(awards_data, by = c("Org.Name" = "Grantee.Organization"))

# missing_awards<-awards_data%>%filter(! Grantee.Organization %in% grants2000$Org.Name)
# missing_visits<-activity_data%>%filter(! Org.Name %in% grants2000$Org.Name)

write_csv(joined_data, path = "../New Data/joined_data.csv")


