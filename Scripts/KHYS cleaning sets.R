# setwd("C:/HY/DS Meetups/DataKind/GFC")
library(openxlsx); library(tidyverse); library(dplyr)
grants2000<-read.xlsx(paste("Raw Data (dont edit)/",
                            "All grants data since 2000 September 20.xlsx",
                            sep=""),
                      detectDates = T)
oci<-read.xlsx("Raw Data (dont edit)/OCI_Generator for Datakind.xlsm",
               sheet="GrantData",
               detectDates = T)
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
# cleaned<-joinThenClean(grants2000, oci)