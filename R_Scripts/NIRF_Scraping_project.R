#IMPORTING RELEVANT PACKAGES#

library(data.table)
library(pdftools)
library(tidyverse)
library(stringr)
library(xlsx)




directives <- as.vector(list.dirs())


for(j in 2:length(directives))
{
  #sETTING WD#
  
  setwd(directives[j])
  
  
  #cHECKING LIST OF FILES TO READ#
  tempPDFList <- (list.files(pattern = "\\.pdf$"))
  tempPDFList <- as.vector(tempPDFList)
  noOfPDFs <- length(tempPDFList)
  
  
  
  
  
  #PRIMING VARIABLES AND SETTING UP DATA TABLES
  colname <- c("Male", "Female", "Total", "InState", "OutState", "INTL", "EWS", "SCSTOBC", "FullStateReimb", "FullInstiReimb", "FullPRivReimb", "NoReimb")
  DT <- data.table(levels=1:(9*noOfPDFs),University=as.character(1:(9*noOfPDFs)), ProgrammeTypes=as.character(1:(9*noOfPDFs)))
  studentnumbers<- data.table(levels=1:(9*noOfPDFs))
  studentnumbers[, (colname) := 0]
  filenametable <- data.table(levels=1:(9*noOfPDFs), filename=as.character(1:(9*noOfPDFs)))
  finaltable <- merge(DT, studentnumbers)
  finaltable <- merge(finaltable,filenametable)
  
  
  
  #THIS IS WHERE THE FOR LOOP sHOULD Go#
  
  
  
  
  for(i in 1:noOfPDFs)
  {
    
    a <- seq.int(9*i-8,9*i)
      
    
    
    
    rawtext <- pdf_text(tempPDFList[i])
    rawtext <- str_squish(rawtext)
    rawtext <- paste(rawtext, sep = '', collapse='')
    
    univname <- str_match(rawtext, "Institute Name: (.*?) \\[IR")
    univname <- univname[,2]
    univname <- as.vector(univname)
    
    relelvantdata <- str_match(rawtext, "Total Actual Student Strength(.*?)Higher")[,2]
    
    

    
    ug1 <- str_match(relelvantdata, "UG \\[1 (.*?)Program\\(s\\)\\]")
    ug1 <- ug1[1,2]
    ug1 <-gsub("[[:punct:]+[:alpha:]]","", ug1) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()  
    
    ug2 <- str_match(relelvantdata, "UG \\[2 (.*?)Program\\(s\\)\\]")
    ug2 <- ug2[1,2]
    ug2 <-gsub("[[:punct:]+[:alpha:]]","", ug2) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    ug3 <- str_match(relelvantdata, "UG \\[3 (.*?)Program\\(s\\)\\]")
    ug3 <- ug3[1,2]
    ug3 <-gsub("[[:punct:]+[:alpha:]]","", ug3) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    
    ug4 <- str_match(relelvantdata, "UG \\[4 (.*?)Program\\(s\\)\\]")
    ug4 <- ug4[1,2]
    ug4 <-gsub("[[:punct:]+[:alpha:]]","", ug4) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    ug5 <- str_match(relelvantdata, "UG \\[5 (.*?)Program\\(s\\)\\]")
    ug5 <- ug5[1,2]
    ug5 <-gsub("[[:punct:]+[:alpha:]]","", ug5) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    
    
    pg1 <- str_match(relelvantdata, "PG \\[1 (.*?)Program\\(s\\)\\]")
    pg1 <- pg1[1,2]
    pg1 <-gsub("[[:punct:]+[:alpha:]]","", pg1) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    pg2 <- str_match(relelvantdata, "PG \\[2 (.*?)Program\\(s\\)\\]")
    pg2 <- pg2[1,2]
    pg2 <-gsub("[[:punct:]+[:alpha:]]","", pg2) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
    pg3 <- str_match(relelvantdata, "PG \\[3 (.*?)Program\\(s\\)\\]")
    pg3 <- pg3[1,2]
    pg3 <-gsub("[[:punct:]+[:alpha:]]","", pg3) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
  
    pgi <- str_match(relelvantdata, "PG-Integrated(.*?)Placement")
    pgi <- pgi[1,2]
    pgi <-gsub("[[:punct:]+[:alpha:]]","", pgi) %>% trimws() %>% unlist() %>% str_split(pattern = " ") %>% unlist() %>% as.numeric() %>% as.vector()
    
  
  
    

    sm <- rbind(ug1,ug2,ug3,ug4,ug5,pg1,pg2,pg3,pgi)

    row.names(sm)
        
    finaltable[a, `:=`(University=univname,
                       ProgrammeTypes = row.names(sm),
                       Male=sm[,1], 
                       Female=sm[,2],
                       Total=sm[,3],
                       InState=sm[,4],
                       OutState=sm[,5],
                       INTL=sm[,6],
                       EWS=sm[,7],
                       SCSTOBC=sm[,8],
                       FullStateReimb=sm[,9],
                       FullInstiReimb=sm[,10],
                       FullPRivReimb=sm[,11],
                       NoReimb=sm[,12],
                       filename=tempPDFList[i])
               ]
    
    
 
  }
  
  
  
  setkey(finaltable, University)
  write.xlsx(finaltable[complete.cases(finaltable[ , Male]),], str_glue("./{directives[j]}2.xlsx"))
  
  setwd('..')
}
