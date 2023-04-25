MSLOverallDataTest<- copy(overalldatatest)
MSLOverallDataTest[,SNO:=NULL]
MSLOverallDataTest[,SNO:=rep(1:10000, times=12)]
MSLOverallDataTest


myFunctionreduce <- compose(prod,unname,unlist,~lapply(.x,nlevels))
MSLfunction<- function(MSLloansize,MSLwidth, MSLinterest, MSLinterestwidth, MSLinflation, timetopay, salaryinc, salarybase, rcor, rcorint,RB){
  #All values except rcor and rcorint are in  full numbers
  #Initialise
  salarygrowth <- ((100+salaryinc+MSLinflation)/100)^seq(salarybase,37+salarybase, by=1)
  loanparams <- data.table(MSLloansize,MSLinterest,MSLinflation, timetopay, salaryinc, salarybase)
  loanparams <- loanparams[, lapply(.SD, as.factor)]
  
  #setting up Loans
  loansize <- MSLloansize
  loaninterest <- MSLinterest
  loanlength <- 12*timetopay
  b <- -PMT((loaninterest+MSLinflation)/1200, loanlength, loansize)
  
  #Mean Loan and SD of Loan
  m <- MSLloansize
  s <- MSLwidth
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  
  
  
  m2 <-MSLinterest + MSLinflation
  s2 <- MSLinterestwidth
  location2 <- log(m2^2 / sqrt(s2^2 + m2^2))
  shape2 <- sqrt(log(1 + (s2^2 / m2^2)))
  
  
  flocation2 <- log((m2-1)^2 / sqrt(s2^2 + (m2-1)^2))
  fshape2 <- sqrt(log(1 + (s2^2 / (m2-1)^2)))
  
  
  
  loancheck <-  data.table(loancor=rowSums(MSLOverallDataTest[,1:10]))
  loancheck <- cbind(loancheck,MSLOverallDataTest[,.(SNO,sex,sector,modeltype)])
  loancheck[modeltype=="dynamic",loancor:=loancheck[modeltype=="static",loancor]]
  
  loancheck[,loansize:=exp(rnorm_pre(x = loancheck$loancor,  mu = location, sd = shape, r=rcor, empirical=TRUE))]
  loancheck[modeltype=="dynamic",loansize:=loancheck[modeltype=="static",loansize]]
  
  
  loancheck[sex==1&modeltype=="static",loaninterest:=exp(rnorm_pre(x = loancheck[modeltype=="static"&sex==1,loansize],  mu = location2, sd = shape2, r=-rcorint, empirical=TRUE))]
  loancheck[sex==1&modeltype=="dynamic",loaninterest:=loancheck[modeltype=="static"&sex==1,loaninterest]]
  
  loancheck[sex==2&modeltype=="static",loaninterest:=exp(rnorm_pre(x=loancheck[sex==2&modeltype=="static",loansize],mu=flocation2,sd=fshape2, r=rcorint, empirical=TRUE))]
  loancheck[sex==2&modeltype=="dynamic",loaninterest:=loancheck[modeltype=="static"&sex==2,loaninterest]]
  
  loancheck[,EMI:=-PMT((loaninterest+MSLinflation)/1200,loanlength, loansize)]
  
  
  
  
  
  
  
  MSLOverallMatrix<- as.matrix(copy(MSLOverallDataTest[, Map("*", .SD[, 1:38], salarygrowth)]))
  rownames(MSLOverallMatrix) <- MSLOverallDataTest[,SNO]
  
  
  fixedLoanResultMatrix<- (1/MSLOverallMatrix[,1:eval(timetopay)])
  fixedLoanResultMatrix <- (b*100)*fixedLoanResultMatrix
  fixedLoanResultMatrix <- as.data.table(cbind(fixedLoanResultMatrix,MSLOverallDataTest[,.(SNO,sex,sector,modeltype)]))
  fixedLoanResultOutput <- cbind(fixedLoanResultMatrix[,1:eval(timetopay)]>RB,fixedLoanResultMatrix[,.(SNO,sex,sector,modeltype)])
  fixedLoanResultOutput[,YearsMoreThanRB:=rowSums(.SD),.SDcols=1:eval(timetopay)]
  fixedLoanResultOutput[,.N/100,keyby=.(sex,sector,modeltype,YearsMoreThanRB)]
  
  
  floatLoanResultMatrix <- (1/MSLOverallMatrix[,1:eval(timetopay)])
  floatLoanResultMatrix <- loancheck[,EMI]*floatLoanResultMatrix
  floatLoanResultMatrix <- as.data.table(cbind(floatLoanResultMatrix,MSLOverallDataTest[,.(SNO,sex,sector,modeltype)]))
  floatLoanResultOutput <- cbind(floatLoanResultMatrix[,1:eval(timetopay)]>RB,floatLoanResultMatrix[,.(SNO,sex,sector,modeltype)])
  floatLoanResultOutput[,YearsMoreThanRB:=rowSums(.SD),.SDcols=1:eval(timetopay)]
  floatLoanResultOutput[,.N/100,keyby=.(sex,sector,modeltype,YearsMoreThanRB)]
  
  
  return(list(fixedLoanResultOutput[,.N/100,keyby=.(sex,sector,modeltype,YearsMoreThanRB)][,loanexample:="constant"], floatLoanResultOutput[,.N/100,keyby=.(sex,sector,modeltype,YearsMoreThanRB)][,loanexample:="varied"]))
  

}


  
names(MSLfunctiondata, new=c(MSLloansize,MSLwidth, MSLinterest, MSLinterestwidth, MSLinflation, timetopay, salaryinc, salarybase, rcor, rcorint,RB))


MSLfunctiondata1 <- matrix(c(200000,80000,7,1,3,7,2,0,0.5,0.3,18))
MSLfunctiondata<- as.data.table(t(MSLfunctiondata1))
colnames(MSLfunctiondata) <- c("MSLloansize","MSLwidth", "MSLinterest", "MSLinterestwidth", "MSLinflation", "timetopay", "salaryinc", "salarybase", "rcor", "rcorint","RB")


results<- pmap_df(MSLfunctiondata,MSLfunction)


write.table(results, "MSLloaninfo.txt")


results.recasted <- dcast.data.table(results, loanexample + sector + sex + modeltype ~YearsMoreThanRB, value.var="V1")
write.table(results.recaste "MSLloaninforecasted.txt")

