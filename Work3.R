#Select only Men
weightedpanelfemale <- copy(weightedpanel[sex.x==2])





SelectedCopula <- list()

#Determine apprope COpula - Men
for(i in 23:60){
  SelectedCopula[[i]] <- BiCopSelect(
    u1=pobs(weightedpanelfemale[age.x==i,totalearnings.yPercentile]),
    u2=pobs(weightedpanelfemale[age.x==i, totalearnings.xPercentile]))
  
  
}







sele#weightedpanelmale


#tCopula <- list()

#cop_model <- tCopula()

#m <- as.matrix(weightedpanelmale[age.y==45,.(totalearnings.yPercentile,totalearnings.xPercentile)])


#plot(m)


#fit <- fitCopula(cop_model, m, optim.method="Nelder-Mead")




#Determine Copula Estimates
fCopulaEstimations <- list()
fCopulaParameters <- list()

for (i in 23:60) {
  
  fcop_model <- tCopula(dim=2)
  
  
  fm <- as.matrix(pobs(weightedpanelfemale[age.y==i,.(totalearnings.yPercentile,totalearnings.xPercentile)]))
  
  ffit <- fitCopula(fcop_model, fm, optim.method="Nelder-Mead")
  fCopulaEstimations[[i]] <- ffit
  fCopulaParameters[[i]] <- as.list(ffit@estimate)    
}

#Construct Table of Estimates of COpula params
fCopulaTable<- rbindlist(as.list(fCopulaParameters))
fCopulaTable
fCopulaTable[,age:=(23:60)]
setnames(fCopulaTable,c(V1="rhoestim", V2="dfestim", age="age"))



#tCopula(fit)

#myCop <- tCopula(param=rho, dim=2, df=2, dispstr= "un")

#plot(CopulaTable[,.])
#plx<-predict(loess(cars$dist ~ cars$speed), se=T)


#nu <- as.numeric(CopulaTable[age==27, dfestim])
#rho <- as.numeric(CopulaTable[age==27,rhoestim])

#m <- m*100

#m


#n <- pobs(as.matrix(weightedpanelmale[age.y==30,.(totalearnings.yPercentile,totalearnings.xPercentile)]))


#plot(m)
#plot(cCopula(u=m, copula=myCop, inverse=TRUE))
#plot(cCopula(u=pobs(m), copula=myCop, inverse=TRUE))


#cCopula(u=0.8, copula=myCop, inverse=false)
#trialcop <- BiCop(family=2, par = rho, par2 = nu)

#Predict COpula Estimates for usage
fpredicteddfmodel <- loess(dfestim~age, data=fCopulaTable)
fpredictedrhomodel <- loess(rhoestim~age, data=fCopulaTable)

fCopulaTable[,predictedrho:=fitted(fpredictedrhomodel)] 
fCopulaTable[,predicteddf:=fitted(fpredicteddfmodel)]

write.table(fCopulaTable,file="femalecopulaTable.txt")

fprojectedtable <- data.table(SNO=c(1:10000), p23=rep(1:100, each=100),
                             rnames=pobs(runif(n=10000)))

set.seed(30)

#First 'for' loop - choosing which Copula function i'm using.
for(l in 23:60){
  rho <- as.numeric(fCopulaTable[age==l, predictedrho])
  nu <- as.numeric(fCopulaTable[age==l,predicteddf])
  temp <- paste0("p",l)
  temp2 <- paste0("p",l+1)
  #just checking to make sure it's been selected
  rho
  nu
  
  fprojectedtable[,rnames:=pobs(runif(10000))]
  

  #setting up the Copula using the chosen function
  fmyCop <- tCopula(param=rho, dim=2, df=nu)
  
  
  #Estimating numerically the value of the conditional DF of u2 given different values of u, where u = distribution of ranks at t1. 
  #This basically generates points on a CDF curve to show what the likelihood of the value of U2 will be given the value of u1 (e.g.: if u1 is 0.1,
  #U2 will have a really really high likelihood of being 0.09, 0.1, or 0.11, so the CDF will reflect that accordingly)
  fu <- pobs(seq(0.01,1, by=0.01))
  fu2 <- seq(0,1, by=0.01)
  fccop <- sapply(fu, function(fu.)
    cCopula(cbind(fu.,fu2),copula=fmyCop, indices=2))
  
  
  #Setting up storage for the results of the loops below
  ftesttable <- list()
  
  #2nd Loop: to work through each percentile variable, we subset the main table by choosing each percentile, i.e. value of u1 
  for(k in 1:100){
    
    ftesttable[[k]] <- fprojectedtable[get(temp)==k,.(SNO,rnames)]
    #3rd Loop: selecting each random number within the kth percentile student at t1 
    if(nrow(ftesttable[[k]])>0){
      for(j in 1:as.numeric(count(ftesttable[[k]]))){
        
        frvalue<- ftesttable[[k]][j,rnames]
        
        #4th Loop: to determine where the generated random number lies in the conditional DF, determining what its percentile rank in t2 is 
        for(i in 1:100){
          ifelse(data.table::between(frvalue, fccop[i,k], fccop[i+1,k]), t <- i, "FALSE")
          
        }
        
        ftesttable[[k]][j,eval(temp2):=t]
        
        
      }}
    
  }
  
  
  fsecondttable <- as.data.table(rbindlist(ftesttable, fill=TRUE))
  fsecondttable[,rnames:=NULL]
  
  fprojectedtable<- merge.data.table(fprojectedtable,fsecondttable, by=c("SNO"))
  
  
}

fprojectedtable[,rnames:=NULL]




ftestingprocess <- copy(weightedpanelfemale)


ftestingprocess[,randomno:=pobs(runif(nrow(weightedpanelfemale)))]

ftesttable2 <- list()

set.seed(30)

#First 'for' loop - choosing which Copula function i'm using.
for(l in 23:60){
  rho <- as.numeric(fCopulaTable[age==l, predictedrho])
  nu <- as.numeric(fCopulaTable[age==l,predicteddf])
  
  #just checking to make sure it's been selected
  rho
  nu
  
  
  #setting up the Copula using the chosen function
  fmyCop <- tCopula(param=rho, dim=2, df=nu)
  
  
  #Estimating numerically the value of the conditional DF of u2 given different values of u, where u = distribution of ranks at t1. 
  #This basically generates points on a CDF curve to show what the likelihood of the value of U2 will be given the value of u1 (e.g.: if u1 is 0.1,
  #U2 will have a really really high likelihood of being 0.09, 0.1, or 0.11, so the CDF will reflect that accordingly)
  fu <- pobs(seq(0.01,1, by=0.01))
  fu2 <- seq(0,1, by=0.01)
  fccop <- sapply(fu, function(fu.)
    cCopula(cbind(fu.,fu2),copula=fmyCop, indices=2))
  
  
  #Setting up storage for the results of the loops below
  
  
  #2nd Loop: to work through each percentile variable, we subset the main table by choosing each percentile, i.e. value of u1 
  for(k in 1:100){
    
    ftesttable2[[(100*(l-23))+k]] <- ftestingprocess[totalearnings.yPercentile==k&age.y==l,.(UID,randomno, age.y)]
    #3rd Loop: selecting each random number within the kth percentile student at t1 
    if(nrow(ftesttable2[[(100*(l-23))+k]])>0){
      for(j in 1:as.numeric(count(ftesttable2[[(100*(l-23))+k]]))){
        
        rvalue<- ftesttable2[[(100*(l-23))+k]][j,randomno]
        
        #4th Loop: to determine where the generated random number lies in the conditional DF, determining what its percentile rank in t2 is 
        for(i in 1:100){
          ifelse(data.table::between(rvalue, fccop[i,k], fccop[i+1,k]), t <- i, "FALSE")
          
        }
        
        ftesttable2[[(100*(l-23))+k]][j,fpredictedt2:=t]
        
        
      }}
    
  }
  
  
  
}


fsecondttable <- as.data.table(rbindlist(ftesttable2, fill=TRUE))
fsecondttable[,randomno:=NULL]

ftestingprocess <- merge.data.table(ftestingprocess,fsecondttable, by=c("UID"))






predictedstrat<- read.table(file = "predictedstrat.txt")


predictedstrat <- as.data.table(predictedstrat)


fprojectedestims <- copy(fprojectedtable)
fprojectedestimsrural <- copy(fprojectedestims)
fprojectedestimsurban <- copy(fprojectedestims)


fprojectedestims <- fprojectedestims[,lapply(.SD, as.numeric)]

fprojectedestims[,p61:=NULL]



for(j in 23:60){
  
  temp <- paste0("p",j)
  
  for(i in 1:100){
    fprojectedestims[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==2&percentile==i&sector==3,estimincome]]
    fprojectedestimsurban[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==2&percentile==i&sector==2,estimincome]]
    fprojectedestimsrural[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==2&percentile==i&sector==1,estimincome]]
    
  }
}

fpfprojectedestimscalc <- as.matrix(fprojectedestims)
fprojectedestimscalc <- fprojectedestimscalc[,-1]

fprojectedestimscalc2 <- fprojectedestimscalc[,-1]
fprojectedestimscalc2 <- cbind(fprojectedestimscalc2, c(rep(0,10000)))

plot(density(fprojectedestimscalc2-fprojectedestimscalc), col="blue")


fprojectedestimscalc <- as.matrix(fprojectedestims)
fprojectedestimscalc <- fprojectedestimscalc[,-1]
fprojectedestimscalc2 <- fprojectedestimscalc[,-1]
fprojectedestimscalc2 <- cbind(fprojectedestimscalc2, c(rep(0,10000)))


fprojectedestimscalcurban <- as.matrix(fprojectedestimsurban)
fprojectedestimscalcurban <- fprojectedestimscalcurban[,-1]
fprojectedestimscalcurban2 <- fprojectedestimscalcurban[,-1]
fprojectedestimscalcurban2 <- cbind(fprojectedestimscalcurban2, c(rep(0,10000)))

fprojectedestimscalcrural <- as.matrix(fprojectedestimsrural)
fprojectedestimscalcrural <- fprojectedestimscalcrural[,-1]
fprojectedestimscalcrural2 <- fprojectedestimscalcrural[,-1]
fprojectedestimscalcrural2 <- cbind(fprojectedestimscalcrural2, c(rep(0,10000)))




for(j in 23:60){
  
  
  for(i in 1:100){
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincome:=predictedstrat[age==j&sex==2&percentile==i&sector==3,estimincome]]
    ftestingprocess[age.y.x==j&fpredictedt2==i, finalincome:=predictedstrat[age==j&sex==2&percentile==i&sector==3,estimincome]]
    
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomeurban:=predictedstrat[age==j&sex==2&percentile==i&sector==2,estimincome]]
    ftestingprocess[age.y.x==j&fpredictedt2==i, finalincomeurban:=predictedstrat[age==j&sex==2&percentile==i&sector==2,estimincome]]
    
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerural:=predictedstrat[age==j&sex==2&percentile==i&sector==1,estimincome]]
    ftestingprocess[age.y.x==j&fpredictedt2==i, finalincomerural:=predictedstrat[age==j&sex==2&percentile==i&sector==1,estimincome]]
  }
}



for(j in 23:60){
  
  
  for(i in 1:100){
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomereal:=predictedstrat[age==j&sex==2&percentile==i&sector==3,estimincome]]
    ftestingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomereal:=predictedstrat[age==j&sex==2&percentile==i&sector==3,estimincome]]
    
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerealurban:=predictedstrat[age==j&sex==2&percentile==i&sector==2,estimincome]]
    ftestingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomerealurban:=predictedstrat[age==j&sex==2&percentile==i&sector==2,estimincome]]
    
    ftestingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerealrural:=predictedstrat[age==j&sex==2&percentile==i&sector==1,estimincome]]
    ftestingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomerealrural:=predictedstrat[age==j&sex==2&percentile==i&sector==1,estimincome]]
  }
}


for(i in c(21, 41, 61, 81, 101)){
  ftestingprocess[totalearnings.yPercentile<i&totalearnings.yPercentile>(i-21),quintile1:=(i-1)/20]
  ftestingprocess[totalearnings.xPercentile<i&totalearnings.xPercentile>(i-21),quintile2:=(i-1)/20]
}

fprojectedtablequintiles <- copy(fprojectedtable)

for(i in c(21, 41, 61, 81, 101)){
  
  for(j in 23:61){
    
    temp <- paste0("p",j)
    temp2 <- paste0("pq",j)
    fprojectedtablequintiles[get(temp)<i& get(temp)>(i-21),eval(temp2):=(i-1)/20]
    
    
  }
  
}


fprojectedtablequintiles[,2:40:=NULL]

write.table(fprojectedtablequintiles, file="fprojectedquintiles.txt", col.names = TRUE)
write.table(fprojectedestims, file="fprojectedestims.txt", col.names= TRUE)
write.table(fprojectedtable, file="fprojectedtable.txt", col.names=TRUE)

histogram(weightedpanelfemale[age.x==40, totalearnings.x])
