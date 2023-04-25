#Read the necessary Packages in and changing global settings to be easier to read#
library(data.table) 
library(tidyverse)
library(readr)
library(ggthemes)
library(RColorBrewer)
library(uqr)
library(statar)
library(hutils)
library(VGAM)
library(fitdistrplus)
library(copula)
library(VineCopula)
library(reshape)
options(digits=8)


#Read the 2017 RDS files in: assumption is wd is set to same folder as RDS files#
fvhh2017 <- readRDS("./plfsdata/201718/hhfv201718.rds")
rvhh2017 <- readRDS("./plfsdata/201718/hhrv201718.rds")
fvpp2017 <- readRDS("./plfsdata/201718/perfv201718.rds")
rvpp2017 <- readRDS("./plfsdata/201718/perrv201718.rds")

#Read the 2018 RDS files in: assumption is wd is set to same folder as RDS files#
fvhh2018 <- readRDS("./plfsdata/201819/hhfv201819.rds")
rvhh2018 <- readRDS("./plfsdata/201819/hhrv201819.rds")
fvpp2018 <- readRDS("./plfsdata/201819/perfv201819.rds")
rvpp2018 <- readRDS("./plfsdata/201819/perrv201819.rds")




#Double Check that it's set up as a data table#
fvpp2017 <- as.data.table(fvpp2017)
rvpp2017 <- as.data.table(rvpp2017)
fvpp2018 <- as.data.table(fvpp2018)
rvpp2018 <- as.data.table(rvpp2018)



#Assign personal UIDs#

fvpp2017[,yearobs:=17]
rvpp2017[,yearobs:=17][,hhd_no:=as.integer(hhd_no)]
fvpp2018[,yearobs:=18]
rvpp2018[,yearobs:=18][,hhd_no:=as.integer(hhd_no)]

fvpp2017[,UID:=paste(sector,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

rvpp2017[,UID:=paste(sector,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

fvpp2018[,UID:=paste(sector,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

rvpp2018[,UID:=paste(sector,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]



#subselect only Working-age Folk#

fvdata <- fvpp2017[sector==2][age<61&age>22][gen_edu_level==12|gen_edu_level==13][status_code!=91][sex!=3][,yo:=17]
rvdata <- rvpp2017[sector==2][age<61&age>22][gen_edu_level==12|gen_edu_level==13][sex!=3][visit=='V4'][,yo:=17]

fvdata2 <- fvpp2018[sector==2][age<61&age>22][gen_edu_level==12|gen_edu_level==13][status_code!=91][sex!=3][,yo:=18]
rvdata2 <- rvpp2018[sector==2][age<61&age>22][gen_edu_level==12|gen_edu_level==13][sex!=3][visit=='V4'][,yo:=18]



panel1<- merge.data.table(rvdata[,.(age,sex,yo,visit,totalearnings=earnings_regular + earnings_regular1, UID)], 
                 fvdata[,.(age,sex,yo,visit, totalearnings=earnings_regular + earnings_regular1, UID)],
                 by='UID')


panel2 <- merge.data.table(rvdata2[,.(age,sex,yo,visit,totalearnings=earnings_regular + earnings_regular1, UID)], 
                 fvdata2[,.(age,sex,yo,visit,totalearnings=earnings_regular + earnings_regular1, UID)],
                 by='UID')


#Remove illogical/wrong data entries (e.g. age diff > 1)
panel2[,agediff:=age.x-age.y]
panel1[,agediff:=age.x-age.y]

panel2final <- panel2[agediff<2 & agediff > -2 & sex.x==sex.y]
panel1final <- panel1[agediff<2 & agediff > -2 & sex.x==sex.y]

names(panel1final)
names(panel2final)
paneldata<- rbindlist(l=list(panel1final, panel2final))


count(panel2[agediff>-2&agediff<2&(sex.x==sex.y)])
count(panel1[agediff>-2&agediff<2&(sex.x==sex.y)])


paneldata[agediff!=0]



#add Weights
test <- paneldata[yo.x==17,UID]
weights17 <- fvpp2017[UID %in% test,.(yearobs, UID, weight)]
setnames(weights17,1,"yo.x")
test <- paneldata[yo.x==18,UID]
weights18 <- fvpp2018[UID %in% test,.(yearobs, UID, weight)]
setnames(weights18,1,"yo.x")



weightsfull <- rbindlist(l=list(weights17, weights18))

weightedpanel <- merge.data.table(paneldata, weightsfull, by=c('UID', 'yo.x'))



#Determine Percentiles
mutate_ntile(weightedpanel, col=totalearnings.y, n= 100, weights="weight", keyby=c("age.y","sex.y"))
mutate_ntile(weightedpanel, col=totalearnings.x, n= 100, weights="weight", keyby=c("age.x","sex.x"))


#Select only Men
weightedpanelmale <- weightedpanel[sex.x==1]
weightedpanelfemale <- weightedpanel[sex.x==2]


listofregressions <- list()
listofregressions2 <- list()

count <- 0


#Wrong Estimator

for(i in 1:100){
  
  
  model<- lm(V1~poly(age.y,degree=5),data=weightedpanel[sex.y==1][totalearnings.yPercentile<(i+1)&totalearnings.yPercentile>(i-1),median(totalearnings.y),by=.(age.y)])
  
  listofregressions[[i]]<- data.table(age = c(23:60),
                                      estimated_earnings= unname(predict(model,data.frame(age.y=c(23:60)))),
                                      percentile = i,
                                      t = 1,
                                      sex= 1)
  
    
  model2<- lm(V1~poly(age.x,degree=5),data=weightedpanel[sex.x==1][totalearnings.xPercentile<(i+1)&totalearnings.xPercentile>(i-1),median(totalearnings.x),by=.(age.x)])
  
  listofregressions2[[i]]<- data.table(age = c(23:60),
                                       estimated_earnings= unname(predict(model2,data.frame(age.x=c(23:60)))),
                                       percentile = i,
                                       t = 2,
                                       sex= 1)
  
  
  model <- NULL
  model2 <- NULL
  NULL    
  
  
  model<- lm(V1~poly(age.y,degree=5),data=weightedpanel[sex.y==2][totalearnings.yPercentile<(i+1)&totalearnings.yPercentile>(i-1),median(totalearnings.y),by=.(age.y)])
  
  listofregressions[[i+100]]<- data.table(age = c(23:60),
                                      estimated_earnings= unname(predict(model,data.frame(age.y=c(23:60)))),
                                      percentile = i,
                                      t = 1,
                                      sex = 2)
  
  
  model2<- lm(V1~poly(age.x,degree=5),data=weightedpanel[sex.x==2][totalearnings.xPercentile<(i+1)&totalearnings.xPercentile>(i-1),median(totalearnings.x),by=.(age.x)])
  
  
  
  
  listofregressions2[[i+100]]<- data.table(age = c(23:60),
                                       estimated_earnings= unname(predict(model2,data.frame(age.x=c(23:60)))),
                                       percentile = i,
                                       t = 2,
                                       sex = 2)
  
  model <- NULL
  model2 <- NULL
  NULL                    
  
  count <- count+1
  
}


#Unsure if this Stuff is RElevent, but running anyway#
table1 <- as.data.table(rbindlist(listofregressions), key=age)
table2 <- as.data.table(rbindlist(listofregressions2), key=age)

setnames(table1, c("percentile","sex","age"), c("totalearnings.yPercentile", "sex.y", "age.y"))


setnames(table2, "estimated_earnings", "estimated_earnings2")
setnames(table2, c("percentile","sex","age", "t"), c("totalearnings.xPercentile", "sex.y", "age.y", "t"))


weightedpanel<- merge.data.table(weightedpanel, table2, by=c("age.y", "sex.y", "totalearnings.xPercentile"))
weightedpanel<- merge.data.table(weightedpanel, table1, by=c("age.y", "sex.y", "totalearnings.yPercentile"))




SelectedCopulaMale <- list()

SelectedCopula <- list()

#Determine apprope COpula - Men
for(i in 23:60){
SelectedCopula[[i]] <- BiCopSelect(
  u1=pobs(weightedpanelmale[age.x==i,totalearnings.yPercentile]),
  u2=pobs(weightedpanelmale[age.x==i, totalearnings.xPercentile]))
  

}







#weightedpanelmale


#tCopula <- list()

#cop_model <- tCopula()

#m <- as.matrix(weightedpanelmale[age.y==45,.(totalearnings.yPercentile,totalearnings.xPercentile)])


#plot(m)


#fit <- fitCopula(cop_model, m, optim.method="Nelder-Mead")




#Determine Copula Estimates
CopulaEstimations <- list()
CopulaParameters <- list()

for (i in 23:60) {
  
cop_model <- tCopula(dim=2)


m <- as.matrix(pobs(weightedpanelmale[age.y==i,.(totalearnings.yPercentile,totalearnings.xPercentile)]))
  
    fit <- fitCopula(cop_model, m, optim.method="Nelder-Mead")
    CopulaEstimations[[i]] <- fit
    CopulaParameters[[i]] <- as.list(fit@estimate)    
}
  
#Construct Table of Estimates of COpula params
CopulaTable<- rbindlist(as.list(CopulaParameters))
CopulaTable
CopulaTable[,age:=(23:60)]
setnames(CopulaTable,c(V1="rhoestim", V2="dfestim", age="age"))



AllCopulaTable

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
predicteddfmodel <- loess(dfestim~age, data=CopulaTable)
predictedrhomodel <- loess(rhoestim~age, data=CopulaTable)

CopulaTable[,predictedrho:=fitted(predictedrhomodel)] 
CopulaTable[,predicteddf:=fitted(predicteddfmodel)]



projectedtable <- data.table(SNO=c(1:10000), p23=rep(1:100, each=100),
                             rnames=pobs(runif(n=10000)))

set.seed(30)

#First 'for' loop - choosing which Copula function i'm using.
for(l in 23:60){
rho <- as.numeric(CopulaTable[age==l, predictedrho])
nu <- as.numeric(CopulaTable[age==l,predicteddf])
temp <- paste0("p",l)
temp2 <- paste0("p",l+1)
#just checking to make sure it's been selected
rho
nu

projectedtable[,rnames:=pobs(runif(10000))]

#loading the data for current age group prediction
cdfestimtable <- weightedpanelmale[age.y==l, .(totalearnings.yPercentile,totalearnings.xPercentile)]

#setting up the Copula using the chosen function
myCop <- tCopula(param=rho, dim=2, df=nu)


#Estimating numerically the value of the conditional DF of u2 given different values of u, where u = distribution of ranks at t1. 
#This basically generates points on a CDF curve to show what the likelihood of the value of U2 will be given the value of u1 (e.g.: if u1 is 0.1,
#U2 will have a really really high likelihood of being 0.09, 0.1, or 0.11, so the CDF will reflect that accordingly)
u <- pobs(seq(0.01,1, by=0.01))
u2 <- seq(0,1, by=0.01)
ccop <- sapply(u, function(u.)
  cCopula(cbind(u.,u2),copula=myCop, indices=2))


#Setting up storage for the results of the loops below
testtable <- list()

#2nd Loop: to work through each percentile variable, we subset the main table by choosing each percentile, i.e. value of u1 
for(k in 1:100){

testtable[[k]] <- projectedtable[get(temp)==k,.(SNO,rnames)]
#3rd Loop: selecting each random number within the kth percentile student at t1 
if(nrow(testtable[[k]])>0){
for(j in 1:as.numeric(count(testtable[[k]]))){

rvalue<- testtable[[k]][j,rnames]

 #4th Loop: to determine where the generated random number lies in the conditional DF, determining what its percentile rank in t2 is 
 for(i in 1:100){
   ifelse(data.table::between(rvalue, ccop[i,k], ccop[i+1,k]), t <- i, "FALSE")
  
 }
  
   testtable[[k]][j,eval(temp2):=t]


}}

}


secondttable <- as.data.table(rbindlist(testtable, fill=TRUE))
secondttable[,rnames:=NULL]

projectedtable<- merge.data.table(projectedtable,secondttable, by=c("SNO"))


}

projectedtable[,rnames:=NULL]




testingprocess <- copy(weightedpanelmale)
ftestingprocess <- copy(weightedpanelfemale)
  

testingprocess[,randomno:=pobs(runif(5838))]

testtable2 <- list()

set.seed(30)

#First 'for' loop - choosing which Copula function i'm using.
for(l in 23:60){
  rho <- as.numeric(CopulaTable[age==l, predictedrho])
  nu <- as.numeric(CopulaTable[age==l,predicteddf])
  
  #just checking to make sure it's been selected
  rho
  nu
  

  #setting up the Copula using the chosen function
  myCop <- tCopula(param=rho, dim=2, df=nu)
  
  
  #Estimating numerically the value of the conditional DF of u2 given different values of u, where u = distribution of ranks at t1. 
  #This basically generates points on a CDF curve to show what the likelihood of the value of U2 will be given the value of u1 (e.g.: if u1 is 0.1,
  #U2 will have a really really high likelihood of being 0.09, 0.1, or 0.11, so the CDF will reflect that accordingly)
  u <- pobs(seq(0.01,1, by=0.01))
  u2 <- seq(0,1, by=0.01)
  ccop <- sapply(u, function(u.)
    cCopula(cbind(u.,u2),copula=myCop, indices=2))
  
  
  #Setting up storage for the results of the loops below

  
  #2nd Loop: to work through each percentile variable, we subset the main table by choosing each percentile, i.e. value of u1 
  for(k in 1:100){
    
    testtable2[[(100*(l-23))+k]] <- testingprocess[totalearnings.yPercentile==k&age.y==l,.(UID,randomno, age.y)]
    #3rd Loop: selecting each random number within the kth percentile student at t1 
    if(nrow(testtable2[[(100*(l-23))+k]])>0){
      for(j in 1:as.numeric(count(testtable2[[(100*(l-23))+k]]))){
        
        rvalue<- testtable2[[(100*(l-23))+k]][j,randomno]
        
        #4th Loop: to determine where the generated random number lies in the conditional DF, determining what its percentile rank in t2 is 
        for(i in 1:100){
          ifelse(data.table::between(rvalue, ccop[i,k], ccop[i+1,k]), t <- i, "FALSE")
          
        }
        
        testtable2[[(100*(l-23))+k]][j,predictedt2:=t]
        
        
      }}
    
  }
  
  
  
}


secondttable <- as.data.table(rbindlist(testtable2, fill=TRUE))
secondttable[,randomno:=NULL]

testingprocess <- merge.data.table(testingprocess,secondttable, by=c("UID"))





#loading predicted estimates into the dynamic measure
predictedstrat<- read.table(file = "predictedstrat2.txt")
projectedtable <- as.data.table(read.table(file="projectedtable.txt"))
fprojectedtable <- as.data.table(read.table(file="fprojectedtable.txt"))

predictedstrat <- as.data.table(predictedstrat)


projectedestims <- copy(projectedtable)

projectedestims <- projectedestims[,lapply(.SD, as.numeric)]
projectedestims[,p61:=NULL]
projectedestimsurban <- copy(projectedestims)
projectedestimsrural <- copy(projectedestims)


for(j in 23:60){

  temp <- paste0("p",j)
  
for(i in 1:100){
projectedestims[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==1&percentile==i&sector==3,estimincome]]
projectedestimsurban[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==1&percentile==i&sector==2,estimincome]]
projectedestimsrural[get(temp)==i, eval(temp):=predictedstrat[age==j&sex==1&percentile==i&sector==1,estimincome]]
}
}

#Create a matrix of the difference between concurrent years
projectedestimscalc <- as.matrix(projectedestims)
projectedestimscalc <- projectedestimscalc[,-1]
projectedestimscalc2 <- projectedestimscalc[,-1]
projectedestimscalc2 <- cbind(projectedestimscalc2, c(rep(0,10000)))


projectedestimscalcurban <- as.matrix(projectedestimsurban)
projectedestimscalcurban <- projectedestimscalcurban[,-1]
projectedestimscalcurban2 <- projectedestimscalcurban[,-1]
projectedestimscalcurban2 <- cbind(projectedestimscalcurban2, c(rep(0,10000)))

projectedestimscalcrural <- as.matrix(projectedestimsrural)
projectedestimscalcrural <- projectedestimscalcrural[,-1]
projectedestimscalcrural2 <- projectedestimscalcrural[,-1]
projectedestimscalcrural2 <- cbind(projectedestimscalcrural2, c(rep(0,10000)))





#loading predicted estimates for urban, rural and combined ACTUAL values.
for(j in 23:60){
  
  
  for(i in 1:100){
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincome:=predictedstrat[age==j&sex==1&percentile==i&sector==3,estimincome]]
    testingprocess[age.y.x==j&predictedt2==i, finalincome:=predictedstrat[age==j&sex==1&percentile==i&sector==3,estimincome]]
    
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomeurban:=predictedstrat[age==j&sex==1&percentile==i&sector==2,estimincome]]
    testingprocess[age.y.x==j&predictedt2==i, finalincomeurban:=predictedstrat[age==j&sex==1&percentile==i&sector==2,estimincome]]
    
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerural:=predictedstrat[age==j&sex==1&percentile==i&sector==1,estimincome]]
    testingprocess[age.y.x==j&predictedt2==i, finalincomerural:=predictedstrat[age==j&sex==1&percentile==i&sector==1,estimincome]]
    
    
  }
}


for(j in 23:60){
  
  
  for(i in 1:100){
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomereal:=predictedstrat[age==j&sex==1&percentile==i&sector==3,estimincome]]
    testingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomereal:=predictedstrat[age==j&sex==1&percentile==i&sector==3,estimincome]]
    
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerealurban:=predictedstrat[age==j&sex==1&percentile==i&sector==2,estimincome]]
    testingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomerealurban:=predictedstrat[age==j&sex==1&percentile==i&sector==2,estimincome]]
    
    testingprocess[age.y.x==j&totalearnings.yPercentile==i, initialincomerealrural:=predictedstrat[age==j&sex==1&percentile==i&sector==1,estimincome]]
    testingprocess[age.y.x==j&totalearnings.xPercentile==i, finalincomerealrural:=predictedstrat[age==j&sex==1&percentile==i&sector==1,estimincome]]
  }
  }
    
    
 

for(i in c(21, 41, 61, 81, 101)){
testingprocess[totalearnings.yPercentile<i&totalearnings.yPercentile>(i-21),quintile1:=(i-1)/20]
testingprocess[totalearnings.xPercentile<i&totalearnings.xPercentile>(i-21),quintile2:=(i-1)/20]
}

projectedtablequintiles <- copy(projectedtable)

for(i in c(21, 41, 61, 81, 101)){

  for(j in 23:61){
  
    temp <- paste0("p",j)
    temp2 <- paste0("pq",j)
    projectedtablequintiles[get(temp)<i& get(temp)>(i-21),eval(temp2):=(i-1)/20]
  
  
  }

}


projectedtablequintiles[,2:40:=NULL]

write.table(projectedtablequintiles, file="projectedquintiles.txt", col.names = TRUE)
write.table(projectedestims, file="projectedestims.txt", col.names= TRUE)
write.table(projectedtable, file="projectedtable.txt", col.names=TRUE)




meltedtable <- melt(CopulaTable, id=c("age","sex"))
meltedtable


ggplot(meltedtable, aes(x=age, y=value, linetype=variable, group=variable))+   geom_line(subset)

sexlabels <- c("Male","Female")
names(sexlabels) <- c(1,2)

meltedtable[variable %in% c('rhoestim')]
s <- ggplot(meltedtable[variable %in% c('rhoestim','predictedrho')], aes(x=age, y=value, linetype=variable, group=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values=c(0.5,1)) + 
  scale_linetype_discrete(name = "Legend",
                          breaks = c('rhoestim', 'predictedrho'),
                          labels = c('Estimated Rho', 'Rho used in Simulation'))
  

  

s +  theme_bw() + xlab("Age") + ylab("Rho") + labs(linetype="Legend") + ggtitle("Estimated rho for t-Copulas") + theme(legend.position = "bottom")+  facet_wrap(~sex, labeller = labeller(sex=sexlabels)) +
  scale_linetype_discrete( name = "Legend",
                           breaks = c('rhoestim', 'predictedrho'),
                           labels = c('Estimated Rho', 'Rho used in Simulation' ))

    



r <- ggplot(meltedtable[variable %in% c('dfestim','predicteddf')], aes(x=age, y=value, linetype=variable, group=variable)) + geom_line(aes(size=variable)) + scale_size_manual(values=c(0.5,1)) + 
  theme_bw() + xlab("Age") + ylab("Degrees of Freedom") + labs(linetype="Legend") + ggtitle("Estimated DoF for t-Copulas") + theme(legend.position = "bottom") +
  scale_linetype_discrete( breaks = c('dfestim', 'predicteddf'),
                           labels = c('Estimated DoF', 'DoF used in Simulation' ))


r +  theme_bw() + xlab("Age") + ylab("DoF") + labs(linetype="Legend") + ggtitle("Estimated DoF for t-Copulas") + theme(legend.position = "bottom")+  facet_wrap(~sex, labeller = labeller(sex=sexlabels))



gridExtra::grid.arrange(s,r, nrow=2)


write.table(CopulaTable, file="malecopulatable.txt")
fCopulaTable <- as.data.table(read.table("femalecopulaTable.txt"))
CopulaTable[,sex:=1]
fCopulaTable[,sex:=2]

CopulaTableComplete <- rbind(CopulaTable,fCopulaTable)



meltedtable <- melt(CopulaTableComplete, id=c("age","sex"))




write.table(projectedestimsrural, file="./tables/projectedestimsrural.txt", col.names = TRUE)
