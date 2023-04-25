library(optiRum)

loansize <- 
loaninterest <- 0.12
loanlength <- 12*10


b<- -PMT(loaninterest/12, loanlength, loansize)



b/projectedestims

predictedstrat[,rb:=b*100/estimincome]

RBs<- predictedstrat[percentile==20&sector==3,rb,by=.(age,sex,sector)]

projectedestims[,colSums((b*100/.SD[,!"SNO"])<18)/100]

t(as.matrix(RBs[sex==1&sector==3,V1,by=age]))


sum(t(as.matrix(b*100/predictedstrat[sex==1&sector==3&age==30,estimincome]<40)))

sum(b*100/projectedestims[SNO%%100==1,p30]<40)


rlnorm(10000, meanlog=200000, sdlog=80000)


x = rnorm(10000, 200000)
grid = seq(0,25,.1)

plot(density(x))
plot(grid,dlnorm(grid,200000,80000),type="l",xlab="x",ylab="f(x)")
lines(density(x),col="red")

legend("topright",c("True Density","Estimate"),lty=1,col=1:2)



m <- 200000
s <- 80000

m2 <- 10
s2 <- 1
location <- log(m^2 / sqrt(s^2 + m^2))

location2 <- log(m2^2 / sqrt(s2^2 + m2^2))

shape <- sqrt(log(1 + (s^2 / m^2)))
shape2 <- sqrt(log(1 + (s2^2 / m2^2)))

print(paste("location:", location))
print(paste("shape:", shape))
loansize <- rlnorm(n=10000, location, shape)


set.seed(1)


loancheck <-  data.table(loancor=rowSums(projectedestimsadjust[,2:11]))
loancheck[,SNO:=projectedestims[,SNO]]

loancheck[,loansize:=exp(rnorm_pre(x = loancheck$loancor,  mu = location, sd = shape, r=0.5, empirical=TRUE))]
loancheck[,loaninterest:=exp(rnorm_pre(x = loancheck$loansize,  mu = location2, sd = shape2, r=-0.3, empirical=TRUE))]
loancheck[,EMI:=-PMT(loaninterest/1200,loanlength, loansize)]





RBdynamic5overall2 <- )







table(rowSums(((1/(projectedestimsadjust)*100/loancheck[,EMI])>18)[,1:7]))*100/sum(table(rowSums(((1/(projectedestimsadjust)*100/loancheck[,-EMI])>18)[,1:7])))

compose()














projectedestimsadjust <- copy(projectedestims[,Map("*",.SD[,-1], growthsalary)])
RBdynamic5overall <- projectedestimsadjust[,(b*100/.SD)>18]
dynamictable<- table(rowSums(RBdynamic5overall[,1:7]))*100/sum(table(rowSums(RBdynamic5overall[,1:7])))


predictedstratadjust <- copy(predictedstrat[sex==1&sector==3])
statictable<- table(predictedstratadjust[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(predictedstratadjust[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))
# 

projectedestimsadjustrural <- copy(projectedestimsrural[,Map("*",.SD[,-1], growthsalary)])
RBdynamic5rural <- projectedestimsadjustrural[,(b*100/.SD)>18]
dynamictablerural<- table(rowSums(RBdynamic5rural[,1:7]))*100/sum(table(rowSums(RBdynamic5rural[,1:7])))

predictedstratadjustrural <- copy(predictedstrat[sex==1&sector==1])
statictablerural<- table(predictedstratadjustrural[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(predictedstratadjustrural[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))


projectedestimsadjusturban <- copy(projectedestimsurban[,Map("*",.SD[,-1], growthsalary)])
RBdynamic5urban <- projectedestimsadjusturban[,(b*100/.SD)>18]
dynamictableurban<- table(rowSums(RBdynamic5urban[,1:7]))*100/sum(table(rowSums(RBdynamic5urban[,1:7])))

predictedstratadjusturban <- copy(predictedstrat[sex==1&sector==2])
statictableurban<- table(predictedstratadjusturban[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(predictedstratadjusturban[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))





fprojectedestimsadjust <- copy(fprojectedestims[,Map("*",.SD[,-1], growthsalary)])
fRBdynamic5overall <- fprojectedestimsadjust[,(b*100/.SD)>18]
fdynamictable<- table(rowSums(fRBdynamic5overall[,1:7]))*100/sum(table(rowSums(fRBdynamic5overall[,1:7])))


fpredictedstratadjust <- copy(predictedstrat[sex==2&sector==3])
fstatictable<- table(fpredictedstratadjust[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(fpredictedstratadjust[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))


fprojectedestimsadjustrural <- copy(fprojectedestimsrural[,Map("*",.SD[,-1], growthsalary)])
fRBdynamic5rural <- fprojectedestimsadjustrural[,(b*100/.SD)>18]
fdynamictablerural<- table(rowSuunms(fRBdynamic5rural[,1:7]))*100/sum(table(rowSums(fRBdynamic5rural[,1:7])))

fpredictedstratadjustrural <- copy(predictedstrat[sex==2&sector==1])
fstatictablerural<- table(fpredictedstratadjustrural[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(fpredictedstratadjustrural[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))


fprojectedestimsadjusturban <- copy(fprojectedestimsurban[,Map("*",.SD[,-1], growthsalary)])
fRBdynamic5urban <- fprojectedestimsadjusturban[,(b*100/.SD)>18]
fdynamictableurban<- table(rowSums(fRBdynamic5urban[,1:7]))*100/sum(table(rowSums(fRBdynamic5urban[,1:7])))

fpredictedstratadjusturban <- copy(predictedstrat[sex==2&sector==2])
fstatictableurban<- table(fpredictedstratadjusturban[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1])*100/sum(table(fpredictedstratadjusturban[age<30,rb>18,by=.(age,percentile)][,sum(V1),by=percentile][,V1]))

