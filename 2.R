library(optiRum)
library(functional)

loansize <- 200000
loaninterest <- c(0.12,0.10)
loanlength <- 12*10

2
b<- -PMT(loaninterest/12, loanlength, loansize)



b/projectedestims

predictedstrat[,rb:=b*100/estimincome]



RBs<- predictedstrat[percentile==20&sector==3,rb,by=.(age,sex,sector)]

projectedestims[,colSums((b*100/.SD[,!"SNO"])<18)/100]

t(as.matrix(RBs[sex==1&sector==3,V1,by=age]))


sum(t(as.matrix(b*100/predictedstrat[sex==1&sector==3&age==30,estimincome]<40)))

sum(b*100/projectedestims[SNO%%100==1,p30]<40)



growthsalary <- 1.05^(0:(ncol(projectedestimsadjust)-1))


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


tableoutputs <- list(statictable, dynamictable, statictablerural, dynamictablerural, statictableurban,dynamictableurban)


finaloutputmale<- as.data.table(rbindlist(lapply(tableoutputs,makeinto)))

finaloutputmale[,sector:=c(rep("Overall",2),rep("Rural",2),rep("Urban",2))]
finaloutputmale[,type:=rep(c("static","dynamic"),3)]
finaloutputmale[,sex:="male"]



ftableoutputs <- list(fstatictable, fdynamictable, fstatictablerural, fdynamictablerural, fstatictableurban, fdynamictableurban)
finaloutputfemale<- as.data.table(rbindlist(lapply(ftableoutputs,makeinto), fill=TRUE))
finaloutputfemale[,sector:=c(rep("Overall",2),rep("Rural",2),rep("Urban",2))]
finaloutputfemale[,type:=rep(c("static","dynamic"),3)]
finaloutputfemale[,sex:="female"]


write.table(rbind(finaloutputmale, finaloutputfemale), "finaloutput.txt")
=