

tallyvalues <- compose(as.data.table,t,as.matrix,unname,~.x/100,table,rowSums,~(loancheck[,EMI]*100/.x[,1:7])>18)


loancheck[,loansize:=exp(rnorm_pre(x = loancheck$loancor,  mu = location, sd = shape, r=0.5, empirical=TRUE))]
loancheck[,loaninterest:=exp(rnorm_pre(x = loancheck$loansize,  mu = location2, sd = shape2, r=-0.3, empirical=TRUE))]
loancheck[,EMI:=-PMT(loaninterest/1200,loanlength, loansize)]







datatest <- copy(projectedestims)
datatesturban <- copy(projectedestimsurban)
datatestrural <- copy(projectedestimsrural)



stratdynloan <- data.table(SNO=c(1:10000), percentile=rep(1:100,each=100))
stratdynloanurban <- copy(stratdynloan)
stratdynloanrural <- copy(stratdynloan)

for(i in 23:60){
  
  temp <- paste0("p",i)
  
  stratdynloanrural<- merge(stratdynloanrural,predictedstrat[sex==1&sector==1&age==i,estimincome,by=percentile])
  setnames(stratdynloanrural, old="estimincome", new= temp)
  
  stratdynloan<- merge(stratdynloan,predictedstrat[sex==1&sector==3&age==i,estimincome,by=percentile])
  setnames(stratdynloan, old="estimincome", new= temp)
  
  
  stratdynloanurban<- merge(stratdynloanurban,predictedstrat[sex==1&sector==2&age==i,estimincome,by=percentile])
  setnames(stratdynloanurban, old="estimincome", new= temp)
  
  
  



}



thing <- list(datatest,stratdynloan[,-c(1)],datatesturban,stratdynloanurban[,-c(1)],datatestrural,stratdynloanrural[,-c(1)], 
              fdatatest,fstratdynloan[,-c(1)],fdatatesturban,fstratdynloanurban[,-c(1)],fdatatestrural,fstratdynloanrural[,-c(1)])
overalldatatest<- rbindlist(thing)


sex <- as.factor(c(rep(1:2,times=1,each=60000)))
sector <- as.factor(c(rep(3:1,times=2,each=20000)))
modeltype <- as.factor(c(rep(c("dynamic","static"),times=6, each=10000)))

overalldatatest[,sex:=sex][,sector:=sector][,modeltype:=modeltype]

thingnames <- c("Dynamics - Combined", "Dynamics")



fdatatest <- copy(fprojectedestims)
fdatatesturban <- copy(fprojectedestimsurban)
fdatatestrural <- copy(fprojectedestimsrural)



fstratdynloan <- data.table(SNO=c(1:10000), percentile=rep(1:100,each=100))
fstratdynloanurban <- copy(fstratdynloan)
fstratdynloanrural <- copy(fstratdynloan)


for(i in 23:60){
  
  temp <- paste0("p",i)
  fstratdynloan<- merge(fstratdynloan,predictedstrat[sex==2&sector==3&age==i,estimincome,by=percentile])
  setnames(fstratdynloan, old="estimincome", new= temp)
  
  fstratdynloanurban<- merge(fstratdynloanurban,predictedstrat[sex==2&sector==2&age==i,estimincome,by=percentile])
  setnames(fstratdynloanurban, old="estimincome", new= temp)
  
  fstratdynloanrural<- merge(fstratdynloanrural,predictedstrat[sex==2&sector==1&age==i,estimincome,by=percentile])
  setnames(fstratdynloanrural, old="estimincome", new= temp)
  
  
  
}


fthing <- list()
)
