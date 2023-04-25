tallyvalues <- compose(as.data.table,t,as.matrix,unname,~.x/100,table,rowSums)


datatest[,':='(sector=3,modeltype=1)]
datatesturban[,':='(sector=2,modeltype=1)]
datatestrural[,':='(sector=1,modeltype=1)]


stratdynloan[,':='(sector=3,modeltype=2)]
stratdynloanurban[,':='(sector=2,modeltype=2)]
stratdynloanrural[,':='(sector=1,modeltype=2)] 



thing<- list(datatest,stratdynloan[,-c(1,2)],datatesturban,stratdynloanurban[,-c(1,2)],datatestrural,stratdynloanrural[,-c(1,2)])
overalldatatest<- rbindlist(thing)
overalldatatest[,sex:=1]



arg1 <- c(1:2)
arg2 <- c(1:3)
arg3 <- c(1:2)
arglist <- list(modeltype=arg1, sector=arg2, sex=arg3)
crossArg <- cross_df(arglist)



myFunction <- function(x, y, z){
  modelval <- x
  sectorval <- y
  sexval <- z
  
  intermed<- loancheck[sector==sectorval&modeltype==modelval&sex==sexval,EMI]*100/overalldatatest[sector==sectorval&modeltype==modelval&sex==sexval,c(1:7)]>18
  output<- tallyvalues(intermed)
  names(output) <- c(as.character(1:length(output)))
  return(output)
  
}



loancheck <- copy(overalldatatest[,.(sector,modeltype,SNO,sex)])
loancheck[,loancor:=rowSums(overalldatatest[,2:11])]
set.seed(5)
loancheck[,loansize:=exp(rnorm_pre(x = loancor,  mu = location, sd = shape, r=0.5, empirical=TRUE)),by=.(sector,modeltype,sex)]
loancheck[sex==1,loaninterest:=exp(rnorm_pre(x = loansize,  mu = location2, sd = shape2, r=-0.3, empirical=TRUE)),by=.(sector,modeltype,sex)]
loancheck[sex==2,loaninterest:=exp(rnorm_pre(x = loansize,  mu = flocation, sd = fshape, r=-0.3, empirical=TRUE)),by=.(sector,modeltype,sex)]
loancheck[,EMI:=-PMT(loaninterest/1200,loanlength, loansize)]
loancheck[,sex:=1]
loancheck[,sex:=as.factor(sex)]





myData <- as.data.table(map2_dfr(crossArg$x, crossArg$y, crossArg$z, myFunction))
myData[,':='(sector=crossArg$y,modeltype=crossArg$x)]
my






fdatatest[,':='(sector=3,modeltype=1)]
fdatatesturban[,':='(sector=2,modeltype=1)]
fdatatestrural[,':='(sector=1,modeltype=1)]


fstratdynloan[,':='(sector=3,modeltype=2)]
fstratdynloanurban[,':='(sector=2,modeltype=2)]
fstratdynloanrural[,':='(sector=1,modeltype=2)] 



fthing<- list(fdatatest,fstratdynloan[,-c(1,2)],fdatatesturban,fstratdynloanurban[,-c(1,2)],fdatatestrural,fstratdynloanrural[,-c(1,2)])
foveralldatatest<- rbindlist(fthing)
foveralldatatest[,SNO:=rep(1:10000,6)]
foveralldatatest[,sex:=2]
overalldatatest <- rbind(overalldatatest,foveralldatatest)




m <- 200000
s <- 80000

m2 <- 10
m3 <- 9.5
s2 <- 1
location <- log(m^2 / sqrt(s^2 + m^2))

location2 <- log(m2^2 / sqrt(s2^2 + m2^2))

shape <- sqrt(log(1 + (s^2 / m^2)))
shape2 <- sqrt(log(1 + (s2^2 / m2^2)))




floancheck <- copy(foveralldatatest[,.(sector,modeltype,SNO)])
floancheck[,loancor:=rowSums(foveralldatatest[,2:11])]
set.seed(5)
floancheck[,loansize:=exp(rnorm_pre(x = loancor,  mu = location, sd = shape, r=0.5, empirical=TRUE)),by=.(sector,modeltype)]
floancheck[,loaninterest:=exp(rnorm_pre(x = loansize,  mu = flocation, sd = fshape, r=-0.3, empirical=TRUE)),by=.(sector,modeltype)]
floancheck[,EMI:=-PMT(loaninterest/1200,loanlength, loansize)]
floancheck[,sex:=2]

loancheck <- rbind(loancheck,floancheck)
#you are stupid and have to manually change floancheck to loancheck to work with the function!