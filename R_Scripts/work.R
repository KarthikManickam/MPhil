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
library(lemon)
library(directlabels)
options(digits=5)

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

fvpp2017[,yearobs:=17]
rvpp2017[,yearobs:=17]
fvpp2018[,yearobs:=18]
rvpp2018[,yearobs:=18]

fvpp2017[,UID:=paste(schedule,quarter,
                     sector,state,district,
                     nss_region,stratum,sub_stratum,sub_sample,
                     fod_sub_region,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

rvpp2017[,UID:=paste(schedule,quarter,
                     sector,state,district,
                     nss_region,stratum,sub_stratum,sub_sample,
                     fod_sub_region,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

fvpp2018[,UID:=paste(schedule,quarter,
                     sector,state,district,
                     nss_region,stratum,sub_stratum,sub_sample,
                     fod_sub_region,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]

rvpp2018[,UID:=paste(schedule,quarter,
                     sector,state,district,
                     nss_region,stratum,sub_stratum,sub_sample,
                     fod_sub_region,fsu,sample_sg_no,
                     second_stage_stratum, hhd_no, person_no, sep = "")]



stratdata <- rbindlist(l=list("fvpp2017"=fvpp2017, "fvpp2018"=fvpp2018), use.names = TRUE)


#Create Data Table for Children Only#
adults <- stratdata[age<61&age>22][gen_edu_level==12|gen_edu_level==13][status_code!=91][sex!=3]
adults[,fullearnings:=earnings_regular+earnings_regular1][yearobs==17,fullearnings:=fullearnings*(180.436/167.598)]
adultsseparate <- copy(adults)
mutate_ntile(adults, col=fullearnings, n= 100, weights="weight", by=c("age","sex"))
mutate_ntile(adultsseparate, col=fullearnings, n= 100, weights="weight", by=c("age","sex", "sector"))


fadults <- adults[sex==2]
madults <- adults[sex==1]
fadults2 <- adultsseparate[sex==2]
madults2 <- adultsseparate[sex==1]


listofregressions <- list()
predictedvalues <- list()


for(i in 1:100){
  
  
model<- lm(V1~poly(age,degree=5),madults[fullearningsPercentile==i,median(fullearnings),by=.(age)])
  
listofregressions[[i]]<- madults[fullearningsPercentile==i,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=1]

predictedvalues[[i]] <- data.table("age"=c(23:60), "sex"=1,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))

model<- lm(V1~poly(age,degree=5),fadults[fullearningsPercentile==i,median(fullearnings),by=.(age)])


listofregressions[[i+100]]<- fadults[fullearningsPercentile==i,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=2]

predictedvalues[[i+100]] <- data.table("age"=c(23:60), "sex"=2,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))


}

old <- c("age", "V1", "fittedstuff", "percentile", "sex")
new <- c("age", "actualEarnings", "fittedstuff", "fullearningsPercentile", "sex")

estimatedstratdata <- rbindlist(listofregressions)
write.table(estimatedstratdata, file="stratdata.txt", col.names=TRUE)

setnames(estimatedstratdata, old=old, new=new)

kernelestimatesmale<- adults[,.(UID,fullearnings,fullearningsPercentile,age, sex)]
kernelestimatesmale<- merge.data.table(kernelestimatesmale, estimatedstratdata, by=c("fullearningsPercentile", "age", "sex"))


predictedstratdata <- rbindlist(predictedvalues)
predictedstratdata[estimincome<0,estimincome:=0]
predictedstratdata[,sector:=as.factor(3)]
predictedstratdata[,percentile:=as.factor(percentile)]
predictedstratdata[,sex:=as.factor(sex)]
write.table(predictedstratdata, file="predictedstrat.txt", col.names=TRUE)




listofregressions2 <- list()
predictedvalues2 <- list()


for(i in 1:100){
  
  
  model<- lm(V1~poly(age,degree=5),madults2[fullearningsPercentile==i&sector==1,median(fullearnings),by=.(age)])
  
  listofregressions2[[i]]<- madults2[fullearningsPercentile==i&sector==1,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=1][,sector:=1]
  
  predictedvalues2[[i]] <- data.table("age"=c(23:60), "sex"=1, "sector"=1,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))
  
 
  
   model<- lm(V1~poly(age,degree=5),madults2[fullearningsPercentile==i&sector==2,median(fullearnings),by=.(age)])
  
  listofregressions2[[i+100]]<- madults2[fullearningsPercentile==i&sector==2,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=1][,sector:=2]
  
  predictedvalues2[[i+100]] <- data.table("age"=c(23:60), "sex"=1, "sector"=2,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))
  
  
  
  model<- lm(V1~poly(age,degree=5),fadults2[fullearningsPercentile==i&sector==1,median(fullearnings),by=.(age)])

  listofregressions2[[i+200]]<- fadults2[fullearningsPercentile==i&sector==1,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=2][,sector:=1]
  
  predictedvalues2[[i+200]] <- data.table("age"=c(23:60), "sex"=2,"sector"=1,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))
  
  
  
  model<- lm(V1~poly(age,degree=5),fadults2[fullearningsPercentile==i&sector==2,median(fullearnings),by=.(age)])
  
  listofregressions2[[i+300]]<- fadults2[fullearningsPercentile==i&sector==2,median(fullearnings),by=.(age)][,fittedstuff:=model$fitted.values][,percentile:=i][,sex:=2][,sector:=2]
  
  predictedvalues2[[i+300]] <- data.table("age"=c(23:60), "sex"=2,"sector"=2,  "percentile"=i, "estimincome"=c(predict(model, newdata=data.frame(age=23:60))))
  
  
  
}

old <- c("age", "V1", "fittedstuff", "percentile", "sex", "sector")
new <- c("age", "actualEarnings", "fittedstuff", "fullearningsPercentile", "sex", "sector")


estimatedstratdata2 <- rbindlist(listofregressions2)
write.table(estimatedstratdata2, file="stratdata2.txt", col.names=TRUE)

setnames(estimatedstratdata2, old=old, new=new)

kernelestimatesmale<- adults[,.(UID,fullearnings,fullearningsPercentile,age, sex, sector)]
kernelestimatesmale<- merge.data.table(kernelestimatesmale, estimatedstratdata2, by=c("fullearningsPercentile", "age", "sex", "sector"))


predictedstratdata2 <- rbindlist(predictedvalues2)
predictedstratdata2[estimincome<0,estimincome:=0]
predictedstratdata2[age<25 & percentile==20 &sex==1,estimincome:=0]

predictedstratdata2[,sector:=as.factor(sector)]
predictedstratdata2[,percentile:=as.factor(percentile)]
predictedstratdata2[,sex:=as.factor(sex)]
predictedstratdata2 <- rbind(predictedstratdata2,predictedstratdata)


write.table(predictedstratdata2, file="predictedstrat2.txt", col.names=TRUE)
predictedstratdata2<- as.data.table(read.table("predictedstrat2.txt"))

pgraph <- c(20,50,80,95)
sectorlabels <- c("Rural","Urban","Combined")
names(sectorlabels) <- c(1,2,3)
qgraph <- c(20,50,80,95)
sexlabels <- c("Male","Female")
names(sexlabels) <- c(1,2)
predictedstratdata2[,percentile:=as.factor(percentile)]

p <- ggplot(data=predictedstratdata2[as.numeric(percentile)%in%pgraph], aes(x=age, y=estimincome, linetype=percentile, shape=percentile)) + 
     geom_line() + geom_point(size=0.8) 


  q<- p + facet_rep_grid(sector~sex, labeller = labeller(sector =sectorlabels, sex=sexlabels )) + 
   theme_bw() + xlab("Age") + ylab("Estimated Income") + labs(linetype = "Percentile", shape= "Percentile") + ggtitle("Estimated Age-Income Profiles for Male Graduates") + theme(legend.position = "bottom")  +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
ggsave("test.png", plot=q, width =15.92, height=18.25, units='cm')
  
q <- ggplot(data=predictedstratdata2[as.numeric(percentile)%in%qgraph&sex==2], aes(x=age, y=estimincome, linetype=percentile, shape=percentile)) + 
  geom_line()

q<- q +  facet_rep_wrap(~sector, labeller = labeller(sector =sectorlabels,  )) + 
  theme_bw() + xlab("Age") + ylab("Estimated Income") + labs(linetype = "Percentile", shape= "Percentile") + ggtitle("Estimated Age-Income Profiles for Female Graduates") + theme(legend.position = "bottom") +
  geom_dl(aes(label = percentile),method = list(dl.trans(y=y-0.1, x=x-0.2), "first.points", cex = 0.8) )

ggsave("maleinfo.png")


##Age Distribution

ggplot(data=adults, aes(x=age)) + geom_histogram(fill="white", colour='black') + facet_rep_wrap(sex~sector, scales='free', labeller = labeller(sector =sectorlabels, sex=sexlabels))  + 
  theme_bw() + xlab("Age Histogram")  + ylab("Count") + labs(fill = "Sector") + theme(legend.position = 'bottom') + scale_fill_discrete(labels=sectorlabels)

## Income Distribution
ggplot(data=adults[fullearnings<70000 &fullearnings>0], aes(x=fullearnings)) + geom_histogram(fill="white", colour='black', binwidth = 50) + facet_rep_wrap(sex~sector, scales='free', labeller = labeller(sector =sectorlabels, sex=sexlabels))  + 
  theme_bw() + xlab("Age Histogram")  + ylab("Count") + labs(fill = "Sector") + theme(legend.position = 'bottom') + scale_fill_discrete(labels=sectorlabels)


ggplot(data=adults[fullearnings<70000 &fullearnings>0], aes(x=fullearnings)) + geom_density(fill="white", colour='black', binwidth = 50) + facet_rep_wrap(sex~sector, scales='free', labeller = labeller(sector =sectorlabels, sex=sexlabels))  + 
  theme_bw() + xlab("Age Histogram")  + ylab("Count") + labs(fill = "Sector") + theme(legend.position = 'bottom') + scale_fill_discrete(labels=sectorlabels)
