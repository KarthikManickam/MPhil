pgraph <- c(20,50,80,95)
sectorlabels <- c("Rural","Urban","Combined")
names(sectorlabels) <- c(1,2,3)
qgraph <- c(20,50,80,95)
sexlabels <- c("Male","Female")
names(sexlabels) <- c(1,2)


p <- ggplot(data=predictedstratdata2[as.numeric(percentile)%in%pgraph], aes(x=age, y=estimincome, linetype=percentile, shape=percentile)) + 
  geom_line() + geom_point() + scale_size_manual(values = c(0.2,0.2,0.2,0.2))


q<- p + facet_rep_grid(sex~sector, labeller = labeller(sector =sectorlabels, sex=sexlabels )) + 
  theme_bw() + xlab("Age") + ylab("Estimated Income") + labs(linetype = "Percentile", shape= "Percentile") + ggtitle("Estimated Age-Income Profiles for Male Graduates") + theme(legend.position = "bottom")  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

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



#COpulaGraphs


meltedtable <- melt(CopulaTable, id=c("age","sex"))
meltedtable


ggplot(meltedtable, aes(x=age, y=value, linetype=variable, group=variable))+   geom_line(subset)

sexlabels <- c("Male","Female")
names(sexlabels) <- c(1,2)

meltedtable[variable %in% c('rhoestim')]
s <- ggplot(meltedtable[variable %in% c('rhoestim','predictedrho')], aes(x=age, y=value, linetype=variable)) +
     geom_line() + 
     scale_size_manual(values=c(0.5,1)) +
  scale_linetype_discrete( name = "Legend",
                           breaks = c('rhoestim', 'predictedrho'),
                           labels = c('Estimated Rho', 'Rho used in Simulation'))


s <- s +  theme_bw() + xlab("Age") + ylab("Rho") + labs(linetype="Legend") + ggtitle("Estimates of rho from t-Copulas") +   facet_rep_wrap(~sex, labeller = labeller(sex=sexlabels)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))


ggsave("rho.png", width = 15.92, height=7.38, units = 'cm')

sdf <- ggplot(meltedtable[variable %in% c('dfestim','predicteddf')], aes(x=age, y=value, linetype=variable)) +
  geom_line() + 
  scale_size_manual(values=c(0.5,1)) +
  scale_linetype_discrete( name = "Legend",
                           breaks = c('dfestim', 'predicteddf'),
                           labels = c('Estimated df', 'df used in Simulation'))


sdf <- sdf +  theme_bw() + xlab("Age") + ylab("df") + labs(linetype="Legend") + ggtitle("Estimates of degrees of freedom \nfrom t-Copulas") +   facet_rep_wrap(~sex, labeller = labeller(sex=sexlabels)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))


ggsave("df.png", width = 15.92, height=7.38, units = 'cm')


#Comparing Denisties

projectedestimscalc <- as.matrix(projectedestims)
projectedestimscalc <- projectedestimscalc[,-1]
projectedestimscalc2 <- projectedestimscalc[,-1]
projectedestimscalc2 <- cbind(projectedestimscalc2, c(rep(0,10000)))
densitycalc <- as.data.table(projectedestimscalc2-projectedestimscalc)
densitycalc[,'V38':=1]
densitycalc[,sector:=3]



projectedestimscalcurban <- as.matrix(projectedestimsurban)
projectedestimscalcurban <- projectedestimscalcurban[,-1]
projectedestimscalcurban2 <- projectedestimscalcurban[,-1]
projectedestimscalcurban2 <- cbind(projectedestimscalcurban2, c(rep(0,10000)))
densityurbancalc <- as.data.table(projectedestimscalcurban2-projectedestimscalcurban)
densityurbancalc[,'V38':=1]
densityurbancalc[,sector:=2]


projectedestimscalcrural <- as.matrix(projectedestimsrural)
projectedestimscalcrural <- projectedestimscalcrural[,-1]
projectedestimscalcrural2 <- projectedestimscalcrural[,-1]
projectedestimscalcrural2 <- cbind(projectedestimscalcrural2, c(rep(0,10000)))
densityruralcalc <- as.data.table(projectedestimscalcrural2-projectedestimscalcrural)
densityruralcalc[,'V38':=1]
densityruralcalc[,sector:=1]


fprojectedestimscalc <- as.matrix(fprojectedestims)
fprojectedestimscalc <- fprojectedestimscalc[,-1]
fprojectedestimscalc2 <- fprojectedestimscalc[,-1]
fprojectedestimscalc2 <- cbind(fprojectedestimscalc2, c(rep(0,10000)))
fdensitycalc <- as.data.table(fprojectedestimscalc2-fprojectedestimscalc)
fdensitycalc[,'V38':=2]
fdensitycalc[,sector:=3]



fprojectedestimscalcurban <- as.matrix(fprojectedestimsurban)
fprojectedestimscalcurban <- fprojectedestimscalcurban[,-1]
fprojectedestimscalcurban2 <- fprojectedestimscalcurban[,-1]
fprojectedestimscalcurban2 <- cbind(fprojectedestimscalcurban2, c(rep(0,10000)))
fdensityurbancalc <- as.data.table(fprojectedestimscalcurban2-fprojectedestimscalcurban)
fdensityurbancalc[,'V38':=2]
fdensityurbancalc[,sector:=2]


fprojectedestimscalcrural <- as.matrix(fprojectedestimsrural)
fprojectedestimscalcrural <- fprojectedestimscalcrural[,-1]
fprojectedestimscalcrural2 <- fprojectedestimscalcrural[,-1]
fprojectedestimscalcrural2 <- cbind(fprojectedestimscalcrural2, c(rep(0,10000)))
fdensityruralcalc <- as.data.table(fprojectedestimscalcrural2-fprojectedestimscalcrural)
fdensityruralcalc[,'V38':=2]
fdensityruralcalc[,sector:=1]


projectedcalcfinal <- rbindlist(list(densitycalc,densityruralcalc,densityurbancalc,fdensitycalc,fdensityruralcalc,fdensityurbancalc))
setnames(projectedcalcfinal,"v38","sex")
projectedcalcfinal[,sex:=as.factor(sex)][,sector:=as.factor(sector)]


write.table(projectedcalcfinal,"combinedprojections.txt")
read.table("combinedprojections.txt")

meltedprojectedcalcfinal <- melt(projectedcalcfinal,id=c('sex','sector'))
meltedprojectedcalcfinal[,variable:="simul"]



ggplot(meltedprojectedcalcfinal,aes(x=value))



temp <- c(testingprocess[,finalincomereal-initialincomereal], 
          testingprocess[,finalincomerealurban-initialincomerealurban], 
          testingprocess[,finalincomerealrural-initialincomerealrural],
          ftestingprocess[,finalincomereal-initialincomereal], 
          ftestingprocess[,finalincomerealurban-initialincomerealurban], 
          ftestingprocess[,finalincomerealrural-initialincomerealrural])


tempstats <- as.factor(c(rep(3,testingprocess[,.N]),
                         rep(2,testingprocess[,.N]), 
                         rep(1,testingprocess[,.N]),
                         rep(3,ftestingprocess[,.N]),
                         rep(2,ftestingprocess[,.N]), 
                         rep(1,ftestingprocess[,.N])))

tempsex <- as.factor(c(rep(1,3*testingprocess[,.N]),
                       rep(2,3*ftestingprocess[,.N])))


meltedtestingprocess <- data.table('sex'=tempsex,'sector'=tempstats,'variable'="real", 'value'=temp )
meltedtestingfinal<- rbind(meltedtestingprocess, meltedprojectedcalcfinal)

 _

ptestplot <- ggplot(meltedtestingfinal,aes(x=value,colour=variable))+
                    geom_density() + 
                    facet_wrap(sex~sector, labeller = labeller(sex=sexlabels, sector=sectorlabels), scales="free") +
  scale_x_continuous(labels = comma) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  coord_cartesian(xlim=c(-50000,50000))




## Final graph - check from source 7


test <- pmap_dfr(functionTest,testfunction)
write.table(test, "ICLdata.txt")



p <- ggplot(data=test[ID==1], aes(x=decile, y=recovery, fill=modeltype)) + 
      geom_bar(stat="identity", position="dodge") + 
  facet_rep_wrap(sex~sector, labeller = labeller(sex=sexlabels, sector=sectorlabels), scales="free") +
      theme_bw() +
  scale_fill_discrete(name="Model Type", breaks=c("dynamic","static"), labels=c("Dynamic", "Static")) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) + geom_hline(yintercept = 100,  linetype = "dotted")
p

ggsave("firstresult.png", width=15.92, height=17, units = 'cm')


q <- ggplot(data=test[modeltype=="dynamic"], aes(x=decile, y=recovery, fill=ID)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_rep_wrap(sex~sector, labeller = labeller(sex=sexlabels, sector=sectorlabels), scales="free") +
  theme_bw() +
  scale_fill_discrete(name="Loan Conditions", breaks=c("1","2", "3"), labels=c("Base", "Fee Increase", "Stagnant Wage Growth")) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) + geom_hline(yintercept = 100,  linetype = "dotted")

q
ggsave("secondresult.png", width=15.92, height=17, units = 'cm')

adults[,sector:=as.factor(sector)]
r <- ggplot(data=adults, aes(x=age, fill=sector ))+ geom_density(alpha=0.8) +
  facet_rep_grid(~sex, labeller = labeller(sex=sexlabels, sector=sectorlabels), scales="free")+
  theme_bw() +
  scale_fill_discrete(name="Sector", breaks=c('1','2'), labels=c("Rural", "Urban")) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  ggtitle("Distribution of Ages in the Dataset")
ggsave("ageplot.png", width=15.92, height=10.92, units ='cm')



setnames(meltedstatictest,new='value',old="Earnings in INR (2019 Prices)")

##Confirming StatiC Estimates - check "testingmodule" for info
p<- ggplot(meltedstatictest[value<75000],aes(x=value, color=variable, linetype=variable)) +geom_density(size=1) + 
facet_rep_grid(sector~sex, labeller = labeller(sex=sexlabels, sector=sectorlabels), scales="free") +
  theme_bw() +
  scale_linetype_discrete(name="Density Source", breaks=c('fullearnings','estimincome'), labels=c("Actual", "Using Percentile Estimates")) +
  scale_color_discrete(name="Density Source", breaks=c('fullearnings','estimincome'), labels=c("Actual", "Using Percentile Estimates")) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
     xlab("Earnings in INR (2019 Prices)") + ylab("Density")
  

ggsave("statictestplot.png", width=15.92, height=14, units='cm')

p



loanamount <- 12000 
ICLcutoff <- 1000
ICLpercent <- 0.1
monhtlyinterest <- 0.01
mslrepayment <- 150

monthlyincome <- 
  sample(seq.int(500, 2000, by= 100), size=12, replace = T)


tablepayment <- data.table
tablepayment[MonthlyIncome>1000,ICLPayment:=ICLpercent*MonthlyIncome][MonthlyIncome<1000,ICLPayment:=0]


meltedtablepayment <- melt.data.table(tablepayment,id.vars = c("Month", )
meltedtablepayment[variable=="MonthlyIncome",colourvar:=as.factor(1)][variable=="ICLPayment"|variable=="rbICL",colourvar:=as.factor(2)][variable=="MSLPayment"|variable=="rbMSL",colourvar:=as.factor(3)]

labelsforgraph=c("Monthly Income and Payment","Repayment Burden as a Percentage")

graphforICL <-ggplot(meltedtablepayment[graph==1], aes(x=Month, shape=variable, color=variable))  +geom_line(aes(y=value))+ geom_point(aes(y=value))+


  theme_bw()  +
  scale_linetype_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Income", "ICL Payment", "MSL Payment","ICL Repayment Burden","MSL Repayment Burden")) +
  scale_shape_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Monthly Income", "ICL Payment", "MSL Payment", "ICL Repayment Burden","MSL Repayment Burden")) +
  scale_color_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Monthly Income", "ICL Payment", "MSL Payment","ICL Repayment Burden","MSL Repayment Burden")) +
  
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  scale_x_continuous(breaks=seq(1,12,by=1))+
  xlab("Month") + ylab("Dollars") 


ggsave("paymentgraph.png", width=12, height=14, units='cm')

graphforICL


graphforICL2 <-ggplot(meltedtablepayment[graph==2], aes(x=Month, shape=variable, color=variable))  +geom_line(aes(y=value))+ geom_point(aes(y=value))+
  
  
  theme_bw()  +
  scale_linetype_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Income", "ICL Payment", "MSL Payment","ICL Repayment Burden","MSL Repayment Burden")) +
  scale_shape_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Monthly Income", "ICL Payment", "MSL Payment", "ICL Repayment Burden","MSL Repayment Burden")) +
  scale_color_discrete(name="Legend", breaks=c('MonthlyIncome','ICLPayment',"MSLPayment","rbICL","rbMSL"), labels=c("Monthly Income", "ICL Payment", "MSL Payment","ICL Repayment Burden","MSL Repayment Burden")) +
  
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  scale_x_continuous(breaks=seq(1,12,by=1))+
  xlab("Month") + ylab("Burden as Percentage") 


ggsave("paymentgraph2.png", width=12, height=14, units='cm')
