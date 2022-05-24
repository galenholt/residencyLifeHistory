#############################
#Lisi et al. Ecology 2022
#Stream and ocean hydrodynamics mediate partial migration 
#strategies in an amphidromous Hawaiian goby
#script for creation of Figure 1 main text and Appendix  Figure S1.
#############################

#############################################
#Load packages
#############################################
library(AICcmodavg)


#############################################
#Load data /set working directory
#############################################
# setwd("C:/Users/peter/Dropbox/Hawaii1.0.R")

sw<-read.csv("data/lifehistory data fig S1.csv",head=T)
aov.dat<-read.csv("data/fig 1 data.csv",header=T)

#############################################
## Code for figure S1 2009 vs 2011
#############################################

#png(filename = "Figure S1 11vs09propRes.png",width =4.5, height = 4.5, units = "in", pointsize = 12,  res = 600)
par(mfrow=c(1,1),oma=c(3,2,1,0), mar=c(3,2,1,1), xpd=T)
plot(1-sw$sw2009,1-sw$sw2011,pch=21,, bg=1, xlab=NA,ylab=NA,xlim=c(0,1),ylim=c(0,1),yaxs="i", xaxs="i")
clip(0,1,0,1)
abline(0,1,lty=2)
mtext(expression(paste(" Proportion freshwater residents 2011")), side=2, line=2.5, cex=1)
mtext(expression(paste(" Proportion freshwater residents 2009")), side=1, line=2.5, cex=1)
dev.off()

t.test(sw$sw2009,sw$sw2011,paired=TRUE)


#############################################
## Code for ANCOVA hydrology varibility and year as a factor
#############################################

Q90.50<-aov.dat$Q90/aov.dat$Q50# stability ratio of discharge
Q10.90<-aov.dat$Q10/aov.dat$Q90# variability index of stream discharge

mYear<- aov(1-Swprop~year,data=aov.dat)
mCV1<-aov(1-Swprop~CV,data=aov.dat)
mCV2<-aov(1-Swprop~year+CV,data=aov.dat)
mCV3<-aov(1-Swprop~year*CV,data=aov.dat)
mQ10<-aov(1-Swprop~year+Q10,data=aov.dat)
mQ50<-aov(1-Swprop~year+Q50,data=aov.dat)
mQ90<-aov(1-Swprop~year+Q90,data=aov.dat)
mQ95<-aov(1-Swprop~year+Q95,data=aov.dat)
mQ5<-aov(1-Swprop~year+Q5,data=aov.dat)
mQ90.50<-aov(1-Swprop~year+Q90.50,data=aov.dat)
mQ10.90<-aov(1-Swprop~year+Q10.90,data=aov.dat)
models<-list(mYear,mCV1,mCV2,mCV3,mQ10, mQ50,mQ90,mQ95,mQ5,mQ90.50, mQ10.90)
mod.names<-c("mYear","mCV1","mCV2","mCV3","mQ10", "mQ50","mQ90","mQ95","mQ5","mQ90.50", "mQ10.90")
aictab(models,modnames=mod.names, second.ord = T,nobs=NULL)
summary.lm(mCV2)#best fit stats for fig 1a



#############################################
## Code for Fig 1 a
#############################################

#png(filename = "fig.1.png",width =6, height = 3.2, units = "in", pointsize = 9,  res = 600)
par(mfrow=c(1,2),oma=c(3,2,1,0), mar=c(3,2,1,1.5), xpd=T)

plot(aov.dat$CV[aov.dat$year=="nine"],1-aov.dat$Swprop[aov.dat$year=="nine"],pch=16,col="grey40",xlab=NA,ylab=NA,xlim=c(0,4),ylim=c(0,1.1),yaxs="i", xaxs="i")
points(aov.dat$CV[aov.dat$year=="eleven"],1-aov.dat$Swprop[aov.dat$year=="eleven"])
clip(min(aov.dat$CV),3.3, 0,1)
#add regression line using coefficents from mCV2 for 2011
abline(0.985,-0.099)#2011
#using coefficents from mCV2 for 2011
clip(min(life.dat$CV),2.8,0,1)
#add regression line using coefficents from mCV2 for 2009
abline(0.985-0.197,-0.099)#2011
mtext(expression(paste("Discharge variability (daily CV)")), side=1, line=2.5, cex=1)
mtext(expression(paste("Proportion freshwater residency")), side=2, line=2, cex=1)
legend("bottomright", pch=c(1,16), c("2011", "2009"), title="Survey year", bty="n", col=c("black","grey40"))
mtext("a",side=3, adj=-.2, line=.5)

#############################################
## Code for Fig 1 b 
#############################################

Island<-list("Hawai'i","Mau'i","Molokai","O'ahu","Kaua'i") #island names
meanSettle<-c(16.0, 20.2, 17.0, 21.3, 25.6) #mean island scale settlement
sdSettle<-c(2.45, 1.92, 1.27, 2.72, 3.88)#standard deviation in island scale settlement
cvSettle<-c( 0.153, 0.095, 0.075, 0.127, 0.151)#coefficent of variation island scale settlment
delRes<-c(0.217,0.144,0.143,0.212,0.322) # change in island scale settlment 2009 to 2011

m1<-lm(delRes~sdSettle)
m2<-lm(delRes~cvSettle)
m3<-lm(delRes~meanSettle)

models<-list(m1,m2,m3)
mod.names<-c("m1 SD", "m2 CV","m3 mean")
aictab(models,modnames=mod.names, second.ord = T,nobs=NULL)
summary(m1)


plot(sdSettle,delRes,pch=16, xlab=NA,ylab=NA,xlim=c(0,5),ylim=c(0,.4),yaxs="i", xaxs="i")
mtext("b",side=3, adj=-.2, line=0.5)
mtext(expression(paste(Delta,"Observ. residency 2009 vs 2011")), side=2, line=2, cex=1)
mtext(expression(paste("Standard deviation in annual island settlement")), side=1, line=2.5, cex=1)
text(c(2, 2.3, .8, 3.2, 3.5),delRes,Island,cex=.8)
clip(1.2,4.2,0,3.2)
abline(m1)

dev.off()


