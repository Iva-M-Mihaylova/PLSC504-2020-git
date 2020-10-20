##############################################
# PLSC 504 -- Fall 2019
#
# Survival Analysis: Introduction + Parametric
##############################################
# Options:

options(scipen=7)
options(digits=4)
par(mar=c(4,4,2,2))

# Set as necessary:

setwd("~/Dropbox (Personal)/PLSC 504/Notes")

# Packages:

library(RCurl)
library(gtools)
library(plyr)
library(nnet)
library(mstate)
library(texreg)
library(survival)
library(eha)
library(rms)
library(flexsurv)
library(pscl)
library(smcure)
library(nltm)
library(coxme)

options(scipen = 5) # bias against scientific notation
options(digits = 2) # show fewer decimal places

# Read KABL data:

KABLURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/KABL.csv"
temp<-getURL(KABLURL)
KABL<-read.csv(textConnection(temp))

# Create survival object:

KABL.S<-Surv(KABL$durat,KABL$ciep12)

# Survival curve estimates:

KABL.fit<-survfit(KABL.S~1)

# Plot:

pdf("KABLKM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.fit,xlab="Time (in months)",
     ylab="Survival Estimate",
     lwd=c(3,1,1))
dev.off()

# Estimates of H(t):

KABL.FH<-survfit(KABL.S~1,type="fleming")

# Plot:

pdf("KABLNA.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.FH$time,-log(KABL.FH$surv),t="l",lwd=3,lty=1,
     xlab="Time (in months)",ylab="Cumulative Hazard",
     ylim=c(0,2.5))
lines(KABL.FH$time,-log(KABL.FH$upper),t="l",lwd=2,lty=2)
lines(KABL.FH$time,-log(KABL.FH$lower),t="l",lwd=2,lty=2)
points(KABL.FH$time[KABL.FH$n.censor>0],
       -log(KABL.FH$surv[KABL.FH$n.censor>0]),pch=19)
dev.off()

# Alternatively, w/no CIs...:
#
# NAalen<-cumsum(KABL.fit$n.event / KABL.fit$n.risk) 
# Ht<- -log(KABL.fit$surv) # Alternative estimate
# 
# pdf("KABLNA.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(NAalen~KABL.fit$time,t="l",lwd=3,xlab="Time (in months)",
#      ylab="Cumulative Hazard Estimate")
# points(KABL.fit$time[KABL.fit$n.censor>0],
#        NAalen[KABL.fit$n.censor>0],pch=4)
# dev.off()

# Log-rank test:

survdiff(KABL.S~invest,data=KABL,rho=0)

# Survival curve comparison:

KABL.fit2<-survfit(KABL.S~invest,data=KABL)

# Plot:

pdf("KABL-By-Invest.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.fit2,conf.int=TRUE,lty=c(1,2),lwd=c(3,1,1,3,1,1),
     col=c("black","red"),ylab="Survival Estimate",
     xlab="Time (in months)", mark.time=FALSE)
legend("topright",inset=.02,
       c("No Investiture Requirement","Investiture Requirement"),
       lty=c(1,2),lwd=c(3,3),col=c("black","red"),bty="n")
dev.off()

#####################################################
# Parametric regression models...

# Survival object (again):

KABL.S<-Surv(KABL$durat,KABL$ciep12)

# KM plot:

KABL.KapM<-survfit(KABL.S~1)

pdf("KABLKapM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.KapM,lwd=c(3,1,1),mark.time=FALSE,
     ylab="Survival Probability",xlab="Time (in months)")
dev.off()

# Covariates:

xvars<-c("fract","polar","format","invest","numst2","eltime2","caretk2")
MODEL<-as.formula(paste(paste("KABL.S ~ ", paste(xvars,collapse="+"))))

# Exponential:

KABL.exp.AFT<-survreg(MODEL,data=KABL,dist="exponential")

KABL.exp.PH<-(-KABL.exp.AFT$coefficients)

KABL.exp.HRs<-exp(-KABL.exp.AFT$coefficients)

# Comparing predicted survival curves:
#
# Refit model using flexsurvreg...

KABL.exp<-flexsurvreg(MODEL,data=KABL,dist="exp")
FakeInvest<-t(c(mean(KABL$fract),mean(KABL$polar),mean(KABL$format),1,
                mean(KABL$numst2),mean(KABL$eltime2),mean(KABL$caretk2)))
FakeNoInvest<-t(c(mean(KABL$fract),mean(KABL$polar),mean(KABL$format),0,
                  mean(KABL$numst2),mean(KABL$eltime2),mean(KABL$caretk2)))
colnames(FakeInvest)<-xvars
colnames(FakeNoInvest)<-xvars

# Plot:

pdf("ExpSurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.exp,FakeInvest,mark.time=FALSE,col.obs="black",
     lty.obs=c(0,0,0),xlab="Time (in months)",ylab="Survival Probability")
lines(KABL.exp,FakeNoInvest,mark.time=FALSE,col.obs="black",
      lty.obs=c(0,0,0),col=c(rep("green",times=3)))
legend("topright",inset=0.05,bty="n",
       c("Investiture Requirement","No Investiture Requirement"),
       lty=c(1,1),lwd=c(2,2),col=c("red","green"))
dev.off()


# Weibulls...
#
# Plot of various hazard shapes:

t<-cbind(1:60,1:60,1:60)
P<-c(0.5,1,2)
WeibullHs<-t(apply(t,1,function(t) 0.02*P*((0.02*t)^(P-1))))
WeibullSs<-t(apply(t,1,function(t) (exp(-0.02*t))^P))

# Plots:

pdf("WeibullHSims.pdf",6,5)
par(mar=c(4,4,2,2))
plot(t[,1],WeibullHs[,1],t="l",lwd=3,lty=1,col="green",
     xlab="Time",ylab="Hazard",ylim=c(0,0.08))
lines(t[,2],WeibullHs[,2],t="l",lwd=3,lty=2,col="black")
lines(t[,3],WeibullHs[,3],t="l",lwd=3,lty=3,col="red")
legend("topright",inset=.02,
       c("p = 0.5","p = 1.0","p = 2.0"),
       lty=c(1,2,3),lwd=c(3,3,3),col=c("green","black","red"),
       cex=1.2,bty="n")
dev.off()

pdf("WeibullSSims.pdf",6,5)
par(mar=c(4,4,2,2))
plot(t[,1],WeibullSs[,1],t="l",lwd=3,lty=1,col="green",
     xlab="Time",ylab="Survival Probability",ylim=c(0,1))
lines(t[,2],WeibullSs[,2],t="l",lwd=3,lty=2,col="black")
lines(t[,3],WeibullSs[,3],t="l",lwd=3,lty=3,col="red")
legend("bottomleft",inset=.02,
       c("p = 0.5","p = 1.0","p = 2.0"),
       lty=c(1,2,3),lwd=c(3,3,3),col=c("green","black","red"),
       cex=1.2,bty="n")
dev.off()

# Weibull KABL:

KABL.weib.AFT<-survreg(MODEL,data=KABL,dist="weibull")

KABL.weib.PH<-(-KABL.weib.AFT$coefficients)/(KABL.weib.AFT$scale) 

KABL.weib.HRs<-exp(KABL.weib.PH)

# Comparing Weibull survival curves

KABL.weib.Ihat<-predict(KABL.weib.AFT,newdata=as.data.frame(FakeInvest),
                        type="quantile",se.fit=TRUE,p=seq(.01,.99,by=.01))

KABL.weib.NoIhat<-predict(KABL.weib.AFT,newdata=as.data.frame(FakeNoInvest),
                          type="quantile",se.fit=TRUE,p=seq(.01,.99,by=.01))

# Plot:

pdf("WeibSurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(KABL.weib.NoIhat$fit,seq(.99,.01,by=-.01),t="l",lwd=3,col="green",
     xlab="Time (in months)",ylab="Survival Probability")
lines(KABL.weib.Ihat$fit,seq(.99,.01,by=-.01),lwd=3,col="red")
lines(KABL.weib.NoIhat$fit+1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="green")
lines(KABL.weib.NoIhat$fit-1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="green")
lines(KABL.weib.Ihat$fit+1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="red")
lines(KABL.weib.Ihat$fit-1.96*(KABL.weib.NoIhat$se),
      seq(.99,.01,by=-.01),lty=2,lwd=1,col="red")
legend("topright",inset=0.05,bty="n",
       c("Investiture Requirement","No Investiture Requirement"),
       lty=c(1,1),lwd=c(2,2),col=c("red","green"))
dev.off()


##################################################
# O'Neal/Russett data & examples:

ORURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2019-git/master/Data/OR.csv"
temp<-getURL(ORURL)
OR<-read.csv(textConnection(temp))
rm(temp)

summary(OR)

# Surv object...

OR.S<-Surv(OR$start,OR$stop,OR$dispute,type=c('counting'))
OR.KM<-survfit(OR.S~1)

# Kaplan-Meier:

pdf("ORKM.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OR.KM,mark.time=FALSE,lwd=c(2,1,1),
     xlab="Time (in years)",ylab="Survival Probability")
dev.off()

# Cox model w/OR data (Breslow):

ORCox.br<-coxph(OR.S~allies+contig+capratio+growth+democracy+trade,
                data=OR,na.action=na.exclude, method="breslow")

# Scaling covariates:

OR$growthPct<-OR$growth*100
summary(coxph(OR.S~allies+contig+capratio+growthPct+democracy+trade,
              data=OR,na.action=na.exclude, method="breslow"))

# Baseline (cumulative) hazard:

OR.BH<-basehaz(ORCox.br,centered=FALSE)

# Plot:

pdf("ORCoxBaseH.pdf",10,5)
par(mar=c(4,4,2,2))
plot(OR.BH$time,OR.BH$hazard,t="l",lwd=4,col="red",
     xlab="Time (in years)",ylab="Baseline Integrated Hazard")
lines(abline(lm(OR.BH$hazard~0+OR.BH$time),lty=2,lwd=2))
legend("bottomright",inset=0.02,bty="n",
       c("Baseline Hazard","Linear Fit"),lty=c(1,2),
       lwd=c(4,2),col=c("red","black"))
dev.off()

# Comparing survival curves:

FakeContig<-as.data.frame(t(c(mean(OR$allies),1,mean(OR$capratio),mean(OR$growth),
                              mean(OR$democracy),mean(OR$trade))))
FakeApart<-as.data.frame(t(c(mean(OR$allies),0,mean(OR$capratio),mean(OR$growth),
                             mean(OR$democracy),mean(OR$trade))))
colnames(FakeContig)<-c("allies","contig","capratio","growth",
                        "democracy","trade")
colnames(FakeApart)<-c("allies","contig","capratio","growth",
                       "democracy","trade")

FCHat<-survfit(ORCox.br,FakeContig)
FAHat<-survfit(ORCox.br,FakeApart)

# Plot:

pdf("SurvCompare.pdf",6,5)
par(mar=c(4,4,2,2))
plot(FAHat,col=c(rep("black",times=3)),lwd=c(3,1,1),lty=c(1,2,2),
     xlab="Time (in years)",ylab="Survival Probability",
     mark.time=FALSE)
par(new=TRUE)
plot(FCHat,col=c(rep("red",times=3)),lwd=c(3,1,1),lty=c(2,2,2),
     mark.time=FALSE)
legend("bottomleft",inset=0.02,bty="n",
       c("Non-Contiguous","Contiguous"),lty=c(1,2),
       lwd=c(3,3),col=c("black","red"))
dev.off()

# Ties:

set.seed(7222009)
Data<-as.data.frame(cbind(c(rep(1,times=400)),
                          c(rep(c(0,1),times=200))))
colnames(Data)<-c("C","X")
Data$T<-rexp(400,exp(0+1*Data$X)) # B = 1.0
Data.S<-Surv(Data$T,Data$C)
plot(survfit(Data.S~Data$X),col=c("black","red"),
     xlab="Time",ylab="Survival") # plot

D.br<-coxph(Data.S~X,data=Data,method="breslow")
D.ef<-coxph(Data.S~X,data=Data,method="efron")
D.ex<-coxph(Data.S~X,data=Data,method="exact")

D.Bs<-c(D.br$coefficients,D.ef$coefficients,D.ex$coefficients)
Dlab<-c("Breslow","Efron","Exact")

# Plot:

pdf("SimNoTies.pdf",12,5)
par(mar=c(4,4,2,2))
dotchart(D.Bs,labels=Dlab,pch=19,cex=1.8,xlab="Estimated Beta")
dev.off()

# Now add ties via rounding (nearest multiple of 5):

Data$Tied<-round(Data$T,0)

DataT.S<-Surv(Data$Tied,Data$C)

DT.br<-coxph(DataT.S~X,data=Data,method="breslow")
DT.ef<-coxph(DataT.S~X,data=Data,method="efron")
DT.ex<-coxph(DataT.S~X,data=Data,method="exact")
DT.Bs<-c(DT.br$coefficients,DT.ef$coefficients,DT.ex$coefficients)

# Plot:

pdf("SimTies.pdf",12,5)
par(mar=c(4,4,2,2))
dotchart(DT.Bs,labels=Dlab,pch=19,xlab="Estimated Beta",
         cex=1.8)
abline(v=D.ex$coefficients,lty=2,lwd=2)
dev.off()

#############################################
# Discrete-Time models
#
# ONeal-Russett data redux...

ORURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2019-git/master/Data/OR.csv"
temp<-getURL(ORURL)
OR<-read.csv(textConnection(temp))
rm(temp)

# Logit models of disputes...

OR.logit<-glm(dispute~allies+contig+capratio+growth+democracy+trade,
              data=OR,na.action=na.exclude,family="binomial")

OR$duration<-OR$stop

OR.trend<-glm(dispute~allies+contig+capratio+growth+democracy+trade
              +duration,data=OR,na.action=na.exclude,family="binomial")

OR$d2<-OR$duration^2*0.1
OR$d3<-OR$duration^3*0.01
OR$d4<-OR$duration^4*0.001

OR.P4<-glm(dispute~allies+contig+capratio+growth+democracy+trade
           +duration+d2+d3+d4,data=OR,na.action=na.exclude,
           family="binomial")

P4test<-anova(OR.logit,OR.P4,test="Chisq")
P4test

OR.dummy<-glm(dispute~allies+contig+capratio+growth+democracy+trade
              +as.factor(duration),data=OR,na.action=na.exclude,
              family="binomial")

Test.Dummies<-anova(OR.logit,OR.dummy,test="Chisq")
Test.Dummies


# Predicted probabilities:

Xhats<-as.data.frame(t(c(mean(OR$allies),mean(OR$contig),mean(OR$capratio),
                         mean(OR$growth),mean(OR$democracy),mean(OR$trade))))
Xhats<-Xhats[rep(1:nrow(Xhats),each=max(OR$duration)),]
Xhats$duration<-1:max(OR$duration)        
Xhats$d2<-Xhats$duration^2*0.1
Xhats$d3<-Xhats$duration^3*0.01
Xhats$d4<-Xhats$duration^4*0.001
colnames(Xhats)<-c("allies","contig","capratio","growth","democracy",
                   "trade","duration","d2","d3","d4")

Hat.logit<-predict(OR.logit,Xhats,type="response")
Hat.trend<-predict(OR.trend,Xhats,type="response")
Hat.P4<-predict(OR.P4,Xhats,type="response")
Hat.dummy<-predict(OR.dummy,Xhats,type="response")

pdf("DiscreteHats.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Xhats$duration,Hat.logit,ylim=c(0,0.04),t="l",lwd=3,col="black",
     xlab="Time (in years)",ylab="Predicted Probability")
lines(Xhats$duration,Hat.trend,lwd=3,lty=2,col="blue")
lines(Xhats$duration,Hat.P4,lwd=3,lty=3,col="green")
lines(Xhats$duration,Hat.dummy,lwd=3,lty=4,col="red")
legend("topright",inset=0.05,bty="n",
       c("No Duration Dependence","Linear Dependence",
         "Fourth-Degree Polynomial","Duration Dummies"),lty=c(1,2,3,4),
       lwd=c(3,3,3,3),col=c("black","blue","green","red"))
dev.off()

# Cox/Poisson equivalence:

OR.Cox<-coxph(Surv(OR$start,OR$stop,OR$dispute)~allies+contig+capratio+
                 growth+democracy+trade,data=OR,method="breslow")

OR.Poisson<-glm(dispute~allies+contig+capratio+growth+democracy+trade
                +as.factor(duration),data=OR,na.action=na.exclude,
                family="poisson")

pdf("CoxPoisson.pdf",10,5)
plot(OR.Cox$coefficients[1:6],OR.Poisson$coefficients[2:7],pch=19,
     xlab="Cox Estimates",ylab="Poisson Estimates",
     xlim=c(-4,1.5),ylim=c(-4,1.5))
abline(0,1)
text(OR.Cox$coefficients[1:6],OR.Poisson$coefficients[2:7],
     labels=colnames(Xhats[1:6]),pos=4,cex=0.8)
legend("bottomright",bty="n",inset=0.02,c("Line is 45-degree line"))
dev.off()

