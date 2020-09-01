################################################
# Code for PLSC 504 - Fall 2020
#
# Binary- and Nominal-Response Regression 
# Models: Extensions (mostly)
#
################################################
# Packages, etc.:

require(RCurl)
# install.packages("elrm") # uncomment as needed -- see below
# require(erlm) # note: package deprecated
# install.packages("zeligverse") # uncomment as needed
devtools::install_version("Zelig", version="5.1.6.1",
                          repos="http://cran.us.r-project.org")
require(Zelig) # Note: Zelig can be weird to load, sometimes...
# install.packages("logistf") # uncomment as needed
require(logistf)
library(VGAM)
# install.packages("mlogit") # uncomment as needed
library(mlogit)
# install.packages("aod") # uncomment as needed
library(aod)

# Set working directory:
setwd("~/Dropbox (Personal)/PLSC 504") # <- change as needed

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

####################################
# Separation:

# Table

Yeas<-t(c(rep(0,times=212),rep(1,times=219)))
Dems<-t(c(rep(0,times=178),rep(1,times=253)))
table(Yeas,Dems)

# Simulated Logits:

set.seed(7222009)
X<-runif(100,min=-5,max=5)
X<-X[order(X)]
Z<-runif(100,min=-5,max=5)
Y<-ifelse(plogis(X+Z)>0.5,1,0)
Y2<-ifelse(plogis(X+0.5*Z)>0.5,1,0)
Y3<-ifelse(plogis(X+0.1*Z)>0.5,1,0)
Ysep<-ifelse(plogis(X)>0.5,1,0)
Yfit<-glm(Y~X,family="binomial")
Y2fit<-glm(Y2~X,family="binomial")
Y3fit<-glm(Y3~X,family="binomial")
Ysepfit<-glm(Ysep~X,family="binomial")

# Plots:

pdf("Separation.pdf",8,7)
par(mar=c(4,4,2,2))
par(mfrow=c(2,2))
plot(X,Y,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Yfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Yfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Yfit))[4],digits=2))))
plot(X,Y2,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y2fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y2fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y2fit))[4],digits=2))))
plot(X,Y3,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y3fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y3fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y3fit))[4],digits=2))))
plot(X,Ysep,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Ysepfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Ysepfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Ysepfit))[4],digits=2))))
dev.off()

# Toy data:

rm(X,Y,Z)
set.seed(7222009)
Z<-rnorm(500)
W<-rnorm(500)
Y<-rbinom(500,size=1,prob=plogis((0.2+0.5*W-0.5*Z)))
X<-rbinom(500,1,(pnorm(Z)))
X<-ifelse(Y==0,0,X)

summary(glm(Y~W+Z+X,family="binomial"))
summary(glm(Y~W+Z+X,family="binomial",maxit=100,epsilon=1e-16))

# data<-as.data.frame(cbind(W,X,Y,Z))
# write.dta(data,"SepSim.dta") # for the Stata illustration

# Exact logistic regression...
# DO NOT ACTUALLY RUN THIS CODE -- your computer will likely
# freeze up. It's here for example purposes only.
#
# First, install the legacy package "elrm":
#
# install.packages("devtools") <-- uncomment if needed
# devtools::install_version("elrm", "1.2.4")
# library(elrm)

# df <- data.frame(one=1,Y=Y,W=W,Z=Z,X=X)
# toy.elrm <- elrm(Y/one~W+Z+X,interest=~X,dataset=df,
#                  r=4,iter=5000,burnIn=1000)
# summary(toy.elrm)

#########################
# Pets-as-family data example:

PetsURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/Pets.csv"
temp<-getURL(PetsURL)
Pets<-read.csv(textConnection(temp))
rm(temp)

Pets.1<-glm(petfamily~female+as.factor(married)+as.factor(partyid)
            +as.factor(education),data=Pets,family=binomial)
summary(Pets.1)

Pets.2<-glm(petfamily~female+as.factor(married)*female+as.factor(partyid)+
              as.factor(education),data=Pets,family=binomial)
summary(Pets.2)

with(Pets, xtabs(~petfamily+as.factor(married)+female))

Pets.Firth<-logistf(petfamily~female+
                      as.factor(married)*female+as.factor(partyid)+
                      as.factor(education),data=Pets)
Pets.Firth

# Profile Firth profile likelihood:

Pets.profile<-profile(Pets.Firth,
     variable="femaleMale:as.factor(married)Widowed",
     firth=TRUE)

# Plot it:

pdf("Notes/PetsProfileL.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Pets.profile)
abline(v=Pets.Firth$coefficients[15],lty=2,lwd=2)
abline(v=0,lty=3,lwd=1)
dev.off()

########################
# Rare Events:
#
# Rare Event bias figure:

N<-c(.1,5,seq(10,10000,by=10))
p4 <- (0.4-0.5) / (N*(0.4)*(1-0.4))
p1 <- (0.1-0.5) / (N*(0.1)*(1-0.1))
p01 <- (0.01-0.5) / (N*(0.01)*(1-0.01))
p001 <- (0.001-0.5) / (N*(0.001)*(1-0.001))

pdf("Notes/RareEventBiasR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(N,p4,t="l",lwd=2,lty=1,col="black",xlab="N",
      ylab="Bias in the Intercept",ylim=c(-0.5,0))
lines(N,p1,t="l",lwd=2,lty=2,col="green")
lines(N,p01,t="l",lwd=2,lty=3,col="yellow")
lines(N,p001,t="l",lwd=2,lty=4,col="red")
legend("bottomright",bty="n",
        legend=c(expression(paste(bar(pi),"=0.4")),
                 expression(paste(bar(pi),"=0.1")),
                 expression(paste(bar(pi),"=0.01")),
                 expression(paste(bar(pi),"=0.001"))),
       col=c("black","green","yellow","red"),
       lty=c(1,2,3,4),lwd=2)
dev.off()

# OR/BKT data:

BKTURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/OR.csv"
temp<-getURL(BKTURL)
RE<-read.csv(textConnection(temp))
rm(temp)

# Basic logit:

baselogit<-glm(dispute~democracy+growth+allies+contig+capratio+trade,
               data=RE,family=binomial)
summary(baselogit)

# (Fake) Case-control sampling:

set.seed(7222009)
REones<-RE[RE$dispute==1,]
REzeros<-RE[RE$dispute==0,]
RSzeros<-REzeros[sample(1:nrow(REzeros),1000,replace=FALSE),]
REsample<-data.frame(rbind(REones,RSzeros))
table(REsample$dispute)

sample.logit<-glm(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,family=binomial)
summary(sample.logit)

# RE logit, prior correction...
# 
# Warning: As a rule, this code won't currently (September 
# 2020) work, because Zelig has some serious issues with 
# package dependencies right now. YMMV, etc.

relogit.pc<-zelig(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,model="relogit",tau=405/20448,case.control=c("prior"))
summary(relogit.pc)

# RE logit, weighting correction:

relogit.wc<-zelig(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,model="relogit",tau=405/20448,case.control=c("weighting"))
summary(relogit.wc)

# fin

##################################################
##################################################
# Nominal-Level Responses...
#
# First, the "small" version of the 1992 presidential
# election data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/Election1992small.csv")
nes92<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(nes92)

# Multinomial logits:

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# Conditional Logit...
#
# Reshape the data:

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

# (Re)Fit the MNL model:

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# CL regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation, using the "bigger" dataset:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/Election1992.csv")
BigNES92<-read.csv(text=temp, header=TRUE)
rm(temp)

# Fit model:

NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)

# Some Wald tests:

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Generate "hats":

PickBush<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3], 1,0)
PickWJC<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3], 2, 0)
PickHRP<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2], 3, 0)
OutHat<-PickBush+PickWJC+PickHRP
table(BigNES92$presvote,OutHat)

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

library(car)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",8,6)
par(mfrow=c(1,3))
plot(BigNES92$partyid,Bush,xlab="Party ID")
plot(BigNES92$partyid,Clinton,xlab="Party ID")
plot(BigNES92$partyid,Perot,xlab="Party ID")
par(mfrow=c(1,1))
dev.off()

# Conditional Logit example:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# In-sample predictions:

CLhats<-predict(nes92.clogit,nes92CL)

pdf("InSampleCLHatsR.pdf",7,6)
plot(nes92$FT.Bush,CLhats[,1],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(nes92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,2],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(nes92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(nes92$FT.Bush,CLhats[,1]),lwd=2,col="red")
lines(lowess(nes92$FT.Clinton,CLhats[,2]),lwd=2,col="blue")
lines(lowess(nes92$FT.Perot,CLhats[,3]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()

# HEV, MNP, etc.
#
# Swedish (2002) election data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/Sweden2002.csv")
Sweden<-read.csv(text=temp, header=TRUE)
rm(temp)

library(mlogit)
Sweden.Long<-mlogit.data(Sweden,choice="partychoice",
                         shape="wide")
Sweden.MNL<-mlogit(partychoice~1|female+union+leftright+
                     age,data=Sweden.Long)
summary(Sweden.MNL)

# Restricted model (omitting Social Democrats)

Sweden.MNL.Restr<-mlogit(partychoice~1|female+union+leftright+age,
                         Sweden.Long,alt.subset=c("Conservatives","Liberals","Left Party"))

hmftest(Sweden.MNL,Sweden.MNL.Restr)

# Heteroscedastic logit / HEV:

Sweden.Het<-mlogit(partychoice~1|female+union+leftright+
                     age,data=Sweden.Long,heterosc=TRUE)
summary(Sweden.Het)

# Compare MNL and HEV coefficients:

MNLBetas<-data.frame(MNL = append(coef(Sweden.MNL),rep(NA,times=3)),
                     HEV = coef(Sweden.Het))
fit<-lm(HEV~MNL,data=MNLBetas)
B0<-round(coef(fit)[1],3)
B1<-round(coef(fit)[2],3)
R2<-round(summary(fit)$r.squared,3)

pdf("MNLvsHEV-Betas.pdf",7,6)
par(mar=c(4,4,2,2))
with(MNLBetas, 
     plot(MNL,HEV,pch=19,
          xlab="MNL Estimates",ylab="HEV Estimates",
          xlim=c(-5,20),ylim=c(-2.5,9)))
with(MNLBetas,
     text(MNL,HEV,labels=rownames(MNLBetas),pos=4,cex=0.8))
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(fit)
text(3.7,8.2,labels=paste0("Intercept = ",B0))
text(3.7,7.6,labels=paste0("Slope = ",B1))
text(3.7,7,labels=paste0("R-Squared = ",R2))
arrows(3.7,6.7,7.4,4.6,length=0.1)
dev.off()

# Tests of HEV vs. MNL:

MNL.HEV.Wald <- waldtest(Sweden.Het, heterosc = FALSE) # Wald test
MNL.HEV.Wald
MNL.HEV.LR <- lrtest(Sweden.Het)         # LR test
MNL.HEV.LR
MNL.HEV.Score <- scoretest(Sweden.MNL, heterosc = TRUE)   # score test
MNL.HEV.Score

# Multinomial Probit:

install.packages("MNP") # uncomment as necessary
library(MNP)
Sweden.MNP<-mnp(partychoice~female+union+leftright+age,
                data=Sweden)
summary(Sweden.MNP)

# Alternative, using -mlogit-:
#
# Warning: This takes a long time...

Sweden.MNP2<-mlogit(partychoice~1|female+union+leftright+
                      age,data=Sweden.Long,probit=TRUE)
summary(Sweden.MNP2)

# Predictions:

MNL.hats<-data.frame(MNL.P = fitted(Sweden.MNL,outcome=FALSE)) # MNL
HEV.hats <- data.frame(HEV.P = fitted(Sweden.Het,outcome=FALSE)) # HEV
foo<-predict(Sweden.MNP,type="prob") # MNP
MNP.hats<-data.frame(MNP.P=foo$p)

hats<-cbind(MNL.hats,HEV.hats,MNP.hats) # combine...

# Correlation plot:

CorrMat <- cor(hats,use="pairwise.complete.obs")

#install.packages("ggcorrplot") # uncomment as needed
library(ggcorrplot)

pdf("AllCorrelations.pdf",8,7)
ggcorrplot(CorrMat, hc.order="TRUE",
           lab="TRUE",type="lower",
           legend.title="Correlations")
dev.off()

# whew