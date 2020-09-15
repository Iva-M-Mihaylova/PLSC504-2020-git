#########################################################
# PLSC 504 -- Fall 2020
#
# Models for Endogenous Sample Selection, and
# introduction to potential outcomes / causal
# inference...
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(lme4)
library(plm)
library(gtools)
library(plyr)
library(texreg)
library(statmod)
library(sampleSelection)

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#######################
# Amici data

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/SampleSelectionExample.csv")
SCOTUS <- read.csv(text = url) 
rm(url)

summary(SCOTUS)

pdf("LnAmiciHistogram.pdf",6,5)
par(mar=c(4,4,2,2))
hist(SCOTUS$LnAmici,main="",col="lightblue",
     xlab="Log(Number of Amici Filed)")
dev.off()

# OLS:

OLS<-lm(LnAmici~Year+USPartic+MultipleLegal+SCscore,data=SCOTUS)
summary(OLS)

# Probit:

SCOTUS$D<-SCOTUS$Amici>0
probit<-glm(D~Year+USPartic+SCscore+MultipleLegal,data=SCOTUS,
            family=binomial(link="probit"))
summary(probit)

# Two-Step

SCOTUS$IMR<-((1/sqrt(2*pi))*exp(-((probit$linear.predictors)^2/2))) / 
  pnorm(probit$linear.predictors)
OLS.2step<-lm(LnAmici~Year+USPartic+MultipleLegal+SCscore+IMR,data=SCOTUS)
summary(OLS.2step)

# Comparing the coefficients:
OLSBs<-OLS$coefficients[2:5]
HeckBs<-OLS.2step$coefficients[2:5]

pdf("Notes/OLSvsHeckmanBs.pdf",7,6)
par(mar=c(4,4,2,2))
plot(seq(1:4),HeckBs,xaxt="n",pch=19,col="darkgreen",
     xlab="Covariate",ylab="Estimate")
points(seq(1:4),OLSBs,pch=17,col="red")
abline(h=0,lty=2)
arrows(seq(1:4),OLSBs,seq(1:4),HeckBs,length=0.1)
axis(1,at=seq(1:4),labels=names(OLSBs))
legend("topleft",bty="n",pch=c(17,19),col=c("red","darkgreen"),
       legend=c("OLS Estimates","Two-Step Heckman Estimates"))
dev.off()

# Two-step using "heckit" - poor specification

heckman2S<-heckit(D~Year+USPartic+SCscore+MultipleLegal, LnAmici~Year+USPartic
                  +SCscore+MultipleLegal,data=SCOTUS,
                  method="2step")
summary(heckman2S)

# Plot of IMRs:

pdf("SCOTUS-IMR.pdf",5,4)
par(mar=c(4,4,2,2))
plot(probit$linear.predictors,SCOTUS$IMR,pch=20,
     xlab="Probit Linear Predictors",
     ylab="Inverse Mills' Ratio")
dev.off()

# MLEs (poor specification)

heckmanML<-heckit(D~Year+USPartic+SCscore+MultipleLegal, 
                  LnAmici~Year+USPartic+SCscore+MultipleLegal,
                  data=SCOTUS,method="ml")
summary(heckmanML)

# MLEs ("better" specification)

betterML<-heckit(D~Year+USPartic+SCscore+MultipleLegal+SGAmicus, 
                 LnAmici~Year+USPartic+SCscore+MultipleLegal,
                 data=SCOTUS,method="ml")
summary(betterML)

###########################################
# Potential outcomes:

Cov0<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,10,12,8,10,12),
                 Y1=c(10,12,14,10,12,14),
                 Ydiff=rep(2,6),
                 Y=c(8,10,12,10,12,14))

with(Cov0, t.test(Y~W))

PosCov<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,8,10,10,12,12),
                 Y1=c(10,10,12,12,14,14),
                 Ydiff=rep(2,6),
                 Y=c(8,8,10,12,14,14))

with(PosCov, t.test(Y~W))

NegCov<-data.frame(i=seq(1:6),
                   W=c(rep(0,3),rep(1,3)),
                   Y0=c(12,12,10,10,8,8),
                   Y1=c(14,14,12,12,10,10),
                   Ydiff=rep(2,6),
                   Y=c(12,12,10,12,10,10))

with(NegCov, t.test(Y~W))

# Not run:

Varying<-data.frame(i=seq(1:6),
                 W=c(rep(0,3),rep(1,3)),
                 Y0=c(8,10,12,8,10,12),
                 Y1=c(9,11,14,9,11,14),
                 Ydiff=c(1,2,3,1,2,3),
                 Y=c(8,10,12,9,11,14))

with(Varying, t.test(Y~W))

#########################################
# Bigger simulation (not in slides):

reps <- 1000 # number of sims
N<-1000      # sample size

ATE.U <- numeric(reps) # ATEs w/no confounding
ATE.C <- numeric(reps) # ATEs w/confounding by X
ATE.A <- numeric(reps) # Confounded + adjusted ATEs 

set.seed(7222009)

for(i in 1:reps) {
  W0 <- rep(0,N)
  W1 <- rep(1,N)
  Y0 <- 0 + 1*W0 + rnorm(N) # counterfactual Y for W=0; ATE=1
  Y1 <- 0 + 1*W1 + rnorm(N) # counterfactual Y for W=1; ATE=1
  X <- rnorm(N) # independent thing, for now
  W  <- numeric(N)
  Y  <- numeric(N)
  for(j in 1:N){
   Y[j] <- ifelse(X[j]<0,Y0[j],Y1[j])
   W[j] <- ifelse(X[j]<0,W0[j],W1[j])
   } # == effectively random selection of W, since Cov(X,Y)=0

  # boxplot(Y~W,ylim=c(-6,6))
  # t.test(Y~W) # works
  foo<-lm(Y~W) # no problem
  ATE.U[i] <- foo$coefficients[2] # stash the estimate
  # summary(lm(Y~W+X)) # also works; X is unnecessary
  #
  # Now make Y correlated with W **and** X:
  #
  W.cor  <- numeric(N)
  Y.cor  <- numeric(N)
  for(j in 1:N){
   Y.cor[j] <- ifelse(X[j]<0,Y0[j]+X[j],Y1[j]+X[j])
   W.cor[j] <- ifelse(Y.cor[j]==Y0[j]+X[j],W0[j],W1[j])
  } # Selection of W with Cov(X,Y)>0
  #
  # cor(X,W.cor)
  # cor(X,Y.cor)
  # boxplot(Y.cor~W.cor,ylim=c(-6,6))
  # t.test(Y.cor~W.cor) # wrong
  bar<-lm(Y.cor~W.cor) # bad upward bias; should be B~=1
  ATE.C[i] <- bar$coefficients[2] # stash the estimate
  baz<-lm(Y.cor~W.cor+X) # controlling for X fixes things
  ATE.A[i] <- baz$coefficients[2] # stash the estimate
}

# Plot the Betas!

pdf("Notes/ConfoundingSims.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(ATE.C),t="l",xlim=c(0.5,3),lwd=2,ylim=c(0,7),
     lty=2,col="red",main="",xlab="Estimated ATE")
lines(density(ATE.U),col="black",lwd=2,lty=1)
lines(density(ATE.A),col="darkgreen",lwd=3,lty=5)
abline(v=1,lty=3)
legend("topright",bty="n",lty=c(1,2,5),lwd=c(2,2,3),cex=0.8,
       col=c("black","red","darkgreen"),legend=c("No Confounding",
                      "Confounding w/o Adjustment",
                      "Confounding with Adjustment"))
dev.off()

