#########################################################
# PLSC 504 -- Fall 2020
#
# Non-continuous Response Panel Data models
# (including GEEs)
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(lme4)
library(plm)
library(boot)
library(nlme)
library(MASS)
library(plm)
library(pglm)
library(glmmML)
library(bife)
library(censReg)
library(geepack)
library(prais)
library(fixest)
library(pcse)
library(stargazer)

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

#######################
# A little simulation with unit effects and 
# binary outcomes:

set.seed(7222009)

reps<-100
N<-100
T<-100
NT<-N*T

MSlogit<-matrix(data=NA,nrow=reps,ncol=3)
FElogit<-matrix(data=NA,nrow=reps,ncol=3)
MSClogit<-matrix(data=NA,nrow=reps,ncol=3)
FEClogit<-matrix(data=NA,nrow=reps,ncol=3)
FECprobit<-matrix(data=NA,nrow=reps,ncol=3)
REBs<-matrix(data=NA,nrow=reps,ncol=3)

for(i in 1:reps){

 alpha <- rnorm(N)
 alphas <- rep(alpha, 100)
 X <- rnorm(NT) # Uncorrelated X
 XCorr <- 0.5*alphas + 0.5*rnorm(NT) # X, alpha correlated
 D <- rbinom(NT,1,0.5) # binary predictor
 Ystar <- 0 + 1*X + 1*D + alphas # latent Y, Cov(X,alpha)=0
 YCstar <- 0 + 1*XCorr + 1*D + alphas # latent Y, Cov(X,alpha)>0
 
 Y <- rbinom(NT,1,plogis(Ystar))
 YC <-  rbinom(NT,1,plogis(YCstar))
 YPC <-  rbinom(NT,1,pnorm(YCstar))
 
 fool<-glm(Y~X+D,family="binomial")
 foolFE<-glm(Y~X+D+as.factor(alphas),family="binomial")
 foolC<-glm(YC~XCorr+D,family="binomial")
 foolCFE<-glm(YC~XCorr+D+as.factor(alphas),family="binomial")
 probitFEC<- foolCFE<-glm(YPC~XCorr+D+as.factor(alphas),
                      family="binomial"(link="probit"))
 RE<-glmmML(YC~XCorr+D, family="binomial",
                 cluster=as.factor(alphas))
 
 MSlogit[i,]<-fool$coefficients[1:3]
 FElogit[i,]<-foolFE$coefficients[1:3]
 MSClogit[i,]<-foolC$coefficients[1:3]  
 FEClogit[i,]<-foolCFE$coefficients[1:3]  
 FECprobit[i,]<-probitFEC$coefficients[1:3]
 REBs[i,]<-RE$coefficients[1:3]
 
}

pdf("BinaryPanelSimsUncorrAlphas.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(MSlogit[,2]),xlim=c(0.6,1.1),lwd=2,
     lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FElogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topleft",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects","No Fixed Effects"),
       bty="n")
dev.off()

pdf("BinaryPanelSimsCorrAlphas.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(MSClogit[,2]),xlim=c(0.6,2.1),ylim=c(0,12),
     lwd=2,lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FEClogit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects","No Fixed Effects"),
       bty="n")
dev.off()

pdf("BinaryPanelSimsCorrProbitRE.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(REBs[,2]),xlim=c(0.8,1.4),ylim=c(0,12),
     lwd=2,lty=3,col="red",main="",
     xlab="Estimated Betas (True value = 1.0)")
lines(density(FECprobit[,2]),lwd=2)
abline(v=1,lty=2,lwd=2)
legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
       legend=c("Fixed Effects Probit","Random Effects Logit"),
       bty="n")
dev.off()

#######################
# Segal data...

URL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/SegalVotes.csv"
temp<-getURL(URL)
Segal<-read.csv(textConnection(temp))
rm(temp,URL)

summary(Segal)

# Basic logit:

SegalLogit<-glm(vote~warrant+house+person+business+car+us+
                    except+justideo,data=Segal,family="binomial")
summary(SegalLogit)


# Fixed Effects logit model:

SegalFEL<-bife(vote~warrant+house+person+business+car+us+
              except | justid,data=Segal,
              model="logit")
summary(SegalFEL)

# Alternative:
# 
# SegalFE<-glmmboot(vote~warrant+house+person+business+car+us+
#          except,data=Segal,family="binomial",
#          cluster=justid)
# summary(SegalFE)

# Fixed Effects probit model:
# 
# SegalFEP<-bife(vote~warrant+house+person+business+car+us+
#                        except | justid,data=Segal,
#                model="probit")
# summary(SegalFEP)

# Random-Effects:

SegalRE<-glmmML(vote~warrant+house+person+business+car+us+
                except+justideo,data=Segal,family="binomial",
                cluster=justid)
summary(SegalRE)

Bs <- data.frame(Logit = SegalLogit$coefficients,
                 FEs = c(NA,SegalFEL$coefficients,NA),
                 REs = SegalRE$coefficients,
                 row.names = names(SegalLogit$coefficients))
Bs

# The following doesn't work:
#
# BinaryTable <- stargazer(SegalLogit,SegalFE,SegalRE,
#                     title="SCOTUS Models",
#                     column.separate=c(1,1,1),align=TRUE,
#                     dep.var.labels.include=FALSE,
#                     dep.var.caption="",
#                     covariate.labels=c("POLITY","POLITY Squared",
#                                        "ln(GDP)","Monarch","Cold War"),
#                     header=FALSE,model.names=FALSE,
#                     model.numbers=FALSE,multicolumn=FALSE,
#                     object.names=TRUE,notes.label="",
#                     out="SegalModels.tex")

#########################################
# Event counts:

# Get SFTF data:

URL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/SFTF.csv"
temp<-getURL(URL)
SFTF<-read.csv(textConnection(temp))
rm(temp,URL)

summary(SFTF)
pdim(SFTF)

pdf("CIOB-Histogram.pdf",7,6)
par(mar=c(4,4,2,2))
with(SFTF, hist(ciob,xlab="Number of Organizations",
                ylab="Count",col="grey68",main=" "))
abline(v=mean(SFTF$ciob),lty=2,lwd=2)
legend("topright",bty="n",
       legend=paste0("Mean = ",round(mean(SFTF$ciob),2)))
dev.off()

# Basic Poisson (no panel effects...)

Poisson<-glm(ciob~POLITY+unuurbpc+poldurab+I(year-1900),
             data=SFTF,family="poisson")
summary(Poisson)

# Fixed effects Poisson:

Poisson.FE<-pglm(ciob~POLITY+unuurbpc+poldurab+I(year-1900),
                 data=SFTF,family="poisson",effect="individual",
                 model="within",index="countryid")
summary(Poisson.FE)

# alternative, "brute force":
#
#Poisson.FE.alt<-glm(ciob~POLITY+unuurbpc+poldurab+I(year-1900)+
#                  as.factor(countryid),data=SFTF,family="poisson")

# alternative, using -fixest-:
# 
# Poisson.FE2<-feglm(ciob~POLITY+unuurbpc+poldurab+
#                   I(year-1900) | countryid,
#                   data=SFTF,family="poisson")
# 
# summary(Poisson.FE2,cluster="countryid")

# Random effects Poisson

# library(lme4)
Poisson.RE<-glmer(ciob~POLITY+unuurbpc+poldurab+I(year-1900)+
                    (1|countryid),data=SFTF,family="poisson")
summary(Poisson.RE)

# Alternative RE Poisson, using glmmML:

# library(glmmML)
# Poisson.RE2<-glmmML(ciob~POLITY+unuurbpc+poldurab+I(year-1900),
#                        data=SFTF,cluster=countryid,
#                        family="poisson")
# summary(Poisson.RE2)

# Another RE Poisson, using pglm (slightly different from the
# previous two):
# 
Poisson.RE3<-pglm(ciob~POLITY+unuurbpc+poldurab+I(year-1900),
                       data=SFTF,effect="individual",
                       model="random",family="poisson",
                       index="countryid")
summary(Poisson.RE3)

# Compare coefficients:

BPs <- data.frame(Poisson = Poisson$coefficients,
                 FEs = c(NA,Poisson.FE$estimate),
                 REs = Poisson.RE@beta,
                 row.names = names(Poisson$coefficients))
BPs

# Basic negative binomial:

NB<-glm.nb(ciob~POLITY+unuurbpc+poldurab+I(year-1900),
           data=SFTF)
summary(NB)

# Negative binomial with fixed effects:

NB.FE<-fenegbin(ciob~POLITY+unuurbpc+poldurab+
             I(year-1900) | countryid,data=SFTF,
             theta.init=0.1)
summary(NB.FE,cluster="countryid")

# Negative binomial with random effects (can also use
# -pglm-, in theory...):

NB.RE<-glmer.nb(ciob~POLITY+unuurbpc+poldurab+
                I(year-1900)+(1|countryid),data=SFTF,
                verbose=TRUE)
summary(NB.RE)
isSingular(NB.RE)

# Compare coefficients:

BNBs <- data.frame(NegBin = c(NB$coefficients,NB$theta),
                 NegBinFEs = c(NA,NB.FE$coefficients),
                 NegBinREs = c(NB.RE@beta,getME(NB.RE,"glmer.nb.theta")),
                 row.names = c(names(NB$coefficients),"theta"))
BNBs

# The following does not work: stargazer doesn't accept 
# pglm objects yet...
#
# Table1 <- stargazer(Poisson,Poisson.RE,Poisson.FE,
#                     title="Models of IO Membership",
#                     column.separate=c(1,1),align=TRUE,
#                     dep.var.labels.include=FALSE,
#                     dep.var.caption="",
#                     covariate.labels=c("POLITY","Urban Percentage",
#                                        "Regime Durability","Trend (1901=1)"),
#                     header=FALSE,model.names=FALSE,
#                     model.numbers=FALSE,multicolumn=FALSE,
#                     object.names=TRUE,notes.label="",
#                     out="PanelPoisTable.tex")


##############################################
# Generalized Estimating Equations (GEEs):
#######################
# Bush approval data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/BushApproval.csv")
Bush <- read.csv(text = url) 
rm(url)

summary(Bush)
pdim(Bush)

# Independence:

GEE.IND<-geeglm(approval~partyid+perfin+nateco+age+educ+class+
                  nonwhite+female,data=Bush,id=idno,family=gaussian,
                corstr="independence")
summary(GEE.IND)

# Compare to GLM:

GLM <- glm(approval~partyid+perfin+nateco+age+educ+class+
             nonwhite+female,data=Bush,family=gaussian)

# Coefficients:

cbind(GEE.IND$coefficients,GLM$coefficients)

# Standard Errors:
cbind(sqrt(diag(GEE.IND$geese$vbeta.naiv)),sqrt(diag(vcov(GLM))))

# Exchangeable correlation:

GEE.EXC<-geeglm(approval~partyid+perfin+nateco+age+educ+class+
                  nonwhite+female,data=Bush,id=idno,family=gaussian,
                corstr="exchangeable")
summary(GEE.EXC)

# AR(a) correlation:

GEE.AR1<-geeglm(approval~partyid+perfin+nateco+age+educ+class+
                  nonwhite+female,data=Bush,id=idno,family=gaussian,
                corstr="ar1")
summary(GEE.AR1)

# Unstructured correlation:

GEE.UNSTR<-geeglm(approval~partyid+perfin+nateco+age+educ+class+
                    nonwhite+female,data=Bush,id=idno,family=gaussian,
                  corstr="unstructured")
summary(GEE.UNSTR)

# Plot the betas / SEs:

library(car)
betas<-cbind(GEE.IND$coefficients,GEE.EXC$coefficients,
             GEE.AR1$coefficients,
             GEE.UNSTR$coefficients)

pdf("GEEBetasR.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(betas[-1,],smooth=FALSE,
                  var.labels=c("Ind","Exch","AR1","Unstr"),
                  diagonal="none")
dev.off()

# SEs:

ses<-cbind(sqrt(diag(GEE.IND$geese$vbeta)),
           sqrt(diag(GEE.EXC$geese$vbeta)),
           sqrt(diag(GEE.AR1$geese$vbeta)),
           sqrt(diag(GEE.UNSTR$geese$vbeta)))

pdf("GEE-SEsR.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(ses[-1,],smooth=FALSE,
                  var.labels=c("Ind","Exch","AR1","Unstr"),
                  diagonal="none")
dev.off()
