########################################################
# PLSC 504 -- Fall 2020
#
# Panel / TSCS data: Introduction, unit effects,
# and dynamics...
#
########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(psych)
library(plyr)
library(lme4)
library(plm)
library(gtools)
library(boot)
library(plyr)
library(texreg)
library(statmod)
library(pscl)
library(stargazer)
library(prais)
library(nlme)
# install.packages("tseries")
library(tseries)
# install.packages("pcse")
library(pcse)
# install.packages("panelView")
library(panelView)
# install.packages("OrthoPanels")
library(OrthoPanels)

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#####################
# Tiny TSCS example:

tinyURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/tinyTSCSexample.txt"
temp<-getURL(tinyURL)
tiny <- read.table(textConnection(temp),header=TRUE)
rm(temp)
tiny

aggXS <- ddply(tiny, .(ID), summarise,
               Female = Female[1],
               Approve = mean(Approve))

aggT <- ddply(tiny, .(Year), summarise,
              pres=PresVote[1],
              approve=mean(Approve))

########################################
# Cross-country example data, 1945-2014:

TSCSURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/DemonstrationsTSCS.csv"
temp<-getURL(TSCSURL)
Demos<-read.csv(textConnection(temp))
rm(temp)

Demos$X<-NULL
Demos$ColdWar <- with(Demos, 
                      ifelse(Year<1990,1,0))

# Create variables; kill duplicates...

Demos$lnGDP <- log(Demos$GDP)
Demos <- Demos[!duplicated(Demos[c("Year", "ccode")]),] # kill duplicates
PDF <- pdata.frame(Demos,index=c("ccode","Year")) # panel data frame

# summary(PDF)

#################################
# Visualizing panel data...

pdf("PanelDemosViz.pdf",7,5)
panelView(lnDemons~POLITY+Monarch,data=Demos,theme.bw=TRUE,
          outcome.type="continuous",type="outcome",
          by.timing=TRUE,index=c("ccode","Year"),
          main=" ",ylab="log(Demonstrations)",
          legendOff=TRUE,)
dev.off()

pdf("PanelMonarchViz.pdf",7,5)
panelView(lnDemons~Monarch,data=Demos,theme.bw=TRUE,
          by.timing=FALSE,index=c("ccode","Year"),
          color=c("white","darkgreen","red"),
          legend.labs=c("Missing","Not Monarchy","Monarchy"),
          main=" ",ylab="Country Code",axis.lab.gap=c(5,5),
          background="white")
dev.off()


##### Variation, within and between #########
with(Demos, describe(POLITY)) # all variation

pdf("POLITYAll.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Demos$POLITY,na.rm=TRUE),
     main="",xlab="POLITY Score",
     lwd=2)
dev.off()

POLITYmeans <- ddply(Demos,.(ccode),summarise,
                 POLITYmean = mean(POLITY))

with(POLITYmeans, describe(POLITYmean)) # "between" variation

pdf("POLITYbetween.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(POLITYmeans$POLITYmean,na.rm=TRUE),
     main="",xlab="Mean POLITY Scores",
     lwd=2)
dev.off()

Demos <- ddply(Demos, .(ccode), mutate,
                POLITYmean = mean(POLITY))
Demos$POLITYwithin <- with(Demos, POLITY-POLITYmean)

with(Demos, describe(POLITYwithin))

pdf("POLITYwithin.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Demos$POLITYwithin,na.rm=TRUE),
     main="",xlab="POLITY: Within-Country Variation",
     lwd=2)
abline(v=0,lty=2)
dev.off()

#####################################################
# Unit Effects models:

# Pooled OLS:

OLS<-lm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar, 
          data=PDF)

summary(OLS)

# "Fixed" / within effects:

FE<-plm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar, 
           data=PDF, effect="individual",model="within")

summary(FE)

# Make a table:

Table1 <- stargazer(OLS,FE,
                    title="Models of Demonstrations",
                    column.separate=c(1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("POLITY","POLITY Squared",
                                       "ln(GDP)","Monarch","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX1.tex")

# Between effects:

BE<-plm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar, 
          data=PDF, effect="individual",model="between")

summary(BE)

Table2 <- stargazer(OLS,FE,BE,
                    title="Models of Demonstrations",
                    column.separate=c(1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("POLITY","POLITY Squared",
                                       "ln(GDP)","Monarch","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX2.tex")

# REs:

RE<-plm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar,
              data=PDF, effect="individual", model="random")

summary(RE)

AltRE<-lmer(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar+
                 (1|ccode), data=Demos)

summary(AltRE)

Table3 <- stargazer(OLS,FE,BE,RE,
                    title="Models of Demonstrations",
                    column.separate=c(1,1,1,1),align=TRUE,
                    dep.var.labels.include=FALSE,
                    dep.var.caption="",
                    covariate.labels=c("POLITY","POLITY Squared",
                                       "ln(GDP)","Monarch","Cold War"),
                    header=FALSE,model.names=FALSE,
                    model.numbers=FALSE,multicolumn=FALSE,
                    object.names=TRUE,notes.label="",
                    out="UFX3.tex")

# Hausman test:

phtest(FE, RE)

######################################################
# Dynamics...
#
# Panel unit root tests:

lnDemons<-cbind(Demos$ccode,Demos$Year,Demos$lnDemons)
lnDemons<-lnDemons[complete.cases(lnDemons),]
purtest(lnDemons,exo="trend",test=c("levinlin"))
purtest(lnDemons,exo="trend",test=c("hadri"))
purtest(lnDemons,exo="trend",test=c("ips"))

# Some regression models, with dynamics...

# Lagged -dependent-variable model:

PDF$lnDemons.L <- plm::lag(PDF$lnDemons,k=1) # be sure to use the
                                             # -plm- version of -lag-

LDV.fit <- lm(lnDemons~lnDemons.L+POLITY+I(POLITY^2)+
                      lnGDP+Monarch+ColdWar,data=PDF)

FD.fit <- plm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar, 
              data=PDF, effect="individual",model="fd")

FE.fit <- plm(lnDemons~POLITY+I(POLITY^2)+lnGDP+Monarch+ColdWar, 
              data=PDF, effect="individual",model="within")

LDV.FE.fit <- plm(lnDemons~lnDemons.L+POLITY+I(POLITY^2)+lnGDP+
              Monarch+ColdWar,data=PDF, effect="individual",
              model="within")

# Table:

texreg(list(LDV.fit,FD.fit,FE.fit,LDV.FE.fit),
       custom.model.names=c("LDV","First Difference","FE","LDV + FE"),
       custom.coef.names=c("Intercept","Lagged ln(Demonstrations)",
                           "POLITY","POLITY Squared","ln(GDP)",
                           "Monarch","Cold War"),
       digits=3,stars=0.05)

# OPMs:

PDF$POLITYSQ <- PDF$POLITY^2
set.seed(7222009)
OPM.fit <- opm(lnDemons~POLITY+POLITYSQ+lnGDP+Monarch+ColdWar,
               data=PDF,index=c("ccode","Year"),n.samp=1000)

# Ladder plot of estimates & CIs:

pdf("OPM-Ladder.pdf",8,6)
par(mar=c(4,8,2,2))
caterplot(OPM.fit,main=c(""),
             xlab="Parameter Estimate",
             labels=c("Rho","Variance","POLITY",
                      "POLITY Squared","ln(GDP)","Monarchy",
                      "Cold War"))
abline(v=0,lty=2)
dev.off()

# Short- and long-run effects:

SREs<-summary(OPM.fit)$quants[3:7,3]
LREs<-numeric(5)
for(i in 1:5){
        LREs[i]<-quantile(OPM.fit$samples$beta[,i]/(1-OPM.fit$samples$rho),
                          probs=c(0.50))
}

print(cbind(round(SREs,4),round(LREs,4)))
