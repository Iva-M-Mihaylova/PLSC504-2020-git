##########################################
# Code for PLSC 504 - Fall 2020
#
# Ordinal Response and Event Count Models
#
##########################################
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Packages, etc. (install as needed):

require(RCurl)
library(MASS) # for polr (ordered logit / probit)
# install.packages("oddsratio") # uncomment as needed
library(oddsratio)
library(mfx)
library(VGAM)
library(pscl)

####################
# NOTE: Much of the ordered-response code is commented
# out, since (in PLSC 504) we didn't do much in the way of
# real-data or simulated examples.
#
# Ordered-response simulation:
# 
# set.seed(7222009)
# X<-runif(1000,0,10)
# Ystar<-0 + 1*X + rnorm(1000)
# Y1<-Ystar
# Y1[Ystar<2.5]<-1
# Y1[Ystar>=2.5 & Ystar<5]<-2
# Y1[Ystar>=5 & Ystar<7.5]<-3
# Y1[Ystar>=7.5]<-4
# table(Y1)
# 
# summary(lm(Ystar~X))
# summary(lm(Y1~X))
# 
# pdf("OrdinalOneR.pdf",7,5)
# par(mar=c(4,4,2,2))
# par(mfrow=c(1,2))
# plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
# abline(lm(Ystar~X),lwd=3,col="red")
# abline(h=c(2.5,5,7.5),lty=2)
# plot(X,Y1,pch=20,xlab="X",ylab="Y1")
# abline(lm(Y1~X),lwd=3,col="red")
# dev.off()
# 
# Y2<-Ystar
# Y2[Ystar<2]<-1
# Y2[Ystar>=2 & Ystar<8]<-2
# Y2[Ystar>=8 & Ystar<9]<-3
# Y2[Ystar>9]<-4
# table(Y2)
# 
# summary(lm(Y2~X))
# 
# pdf("OrdinalTwoR.pdf",7,5)
# par(mar=c(4,4,2,2))
# par(mfrow=c(1,2))
# plot(X,Ystar,pch=20,xlab="X",ylab="Y*")
# abline(lm(Ystar~X),lwd=3,col="red")
# abline(h=c(2,8,9),lty=2)
# plot(X,Y2,pch=20,xlab="X",ylab="Y2")
# abline(lm(Y2~X),lwd=3,col="red")
# dev.off()
# 
# # Best Example Ever...
# 
# temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2019-git/master/Data/Beer.csv")
# beer<-read.csv(text=temp, header=TRUE)
# rm(temp)
# 
# summary(beer)
# 
# beer.logit<-polr(as.factor(quality)~price+calories+craftbeer
#                  +bitter+malty,data=beer)
# summary(beer.logit)
# 
# beer.probit<-polr(as.factor(quality)~price+calories+craftbeer+
#                     bitter+malty,data=beer,method="probit")
# summary(beer.probit)
# 
# # Profile-likelihood-based CIs:
# 
# CIs.logit <- confint(beer.logit)
# 
# # Compare to normal CIs:
# 
# CIs.alt <- cbind(beer.logit$coefficients-1.96*sqrt(diag(vcov(beer.logit)))[1:5],
#                  beer.logit$coefficients+1.96*sqrt(diag(vcov(beer.logit)))[1:5])
# 
# CIs.logit
# CIs.alt
# 
# # Odds Ratios
# 
# olreg.or <- function(model) { 
#    coeffs <- coef(summary(beer.logit)) 
#    lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]) 
#    or <- exp(coeffs[ ,1]) 
#    uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]) 
#    lreg.or <- cbind(lci, or, uci) 
#    lreg.or 
#    } 
# 
# olreg.or(beer.logit)
# 
# # Predicted probs
# 
# calories<-seq(60,200,1)
# price<-mean(beer$price)
# craftbeer<-median(beer$craftbeer)
# bitter<-mean(beer$bitter)
# malty<-mean(beer$malty)
# beersim<-cbind(calories,price,craftbeer,bitter,malty)
# beer.hat<-predict(beer.logit,beersim,type='probs')
# 
# pdf("ROrdinalProbs.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(c(60,200), c(0,1), type='n', xlab="Calories", ylab='Fitted 
#   Probability')
# lines(60:200, beer.hat[1:141, 1], lty=1, lwd=3)
# lines(60:200, beer.hat[1:141, 2], lty=2, lwd=3)
# lines(60:200, beer.hat[1:141, 3], lty=3, lwd=3)
# lines(60:200, beer.hat[1:141, 4], lty=4, lwd=3)
# dev.off()
# 
# # Cumulative probs:
# 
# xaxis<-c(60,60:200,200)
# yaxis1<-c(0,beer.hat[,1],0)
# yaxis2<-c(0,beer.hat[,2]+beer.hat[,1],0)
# yaxis3<-c(0,beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)
# yaxis4<-c(0,beer.hat[,4]+beer.hat[,3]+beer.hat[,2]+beer.hat[,1],0)
# 
# pdf("ROrdinalCumProbs.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(c(60,200), c(0,1), type='n', xlab="Calories", 
#  ylab="Cumulative Probability")
# polygon(xaxis,yaxis4,col="white")
# polygon(xaxis,yaxis3,col="grey80")
# polygon(xaxis,yaxis2,col="grey50")
# polygon(xaxis,yaxis1,col="grey10")
# dev.off()
# 
# # Variants: Series of Binary Regressions:
# 
# beer$goodplus<-as.factor(quality>1)
# beer.good<-glm(goodplus~price+calories+craftbeer+bitter+
#                  malty,family="binomial",data=beer)
# summary(beer.good)
# 
# beer$VGplus<-as.factor(quality>2)
# beer.VG<-glm(VGplus~price+calories+craftbeer+bitter+malty,
#              family="binomial",data=beer)
# summary(beer.VG)

################################
# Event Count Data
################################

###############################
# Hurdles, Zero-Inflation, etc.
#
# Conflict data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/wars.csv")
wars<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(wars)

# Poisson:

wars.poisson<-glm(conflicts~polity+politysq+logPopulation+logGDP+
                     GDPGrowth+logOpenness+govshareGDP,family="poisson",
                  data=wars)
summary.glm(wars.poisson)

# Negative Binomial:

wars.nb<-glm.nb(conflicts~polity+politysq+logPopulation+logGDP+GDPGrowth+
                   logOpenness+govshareGDP,data=wars)
summary(wars.nb)

# Note: Coefficients are essentially identical:

BPois <- wars.poisson$coefficients[2:8]
BNB <- wars.nb$coefficients[2:8]

pdf("PoissonVsNB.pdf",7,6)
par(mar=c(4,4,2,2))
plot(BPois,BNB,pch=19,xlim=c(-1.1,1.5),
     xlab="Poisson Estimates",
     ylab="Negative Binomial Estimates")
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(a=0,b=1,lwd=2)
text(BPois,BNB,labels = names(BNB),pos=4)
text(-0.6,0.7,"Solid line is 45-degree line")
dev.off()

# Truncation E(Y) / Var(Y) plot:

lambda<-seq(0.1,6,by=0.1)
EY <- (lambda)/(1-exp(-lambda))
VY <- ((lambda)/(1-exp(-lambda))) * (1-((lambda)/(exp(lambda)-1)))

pdf("TruncatedPoissonEYVarYR.pdf",6,5)
plot(lambda,EY,t="l",lwd=2,lty=2,col="red",ylim=c(0,6),
     xlab="Lambda",ylab="E(Y|Y>0) and Var(Y|Y>0)")
lines(lambda,VY,lwd=2,lty=3,col="darkgreen")
abline(a=0,b=1,lwd=1)
legend("bottomright",bty="n",lwd=c(2,2,1),lty=c(2,3,1),
       legend=c("E(Y)","Var(Y)","Mean-Variance Equality"),
       col=c("red","darkgreen","black"))
dev.off()

# Truncation barplot:

outcomes<-seq(0,10,by=1)
outsPos<-seq(1,10,by=1)
upperT <- 4  # value at which dist. is upper-truncated...
outsUpper<-seq(0,upperT,by=1)
L <- 2.0
PoissonPr<- dpois(outcomes,L)
ZTPr <- (exp(-L)*(L^outsPos)) / ((factorial(outsPos)) * (1-exp(-L)))
ZTPr<-append(0,ZTPr)
T4Pr <- numeric(upperT+1)
PoisProb <- (exp(-L)*(L^outsUpper))/(factorial(outsUpper))
for (i in 1:(upperT+1)) {
   T4Pr[i] <- (exp(-L)*(L^outsUpper[i])) / 
      ((factorial(outsUpper[i])) * sum(PoisProb))
}
T4Pr<-append(T4Pr, rep(0,times=(10-upperT)))

df<-data.frame(Count = seq(0,10,by=1),
               Poisson = PoissonPr,
               ZeroTruncated = ZTPr,
               TruncAtFour = T4Pr)

pdf("PoissonTruncatedDensitiesR.pdf",7,5)
with(df, plot(Count,Poisson,pch=20,cex=1.2,col="black",lab=c(11,5,7),
              ylim=c(0,0.35),xlim=c(-0.3,10.3),ylab="Probability"))
with(df, points(Count-0.2,ZTPr,pch=4,col="red"))
with(df, points(Count+0.2,T4Pr,pch=17,col="darkgreen"))
with(df, segments(Count,0,Count,Poisson,lwd=1))
with(df, segments(Count-0.2,0,Count-0.2,ZTPr,lwd=1,col="red"))
with(df, segments(Count+0.2,0,Count+0.2,T4Pr,lwd=1,col="darkgreen"))
legend("topright",bty="n",col=c("black","red","darkgreen"),
       pch=c(20,4,17),lwd=1,lty=1,legend=c("Poisson","Truncated At Zero",
                                           "Upper Truncated at Four"))
dev.off()

# Truncation example. Standard Poisson (no zeros):

wars.poisNo0s<-glm(conflicts_no_zeros~polity+politysq+logPopulation+
                      logGDP+GDPGrowth+logOpenness+govshareGDP,
                   family="poisson",data=wars)

summary(wars.poisNo0s)

# Truncated at Zero:

wars.0tpois<-vglm(conflicts_no_zeros~polity+politysq+logPopulation+
                     logGDP+GDPGrowth+logOpenness+govshareGDP,
                  pospoisson,data=wars)

summary(wars.0tpois)

# Upper-censored Poisson:

wars$censoredconflicts<-wars$conflicts
wars$censoredconflicts<-ifelse(wars$conflicts>3,4,wars$censoredconflicts)
wars$censindicator<-ifelse(wars$censoredconflicts==4,1,0)

# Incorrect:

wars.poisCensored<-glm(censoredconflicts~polity+politysq+
                          logPopulation+logGDP+GDPGrowth+logOpenness+
                          govshareGDP,family="poisson",data=wars)

summary(wars.poisCensored)

# Censored:

wars.censpois<-vglm(SurvS4(censoredconflicts,censindicator)~polity+
                       politysq+logPopulation+logGDP+GDPGrowth+
                       logOpenness+govshareGDP,
                    cens.poisson,data=wars)
summary(wars.censpois)

# Zero-Inflated & hurdle models:

wars.ZIP<-zeroinfl(conflicts~polity+politysq+logPopulation+
                      logGDP+GDPGrowth+logOpenness+govshareGDP,
                   data=wars,dist="poisson",link="logit")
summary(wars.ZIP)

wars.ZINB<-zeroinfl(conflicts~polity+politysq+logPopulation+
                       logGDP+GDPGrowth+logOpenness+govshareGDP,
                    data=wars,dist="negbin",link="logit")
summary(wars.ZINB)

wars.hurdle<-hurdle(conflicts~polity+politysq+logPopulation+
                       logGDP+GDPGrowth+logOpenness+govshareGDP,
                    data=wars,dist=c("poisson"),zero.dist=c("poisson"),
                    link=c("log"))
summary(wars.hurdle)

# Some comparisons:

BZIP <- wars.ZIP$coefficients$count[2:8]
BZIP0 <- wars.ZIP$coefficients$zero[2:8]
BZINB <- wars.ZINB$coefficients$count[2:8]
BZINB0 <- wars.ZINB$coefficients$zero[2:8]

# ZIP vs standard Poisson:

pdf("Poisson-ZIP.pdf",7,6)
par(mar=c(4,4,2,2))
plot(BPois,BZIP,pch=19,xlim=c(-1.1,1.5),
     xlab="Poisson Estimates",
     ylab="Zero-Inflated Poisson Estimates")
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(a=0,b=1,lwd=2)
text(BPois,BZIP,labels = names(BZIP),pos=4)
text(-0.6,0.7,"Solid line is 45-degree line")
dev.off()


# ZIP vs ZINB:

pdf("ZIP-ZINB.pdf",9,6)
par(mfrow=c(1,2))
par(mar=c(4,4,4,2))
plot(BZIP,BZINB,pch=19,xlim=c(-1,1.5),
     xlab="ZIP Estimates: Counts",
     ylab="ZINB Estimates: Counts",
     main="Poisson Coefficients")
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(a=0,b=1,lwd=2)
text(BZIP,BZINB,labels = names(BZIP),pos=4)
text(-0.26,0.58,"Solid line is 45-degree line")
#
par(mar=c(4,4,4,2))
plot(BZIP0,BZINB0,pch=19,xlim=c(-4,7),
     xlab="ZIP Estimates: Logits",
     ylab="ZINB Estimates: Logits",
     main="Logit Coefficients")
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(a=0,b=1,lwd=2)
text(BZIP0,BZINB0,labels = names(BZIP0),pos=4)
text(-0.5,4,"Solid line is 45-degree line")
dev.off()

# fin


