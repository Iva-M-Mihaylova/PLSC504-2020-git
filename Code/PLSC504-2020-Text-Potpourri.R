#########################################################
# PLSC 504 -- Fall 2020
#
# A veritable cornucopia of methods for analyzing
# text.
#
# (Not really, but it's 2020, so truth in advertising
# isn't really a thing anymore.)
#
#########################################################
# Load packages (install as needed), set options:

library(RCurl)
library(plyr)
library(lubridate)
library(stringr)
library(car)
# install.packages("ctv")
# library(ctv)  <-- as necessary
# install.views("NaturalLanguageProcessing")
library(tm)
library(arm)
library(stopwords)
library(SnowballC)
# install.packages(wordcloud)
library(wordcloud)
# install.packages("stm")
library(stm) 
library(qdap)
library(SentimentAnalysis)
library(readtext)
library(rvest)
library(quanteda)
library(devtools)
#devtools::install_github("conjugateprior/austin")
#library(austin) # <- not used here, but useful

setwd("~/Dropbox (Personal)/PLSC 504")  # <-- change as necessary...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#################################################
######### DICTIONARY-BASED METHODS ##############
#################################################

#################################################
# General Inquirer example:

data(DictionaryGI)

# AFINN example:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/AFINN-111.txt")
AFINN <- read.delim(text=temp,header=FALSE,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

AFINN<-AFINN[order(AFINN$V2),]
View(AFINN)

########################################
# UNHCR speech data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/UNHCRSpeeches.csv")
UN <- read.csv(text=temp,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

UN$content <- removeNumbers(UN$content) # no numbers
UN$content <- str_replace_all(UN$content, "[\n]", " ") # line breaks
UN$content <- removeWords(UN$content,stopwords("en")) # remove stopwords
UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Corpus:

UN2 <- with(UN, data.frame(doc_id = id,
                           text = content))
ds <- DataframeSource(UN2) 
UNC <- Corpus(ds)
meta(UNC)

# Some tools in SentimentAnalysis...

UNCount<-countWords(UNC,removeStopwords=FALSE)
summary(UNCount$WordCount)

pdf("Notes and Slides/UNHCR-Hist.pdf",6,5)
par(mar=c(4,4,2,2))
hist(UNCount$WordCount,main=" ",xlab="Word Count",
     col="grey32")
dev.off()

######################################
# Simple sentiment analysis:

UNSent <- analyzeSentiment(UNC)
summary(UNSent)


# Plots, etc.:

rSC<-with(UNSent, cor(log(WordCount),SentimentGI))

pdf("Notes and Slides/UNHCRSentVsCount.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~WordCount,data=UNSent,log="x",
            pch=20,grid=FALSE,xlab="ln(Word Count)",
            ylab="Sentiment",spread=FALSE)
abline(h=0,lty=2,lwd=1.5)
text(100,0.25,paste0("r = ",round(rSC,2)))
dev.off()

pdf("Notes and Slides/UNHCRSentOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNSent$SentimentGI,t="l",lwd=1.5,
     xlab="Date",ylab="Speech Sentiment")
lines(lowess(UN$Date,UNSent$SentimentGI),lwd=2,col="red")
abline(h=0,lty=2)
dev.off()

# By year...

AnnMeans<-aggregate(UNSent$SentimentGI,list(UN$Year),mean)

pdf("Notes and Slides/UNHCRAnnMeans.pdf",6,5)
par(mar=c(4,4,2,2))
plot(AnnMeans$Group.1,AnnMeans$x,t="l",lwd=1.5,
     xlab="Year",ylab="Average Sentiment",ylim=c(0.04,0.17))
lines(lowess(AnnMeans$Group.1,AnnMeans$x),lwd=2,col="red")
dev.off()

# By author:

UN$Author<-ordered(UN$Author,levels=c("Goedhart","Lindt",
                             "Schnyder","Khan","Hartling",
                             "Hocké","Stoltenberg","Ogata",
                             "Lubbers","Guterres"))

pdf("Notes and Slides/UNHCR-by-Author.pdf",6,5)
par(mar=c(6,4,2,2))
boxplot(UNSent$SentimentGI~UN$Author,las=2)
abline(h=0,lty=2)
dev.off()


# Similar results by dictionary?

GI<-loadDictionaryGI()
QD<-loadDictionaryQDAP()

compareDictionaries(GI,QD)

r.GI.QDAP <- with(UNSent, cor(SentimentGI,SentimentQDAP))

pdf("Notes and Slides/UNHCR-Dict-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~SentimentQDAP,data=UNSent,
            xlab="QDAP",ylab="General Inquirer",pch=20,
            grid=FALSE)
text(0,0.20,paste0("r = ",round(r.GI.QDAP,2)))
dev.off()

# For which speakers does the dictionary matter?

DictDiff <- with(UNSent, abs(SentimentGI - SentimentQDAP))

summary(lm(DictDiff~UN$Author - 1))

# Custom dictionary by-hand:

YugoWords <- c("yugoslavia","serbia","bosnia","herzegovina",
               "kosovo","montenegro","macedonia","croatia",
               "vojvodina","balkans")

FmrYugo <- SentimentDictionaryWordlist(YugoWords)

UNHCRYugo <- analyzeSentiment(UNC,
                  rules=list("YugoTopic"=list(
                    ruleRatio,FmrYugo)))

summary(UNHCRYugo$YugoTopic)

pdf("Notes and Slides/UNHCRYugoOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNHCRYugo$YugoTopic,t="l",lwd=1.5,
     xlab="Date",ylab="Fmr. Yugoslavia Content")
dev.off()

#################################################
############    TOPIC MODELS    #################
#################################################

# UNHCR speech data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2020-git/master/Data/UNHCRSpeeches.csv")
UN <- read.csv(text=temp,
               stringsAsFactors=FALSE,allowEscapes=TRUE)
rm(temp)

# Clean things up a bit:

UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Process text (this time using textProcessor from stm):
#
# Note that defaults convert cases, remove stopwords / 
# punctuation / words < 3 characters / extra white space, 
# and stems.

UNHCR <- textProcessor(UN$content, metadata=UN) 

# Create stm corpus. Note that this defaults to dropping
# words that only appear in one document:

UNCorp <- prepDocuments(UNHCR$documents,UNHCR$vocab,UNHCR$meta)

# Let's see what happens if we raise that lower threshold:

pdf("Notes/TopicDocRemoval.pdf",9,6)
plotRemoved(UNHCR$documents, lower.thresh = seq(1, 100, by = 5))
dev.off()

###################
# Basic LDA using topicmodels...
#
# Convert format:

UNLDACorp <- convertCorpus(UNCorp$documents,UNCorp$vocab,
                           type="slam")

# Basic LDA, with six topics:

UN.LDAV.6 <- LDA(UNLDACorp,6,method="VEM",
                 seed=7222009) 

# Check out the terms / topics:

terms(UN.LDAV.6,10)

# ... and the topics:
#
# topics(UN.LDAV.6,10)

# Generate posterior probabilities of the topics 
# for each document and the terms for each topic:

V.6.Post <- posterior(UN.LDAV.6)
cor(V.6.Post$topics)

# Plot those:

pdf("Notes/LDA-Posteriors.pdf",9,7)
scatterplotMatrix(V.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                                    colnames(V.6.Post$topics)))
dev.off()

# Examine topic probabilities by author:

pdf("Notes/LDA-By-Author.pdf",8,6)
par(mar=c(4,4,2,2))
par(mfrow=c(2,3))
boxplot(V.6.Post$topic[,1]~UN$Author,las=2,main="Topic One")
boxplot(V.6.Post$topic[,2]~UN$Author,las=2,main="Topic Two")
boxplot(V.6.Post$topic[,3]~UN$Author,las=2,main="Topic Three")
boxplot(V.6.Post$topic[,4]~UN$Author,las=2,main="Topic Four")
boxplot(V.6.Post$topic[,5]~UN$Author,las=2,main="Topic Five")
boxplot(V.6.Post$topic[,6]~UN$Author,las=2,main="Topic Six")
dev.off()

# Same model, using Gibbs sampling:
# 
# UN.LDAG.6 <- LDA(UNLDACorp,6,method="Gibbs",
#                  seed=7222009) 
# 
# # Examine topics:
# 
# terms(UN.LDAG.6,10)
#
# Loop over different numbers of topics, and
# check the perplexity for each:

MaxTopics <- 40
Seq <- seq(2,MaxTopics,by=2)
Perps <- numeric(MaxTopics/2)
for (i in Seq) {
  foo <- LDA(UNLDACorp,i,method="VEM",
             seed=7222009)
  Perps[i/2] <- perplexity(foo)
}

# Plot:

pdf("Notes/PerplexityByK.pdf",7,5)
par(mar=c(4,4,2,2))
plot(Seq,Perps,t="l",lwd=2,ylab="Perplexity",
     xlab="Number of Topics",xlim=c(0,41))
dev.off()

##################################
# Correlated topic models
#
# Basic CTM:

UN.CTMV.6 <- CTM(UNLDACorp,6,method="VEM",
                 seed=7222009) 

# Check out topics:

terms(UN.CTMV.6,10)

# Posteriors:

CTMV.6.Post <- posterior(UN.CTMV.6)
cor(CTMV.6.Post$topics)

pdf("Notes/CTM-Posteriors.pdf",9,7)
scatterplotMatrix(CTMV.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                                    colnames(CTMV.6.Post$topics)))
dev.off()

##################################
# Structural topic model...
#
# One run:

STM.6 <- stm(UNCorp$documents,UNCorp$vocab,6,
             prevalence=~Year+Author,
             data=UNCorp$meta)

# Things you can do with a single STM...
#
# Check out the topics...

labelTopics(STM.6)


# Plots!
#
# Summary:

pdf("Notes/STM-Summary-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6, type="summary",labeltype="frex",
     main="Top Topics (FREX words)")
dev.off()

# MAP histograms:

pdf("Notes/STM-Hist-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6,type="hist",labeltype="frex")
dev.off()

# Labels:

pdf("Notes/STM-Labels-Plot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(STM.6,type="perspectives",topics=c(5,1),
     labeltype="frex",plabels=c("Yugoslavia","Africa"))
dev.off()

# OMG WORD CLOUDS:

pdf("Notes/STM-Cloud-Plot.pdf",8,7)
par(mar=c(4,4,2,2))
cloud(STM.6,topic=5)
dev.off()

# Examine covariate effects:

UN$Ogata <- ifelse(UN$Author=="Ogata",1,0)
STM.Ogata<- estimateEffect(1:6~Ogata,STM.6,metadata=UN)
summary(STM.Ogata)

pdf("Notes/STM-Ogata-Effects.pdf",6,5)
par(mar=c(4,10,2,2))
plot(STM.Ogata,"Ogata",method="difference",
     cov.value1=1,cov.value2=0,xlab="Difference in Probability")
dev.off()

# Alternative model: Topical *content*:

STM.6C <- stm(UNCorp$documents,UNCorp$vocab,6,
              content=~Author,
              data=UNCorp$meta)
STMC.Ogata<- estimateEffect(1:6~Ogata,STM.6C,metadata=UN)

pdf("Notes/STM-Ogata-Effects2.pdf",6,5)
par(mar=c(4,10,2,2))
plot(STMC.Ogata,"Ogata",method="difference",
     cov.value1=1,cov.value2=0,xlab="Difference in Probability")
dev.off()

plot(STMC.Ogata, type="perspective",topic=5)


# Topic correlations:

STM.TopicR <- topicCorr(STM.6, cutoff=0.5)

pdf("Notes/STM-Topic-Corrs.pdf",6,6)
plot(STM.TopicR)
dev.off()


# Model selection:

STM.6.select <- selectModel(UNCorp$documents,UNCorp$vocab,6,
                            prevalence=~Year+Author,data=UNCorp$meta,
                            max.em.its=150,runs=20,seed=7222009)

multiSTM(STM.6.select)

# Selection of the number of topics:

STM.searchK <- searchK(UNCorp$documents,UNCorp$vocab,
                       c(2,5,seq(10,50,by=10),75,100))

pdf("Notes/STM-Picking-K.pdf",7,5)
par(mar=c(4,4,4,2))
plot(STM.searchK,pch=20)
dev.off()

#################################################
############    TEXT SCALING    #################
#################################################

####################################################
# 2016 Presidential debates texts (read from local 
# files because I'm lazy af, change it if you want,
# whatever yadda yadda):

f1<-"Data/Debates/Debate2016-1.txt"
d1<-readChar(f1,file.info(f1)$size)
f2<-"Data/Debates/Debate2016-2.txt"
d2<-readChar(f2,file.info(f2)$size)
f3<-"Data/Debates/Debate2016-3.txt"
d3<-readChar(f3,file.info(f3)$size)
rm(f1,f2,f3)

# Clean things up / annotate:

m1 <- str_locate_all(d1, pattern = "CLINTON|TRUMP|HOLT")
m1 <- m1[[1]]
m1 <- m1[,1]

res1 <- vector(mode = "character", length = length(m1) - 1)
for (i in 1:(length(m1)-1)) {
  res1[i] <- substr(d1,m1[i],m1[i+1]-1)
}
clinton1 <- res1[sapply(res1,function(x) grepl("CLINTON",x))]
trump1 <- res1[sapply(res1,function(x) grepl("TRUMP",x))]
holt <- res1[sapply(res1,function(x) grepl("HOLT",x))]

# Add labels:

Clinton <- rep("Clinton: First Debate",times=length(clinton1))
Trump <- rep("Trump: First Debate",times=length(trump1))
Holt <- rep("Holt",times=length(holt))

# Corpuses, using quanteda:

CC1 <- corpus(clinton1,docvars=data.frame(Speaker=Clinton))
CT1 <- corpus(trump1,docvars=data.frame(Speaker=Trump))
CH <- corpus(holt,docvars=data.frame(Speaker=Holt))

D1C<-CC1+CT1+CH

# Same for second and third debates:

m2 <- str_locate_all(d2, pattern = "CLINTON|TRUMP|RADDATZ|COOPER")
m2 <- m2[[1]]
m2 <- m2[,1]

res2 <- vector(mode = "character", length = length(m2) - 1)
for (i in 1:(length(m2)-1)) {
  res2[i] <- substr(d2,m2[i],m2[i+1]-1)
}
clinton2 <- res2[sapply(res2,function(x) grepl("CLINTON",x))]
trump2 <- res2[sapply(res2,function(x) grepl("TRUMP",x))]
raddatz <- res2[sapply(res2,function(x) grepl("RADDATZ",x))]
cooper <- res2[sapply(res2,function(x) grepl("COOPER",x))]

# Add labels:

Clinton <- rep("Clinton: Second Debate",times=length(clinton2))
Trump <- rep("Trump: Second Debate",times=length(trump2))
Raddatz <- rep("Raddatz",times=length(raddatz))
Cooper <- rep("Cooper",times=length(cooper))

# Corpuses:

CC2 <- corpus(clinton2,docvars=data.frame(Speaker=Clinton))
CT2 <- corpus(trump2,docvars=data.frame(Speaker=Trump))
CR <- corpus(raddatz,docvars=data.frame(Speaker=Raddatz))
CA <- corpus(cooper,docvars=data.frame(Speaker=Cooper))

D2C<-CC2+CT2+CR+CA

# Third debate:

m3 <- str_locate_all(d3, pattern = "CLINTON|TRUMP|WALLACE")
m3 <- m3[[1]]
m3 <- m3[,1]

res3 <- vector(mode = "character", length = length(m3) - 1)
for (i in 1:(length(m3)-1)) {
  res3[i] <- substr(d3,m3[i],m3[i+1]-1)
}
clinton3 <- res3[sapply(res3,function(x) grepl("CLINTON",x))]
trump3 <- res3[sapply(res3,function(x) grepl("TRUMP",x))]
wallace <- res3[sapply(res3,function(x) grepl("WALLACE",x))]

# Add labels:

Clinton <- rep("Clinton: Third Debate",times=length(clinton3))
Trump <- rep("Trump: Third Debate",times=length(trump3))
Wallace <- rep("Wallace",times=length(wallace))

# Corpuses, using quanteda:

CC3 <- corpus(clinton3,docvars=data.frame(Speaker=Clinton))
CT3 <- corpus(trump3,docvars=data.frame(Speaker=Trump))
CW <- corpus(wallace,docvars=data.frame(Speaker=Wallace))

D3C<-CC3+CT3+CW

# All three:

D123C <- D1C+D2C+D3C
summary(D123C, 12)

#####################
# Summary plot:

Info <- summary(D123C,n=nrow(D123C$documents))

pdf("Notes/DebateResponseLength.pdf",6,5)
par(mar=c(12,4,2,2))
with(Info, boxplot(Tokens~Speaker,log="y",las=2,
                   ylab="Response Length"))
dev.off()


######################
# Create DFM:

DDFM <- dfm(D123C,remove=stopwords("english"),stem=TRUE,
            remove_punct=TRUE,groups="Speaker")
topfeatures(DDFM,28)

# Word clouds!

pdf("Notes/DebateDTWordCloud.pdf",5,5)
par(mar=c(1,1,1,1))
textplot_wordcloud(DDFM[c(2,5,9),],min_count=20,random_order=FALSE,
                   color=RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

pdf("Notes/DebateHCWordCloud.pdf",5,5)
par(mar=c(1,1,1,1))
textplot_wordcloud(DDFM[c(1,4,8),],min_count=20,random_order=FALSE,
                   color=RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

# Word frequency plot:

DFreqs <- textstat_frequency(DDFM,n=50)

pdf("Notes/DebatesTopWords.pdf",7,5)
par(mar=c(4,24,2,2))
with(DFreqs, dotchart2(frequency,labels=feature,pch=17,cex=0.5,
                       horizontal=TRUE,xlab="Frequency",
                       width.factor=2,dotsize=2))
dev.off()


# Word Keyness, first debate:

D1C2<-corpus_subset(D1C,Speaker %in% c("Clinton: First Debate",
                                       "Trump: First Debate"))
D1C2DFM <- dfm(D1C2,remove=stopwords("english"),stem=TRUE,
               remove_punct=TRUE,groups="Speaker")

D1Key <- textstat_keyness(D1C2DFM, target = "Clinton: First Debate")

head(D1Key,12)

pdf("Notes/D1WordKeynessPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_keyness(D1Key,margin=0.15,color=c("darkblue","red"),
                 labelsize=3)
dev.off()



#####################################
# Scaling: Wordfish (unsupervised):

WF <- textmodel_wordfish(DDFM,dir=c(1,2))
summary(WF)

WF.hats<-predict(WF,se.fit=TRUE)
WF.Bs <- coef(WF,margin="both")

# Rope and ladder plot of betas:

pdf("Notes/WordfishLadderPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF)
dev.off()

# Plot of word/feature scores:

pdf("Notes/WordfishFeaturesPlot.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF,margin="features",alpha=0.4,
                 highlighted=names(topfeatures(DDFM,20)),
                 highlighted_color="red")
dev.off()


# Wordfish w/o moderators:

CTonly <- corpus_subset(D123C,Speaker=="Clinton: First Debate" |
                          Speaker=="Clinton: Second Debate" | 
                          Speaker=="Clinton: Third Debate" |
                          Speaker=="Trump: First Debate" |
                          Speaker=="Trump: Second Debate" |
                          Speaker=="Trump: Third Debate",
                        select=Speaker)
CTDFM <- dfm(CTonly,remove=stopwords("english"),stem=TRUE,
             remove_punct=TRUE,groups="Speaker")

WF2 <- textmodel_wordfish(CTDFM,dir=c(1,2))
summary(WF2)

WF2.hats<-predict(WF2,se.fit=TRUE)

# Rope and ladder plot:

pdf("Notes/WordfishLadderPlot2.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2)
dev.off()

# Plot of word/feature scores:

pdf("Notes/WordfishFeaturesPlot2.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2,margin="features",alpha=0.4,
                 highlighted=names(topfeatures(CTDFM,20)),
                 highlighted_color="red")
dev.off()

# Another one, this time with large values of psi
# highlighted:

tfoo <- abs(WF2$psi)>3.5

pdf("Notes/WordfishFeaturesPlot3.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WF2,margin="features",alpha=0.4,
                 highlighted=WF2$features[tfoo==TRUE],
                 highlighted_color="red")
dev.off()

###################################
# Scaling: Wordscores
#
# Work with the "candidates only" corpuses. 
# Set the statements made by Clinton in the first
# debate equal to -1, and those made by Trump
# to -1:

TScores <- c(-1,1,NA)

WS.train <- textmodel_wordscores(D1C2DFM,TScores,
                                 scale="linear")
summary(WS.train)

# Plot wordscores:

pdf("Notes/Wordscore-scores.pdf",6,5)
par(mar=c(4,4,2,2))
textplot_scale1d(WS.train,alpha=0.7,
                 highlighted=c("email"))
dev.off()

# Predict to the second and third debates:

D23Corpus <- corpus_subset(CTonly,Speaker=="Clinton: Second Debate" | 
                             Speaker=="Clinton: Third Debate" |
                             Speaker=="Trump: Second Debate" |
                             Speaker=="Trump: Third Debate",
                           select=Speaker)
D23DFM <- dfm(D23Corpus,remove=stopwords("english"),stem=TRUE,
              remove_punct=TRUE,groups="Speaker")

WS.test <- predict(WS.train,D23DFM,se.fit=TRUE,interval="confidence")
WS.test

# Plot them:

pdf("Notes/WordScoreLadder.pdf",6,5)
with(WS.test, coefplot(fit[c(1,2,4,5,8,9),1],se.fit[c(1,2,4,5,8,9)],
                       varnames=names(fit[c(1,2,4,5,8,9),1]),
                       main="",mar=c(2,10,2,2)))
dev.off()


# Rescaled wordscores:

WS.test2 <- predict(WS.train,D23DFM,se.fit=TRUE,interval="confidence",
                    rescaling="lbg")
WS.test2
