##### Network Analysis (504) #####
library(randomNames)
library(network)
library(sna)
library(ggnetwork)



### Really Basic Network ###

N<-5
set.seed(7222009)
N5 <- rgraph(N,mode="graph",tprob=0.4)
N5 <- network(N5,directed=FALSE)
network.vertex.names(N5) <- randomNames(N,which.names="first")

# plot:

pdf("N5Nondirected.pdf",7,4)
par(mar=c(2,2,2,2))
ggnet2(N5,size=12,label=TRUE,color="grey",
       label.color="black",label.size=3.5,
       edge.color="black",edge.size=0.5)
dev.off()

# Adjacency matrix:

N5[1:5,1:5]

### Star and Circle Networks "by hand" ###

mat<-matrix(c(0, 1, 1, 1, 1, 1, 1, 1,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0, 0, 0) 
            ,nr=8, ncol=8, byrow=T)

net<-network(mat, directed=F, loops=F)
plot(net,displaylabels=F,label.cex=8,vertex.col="blue",
     vertex.border=F)

mat<-matrix(c(0, 1, 0, 0, 0, 0, 0, 1,
              1, 0, 1, 0, 0, 0, 0, 0,
              0, 1, 0, 1, 0, 0, 0, 0,
              0, 0, 1, 0, 1, 0, 0, 0,
              0, 0, 0, 1, 0, 1, 0, 0,
              0, 0, 0, 0, 1, 0, 1, 0,
              0, 0, 0, 0, 0, 1, 0, 1,
              1, 0, 0, 0, 0, 0, 1, 0) 
            ,nr=8, ncol=8, byrow=T)

net<-network(mat, directed=F, loops=F)
plot(net,displaylabels=F,label.cex=8,vertex.col="blue",
     vertex.border=F)


### Star Wars EP2: borrowed from https://rpubs.com/ogimenez/200849 ###

setwd("/starwars_network-master")
# load convenient packages
library(dplyr)
# get rid of the empty lines
lines2 <- filter(lines, raw != "")

# detect scenes: begin by EXT. or INT.
lines3 <-  mutate(lines2, is_scene = str_detect(raw, "T."),scene = cumsum(is_scene)) 

# drop lines that start with EXT. or INT.
lines4 <- filter(lines3,!is_scene)

# distinguish characters from what they say
lines5 <- separate(lines4, raw, c("speaker", "dialogue"), sep = ":", fill = "left",extra='drop')

library(stringr)
library(tidyr)

# read file line by line 
raw <- readLines("attack-of-the-clones.txt")

# create data frame
lines <- data_frame(raw = raw) 

# get rid of leading and trailing white spaces
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
lines= mutate(lines,raw=trim(raw))

# read in aliases (from Evelina's post)
aliases <- read.table('aliases.csv',sep=',',header=T,colClasses = "character")
aliases$Alias
aliases$Name

# assign unique name to characters
multipleReplace <- function(x, what, by) {
  stopifnot(length(what)==length(by))               
  ind <- match(x, what)
  ifelse(is.na(ind),x,by[ind])
}
lines6 <- mutate(lines5,speaker=multipleReplace(speaker,what=aliases$Alias,by=aliases$Name))

# read in actual names (from Evelina's post)
actual.names <- read.csv('characters.csv',header=F,colClasses = "character")
actual.names <- c(as.matrix(actual.names))
# filter out non-characters
lines7 <- filter(lines6,speaker %in% actual.names)

# group by scene
lines8 <- group_by(lines7, scene, line = cumsum(!is.na(speaker))) 

lines9 <- summarize(lines8, speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

# Count the lines-per-scene-per-character
# Turn the result into a binary speaker-by-scene matrix
by_speaker_scene <- count(lines9, scene, speaker)
by_speaker_scene

library(reshape2)
speaker_scene_matrix <-acast(by_speaker_scene , speaker ~ scene, fun.aggregate = length)
dim(speaker_scene_matrix)

cooccur <- speaker_scene_matrix %*% t(speaker_scene_matrix)

cooccur
diag(cooccur)<-0
matrix<-ifelse(cooccur>0,1,0)

library(network)
library(igraph)
library(intergraph)
g <- graph.adjacency(matrix,  mode = "undirected", diag = FALSE)
gnew<-delete.vertices(g, which(degree(g)==0))

starwars<-asNetwork(gnew)
set.seed(1)
plot(starwars,displaylabels=T,label.cex=.7,edge.col=rgb(150,150,150,100,maxColorValue=255),vertex.col="blue",
     vertex.border=T,label.pos=1)

degree(gnew)

closeness(gnew)

betweenness(gnew)

evcent(gnew)


### MIDS ###

mids2000<-read.csv("mids 2000.csv", header=T, sep=",")

#reshape2 will allow us to restructure the data into a directed adjacency matrix#
library(reshape2)

#create an adjacency matrix object by taking the directed dyad data and reshaping it into an asymmetric square matrix#
adjmat<-as.matrix(acast(mids2000, ccode1~ccode2, value.var="cwinit"))
View(adjmat)
#Notice that when we view the MIDs data, there are a lot of NAs. 
#These occur for countries that never initiate a MID during the year under observation. In other words, these are isolates.

#We can fix this by replacing all NA's with 0's#
adjmat[is.na(adjmat)]<-0
#We have now converted a directed dyad dataset into a square matrix#

#read in vertex-level data#
vertex <- read.csv("cow2000.csv", header=T, as.is=T)
View(vertex)

library(igraph)

#take the adjacency matrix from before, create a graph object with directed ties#
gtest<-graph_from_adjacency_matrix(adjmat, mode="directed",diag=F)

#in order to look at igraph data in our console, we always define the object as a graph object with V()#
V(gtest)$name
#vertices are already given a name, defined by their correlates of war country code.

#we want to add a country abbreviation and a few other vertex attributes...#
V(gtest)$Country=as.character(vertex$abbrev1[match(V(gtest)$name,vertex$ccode1)])
#The above code says "create a new character attribute in the graph object called 'country'
#by matching the variable 'abbrev1' where 'ccode1' is matched between the graph object and vertex data.

#now do the same thing, adding a numeric variable for the country's Polity score, which represents its regime type#
V(gtest)$Regime=as.numeric(vertex$polity21[match(V(gtest)$name,vertex$ccode1)])

#now that we have our complete object with vertex-level attributes, we can plot the directed network#
#set the seed for replicability#
set.seed(1)


#plot the network with country abbreviations as the node labels#
plot(gtest, vertex.size=5, edge.arrow.size=0.5, vertex.label=V(gtest)$Country ,
     edge.color="black", layout=layout.fruchterman.reingold, vertex.color="light blue")

#The benefit of this graph is that we can see all nodes, but there are clearly a lot of isolates. Here is the same graph
#without country labels...#
set.seed(1)
plot(gtest, vertex.size=5, edge.arrow.size=0.5,vertex.label.size=0.5, vertex.label=NA ,
     edge.color="black", layout=layout.fruchterman.reingold, vertex.color="light blue")

#create a color palette to give color to nodes based on regime type (-10=autocracy, 10=democracy)#
my_resolution=21 #difference between max and min value of attribute#
my_palette=colorRampPalette(c('purple','red')) #red will represent autocracies, purple will represent democracies#

# This gives you the colors you want for every point#
my_max<-max(V(gtest)$Regime,na.rm=T)
my_vector<-V(gtest)$Regime/my_max
my_colors<-my_palette(my_resolution)[as.numeric(cut(my_vector,breaks=my_resolution))]

#plot again, but set "vertex.color" at "my_colors"#
plot(gtest, vertex.size=5, edge.arrow.size=0.5,vertex.label.size=0.5, vertex.label=NA ,
     edge.color="black", layout=layout.fruchterman.reingold, vertex.color=my_colors)

#All the action is happening in the bottom left corner, but we can't see it because of all the extra nodes that are
#never involved in MIDs. For our purposes, these isolates don't matter much because they do not contribute to the reciprocity 
#in the network. If we are interested only in nodes that are involved in MIDs, we can create a new graph object without the isolates.#
isolates<-V(gtest)[degree(gtest)==0]

#remember that "degree" refers to the number of ties a node has. we defined isolates as those with 0 ties#
#now create a new object, deleting the isolates from our original graph.#
g2<-delete.vertices(gtest,isolates)

#plot the new network#
set.seed(1)
plot(g2, vertex.size=8, edge.arrow.size=0.5,vertex.label=V(g2)$Country , 
     edge.color="black", layout=layout.fruchterman.reingold, vertex.label.degree=-pi/2, vertex.label.dist=1.5,
     vertex.color=my_colors)

## some descriptives ##

library(igraph)

# assortativity
assortativity(g2, types1=V(g2)$Regime, directed=T) #0.225

detach("package:igraph")
library(sna)
net<-asNetwork(g2)

# reciprocity
reciprocity(g2)
grecip(net, measure="correlation") #0.362

# global transitivity
gtrans(net, mode="digraph", measure ="weak") #0.046



### Refugee Networks ###

setwd("refugees")

# Replication of Salehyan (2008) Results #
salehyan<-read.csv("salehyan2.csv", header=T)
salehyanpdat<-salehyan[which(salehyan$year>=1955),]

# updated gdp data from IMF and World Bank#
salehyan<-read.csv("salehyan3.csv", header=T)


#### Build Network and Node Data ####

##3 packages ###
library(reshape2)
library(igraph)
library(intergraph)

### subset data into each year ###
data84<-salehyan[which(salehyan$year==1984),] #2372 obs#
data85<-salehyan[which(salehyan$year==1985),] #2372 obs#
data86<-salehyan[which(salehyan$year==1986),] #2372 obs#
data87<-salehyan[which(salehyan$year==1987),] #2372 obs#
data88<-salehyan[which(salehyan$year==1988),] #2372 obs#

data94<-salehyan[which(salehyan$year==1994),] #3472 obs#
data95<-salehyan[which(salehyan$year==1995),] #3472 obs#
data96<-salehyan[which(salehyan$year==1996),] #3472 obs#
data97<-salehyan[which(salehyan$year==1997),] #3472 obs#
data98<-salehyan[which(salehyan$year==1998),] #3472 obs#


# make a clustered matrix 1984-1988 #
mat80s<-data84+data85+data86+data87+data88
mat80s$ccode1<-mat80s$ccode1/5
mat80s$ccode2<-mat80s$ccode2/5


## Node Characteristics ##

# Civil War #
#mat80s$civwar1= count of civil war years#
mat80s$cwdummy1<-ifelse(mat80s$civwar1>0,1,0) #if civil war in at least 1 year#
mat80s$cwdummy2<-ifelse(mat80s$civwar1>1,1,0) #if civil war in at least 2 years#
mat80s$cwdummy3<-ifelse(mat80s$civwar1>2,1,0) #if civil war in at least 3 years#
mat80s$cwdummy4<-ifelse(mat80s$civwar1>3,1,0) #if civil war in at least 4 years#
mat80s$cwdummy5<-ifelse(mat80s$civwar1>4,1,0) #if civil war in all 5 years#

# Regime (Democracy) #

mat80s$dem1[is.na(mat80s$dem1)]<-0

mat80s$demnew1<-ifelse(mat80s$dem1>0,1,0) #if democracy in at least 1 year#
mat80s$demnew2<-ifelse(mat80s$dem1>1,1,0) #if democracy in at least 2 years#
mat80s$demnew3<-ifelse(mat80s$dem1>2,1,0) #if democracy in at least 3 years#
mat80s$demnew4<-ifelse(mat80s$dem1>3,1,0) #if democracy in at least 4 years#
mat80s$demnew5<-ifelse(mat80s$dem1>4,1,0) #if democracy in all 5 years#

# Transitional #
#mat80s$trans1= count of unstable years (greater=slow transition?)#
mat80s$transnew1<-ifelse(mat80s$trans1>0,1,0) #if unstable in at least 1 year#
mat80s$transnew2<-ifelse(mat80s$trans1>1,1,0) #if unstable in at least 2 years#
mat80s$transnew3<-ifelse(mat80s$trans1>2,1,0) #if unstable in at least 3 years#
mat80s$transnew4<-ifelse(mat80s$trans1>3,1,0) #if unstable in at least 4 years#
mat80s$transnew5<-ifelse(mat80s$trans1>4,1,0) #if unstable in all 5 years#

# GDP #
mat80s$gdpavg<-mat80s$gdp1/5 #average gdp over 5 years#
mat80s$loggdp<-log(mat80s$gdp1/5)
mat80s$stloggdp<-(mat80s$loggdp-mean(mat80s$loggdp))/sd(mat80s$loggdp)


## Edge Characteristics ##

# Binary Directed Refugee Flow Variable #
mat80s$refdummy<-ifelse(mat80s$logref2>0,1,0) #if refugee sender (at least 100 refugees)#
Refs8<-as.matrix(acast(mat80s,ccode1~ccode2, value.var="refdummy"))
Refs8[is.na(Refs8)]<-0


# Contiguity #
library(network)
mat80s$contignew<-ifelse(mat80s$contig>0,1,0) #if contiguous countries#
contiguity<-as.matrix(acast(mat80s,ccode1~ccode2, value.var="contignew"))
contiguity[is.na(contiguity)]<-0

cnet<-network(contiguity)



# Trade Dependence #
mat80s$tradedependence<-mat80s$DEPENDNEW/5 #average trade dependence over 5 years#
td<-as.matrix(acast(mat80s,ccode1~ccode2, value.var="tradedependence"))
td[is.na(td)]<-0
tdnet<-network(td)


### make a clustered matrix 1994-1998 ###
mat90s<-data94+data95+data96+data97+data98
mat90s$ccode1<-mat90s$ccode1/5
mat90s$ccode2<-mat90s$ccode2/5


## Node Characteristics ##
library(network)
# Civil War #

mat90s$cwdummy1<-ifelse(mat90s$civwar1>0,1,0) #if civil war in at least 1 year#
mat90s$cwdummy2<-ifelse(mat90s$civwar1>1,1,0) #if civil war in at least 2 years#
mat90s$cwdummy3<-ifelse(mat90s$civwar1>2,1,0) #if civil war in at least 3 years#
mat90s$cwdummy4<-ifelse(mat90s$civwar1>3,1,0) #if civil war in at least 4 years#
mat90s$cwdummy5<-ifelse(mat90s$civwar1>4,1,0) #if civil war in all 5 years#

# Regime (Democracy) #


mat90s$dem1[is.na(mat90s$dem1)]<-0

mat90s$demnew1<-ifelse(mat90s$dem1>0,1,0) #if democracy in at least 1 year#
mat90s$demnew2<-ifelse(mat90s$dem1>1,1,0) #if democracy in at least 2 years#
mat90s$demnew3<-ifelse(mat90s$dem1>2,1,0) #if democracy in at least 3 years#
mat90s$demnew4<-ifelse(mat90s$dem1>3,1,0) #if democracy in at least 4 years#
mat90s$demnew5<-ifelse(mat90s$dem1>4,1,0) #if democracy in all 5 years#


# Transitional #
#mat80s$trans1= count of unstable years (greater=slow transition?)#
mat90s$transnew1<-ifelse(mat90s$trans1>0,1,0) #if unstable in at least 1 year#
mat90s$transnew2<-ifelse(mat90s$trans1>1,1,0) #if unstable in at least 2 years#
mat90s$transnew3<-ifelse(mat90s$trans1>2,1,0) #if unstable in at least 3 years#
mat90s$transnew4<-ifelse(mat90s$trans1>3,1,0) #if unstable in at least 4 years#
mat90s$transnew5<-ifelse(mat90s$trans1>4,1,0) #if unstable in all 5 years#

# GDP #
mat90s$gdpavg<-mat90s$gdp1/5 #average gdp over 5 years#
mat90s$loggdp<-log(mat90s$gdp1/5)
mat90s$stloggdp<-(mat90s$loggdp-mean(mat90s$loggdp))/sd(mat90s$loggdp)



## Edge Characteristics ##

# Binary Directed Refugee Flow Variable #
mat90s$refdummy<-ifelse(mat90s$logref2>0,1,0) #if refugee sender (at least 100 refugees)#
Refs9<-as.matrix(acast(mat90s,ccode1~ccode2, value.var="refdummy"))
Refs9[is.na(Refs9)]<-0


# Contiguity #
mat90s$contignew<-ifelse(mat90s$contig>0,1,0) #if contiguous countries#
contiguity9<-as.matrix(acast(mat90s,ccode1~ccode2, value.var="contignew"))
contiguity9[is.na(contiguity9)]<-0

cnet9<-network(contiguity9)

# Trade Dependence #
mat90s$tradedependence<-mat90s$DEPENDNEW/5 #average trade dependence over 5 years#
td9<-as.matrix(acast(mat90s,ccode1~ccode2, value.var="tradedependence"))
td9[is.na(td9)]<-0
tdnet9<-network(td9)



#### Network Descriptives ####

### packages ###
library(igraph)
library(reshape2)

### create basic refugee flow network plots ###
graphrefs8<-graph_from_adjacency_matrix(Refs8,mode="directed",diag=F)
plot(graphrefs8)
graphrefs9<-graph_from_adjacency_matrix(Refs9,mode="directed",diag=F)
plot(graphrefs9)

### attach country code names ###
countries<-read.csv("countrycodes.csv",header=T,as.is=T)
V(graphrefs8)$country=as.character(countries$StateAbb[match(V(graphrefs8)$name,countries$CCode)])
V(graphrefs9)$country=as.character(countries$StateAbb[match(V(graphrefs9)$name,countries$CCode)])

set.seed(5)
plot(graphrefs8, vertex.size=7, edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, vertex.color="light blue",
     vertex.label.cex=0.6, vertex.label.dist=2)

set.seed(5)
plot(graphrefs9, vertex.size=7, edge.arrow.size=0.2, vertex.label=V(graphrefs9)$country,
     edge.color="black", layout=layout.fruchterman.reingold, vertex.color="light blue",
     vertex.label.cex=0.6, vertex.label.dist=2)

### attach civil war dummy ###
V(graphrefs8)$civilwardummy=as.numeric(mat80s$cwdummy1[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$civilwardummy=as.numeric(mat90s$cwdummy1[match(V(graphrefs9)$name,mat90s$ccode1)])

V(graphrefs8)$color=V(graphrefs8)$civilwardummy
V(graphrefs8)$color=gsub("1","red",V(graphrefs8)$color)
V(graphrefs8)$color=gsub("0","blue",V(graphrefs8)$color)
V(graphrefs9)$color=V(graphrefs9)$civilwardummy
V(graphrefs9)$color=gsub("1","red",V(graphrefs9)$color)
V(graphrefs9)$color=gsub("0","blue",V(graphrefs9)$color)

set.seed(5)
plot(graphrefs8, vertex.size=7, edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9, vertex.size=7, edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)

## Attach all node attributes generated above ##

# civil war #
V(graphrefs8)$cw1=as.numeric(mat80s$cwdummy1[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$cw1=as.numeric(mat90s$cwdummy1[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$cw2=as.numeric(mat80s$cwdummy2[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$cw2=as.numeric(mat90s$cwdummy2[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$cw3=as.numeric(mat80s$cwdummy3[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$cw3=as.numeric(mat90s$cwdummy3[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$cw4=as.numeric(mat80s$cwdummy4[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$cw4=as.numeric(mat90s$cwdummy4[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$cw5=as.numeric(mat80s$cwdummy5[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$cw5=as.numeric(mat90s$cwdummy5[match(V(graphrefs9)$name,mat90s$ccode1)])

# regime #
V(graphrefs8)$d1=as.numeric(mat80s$demnew1[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$d1=as.numeric(mat90s$demnew1[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$d2=as.numeric(mat80s$demnew2[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$d2=as.numeric(mat90s$demnew2[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$d3=as.numeric(mat80s$demnew3[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$d3=as.numeric(mat90s$demnew3[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$d4=as.numeric(mat80s$demnew4[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$d4=as.numeric(mat90s$demnew4[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$d5=as.numeric(mat80s$demnew5[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$d5=as.numeric(mat90s$demnew5[match(V(graphrefs9)$name,mat90s$ccode1)])

# transitional #
V(graphrefs8)$t1=as.numeric(mat80s$transnew1[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$t1=as.numeric(mat90s$transnew1[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$t2=as.numeric(mat80s$transnew2[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$t2=as.numeric(mat90s$transnew2[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$t3=as.numeric(mat80s$transnew3[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$t3=as.numeric(mat90s$transnew3[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$t4=as.numeric(mat80s$transnew4[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$t4=as.numeric(mat90s$transnew4[match(V(graphrefs9)$name,mat90s$ccode1)])
V(graphrefs8)$t5=as.numeric(mat80s$transnew5[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$t5=as.numeric(mat90s$transnew5[match(V(graphrefs9)$name,mat90s$ccode1)])

# gdp #
V(graphrefs8)$g=as.numeric(mat80s$gdpavg[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$g=as.numeric(mat90s$gdpavg[match(V(graphrefs9)$name,mat90s$ccode1)])

V(graphrefs8)$lg=as.numeric(mat80s$loggdp[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$lg=as.numeric(mat90s$loggdp[match(V(graphrefs9)$name,mat90s$ccode1)])

V(graphrefs8)$stlg=as.numeric(mat80s$stloggdp[match(V(graphrefs8)$name,mat80s$ccode1)])
V(graphrefs9)$stlg=as.numeric(mat90s$stloggdp[match(V(graphrefs9)$name,mat90s$ccode1)])






## Descriptive statistics plots ##
library(network)
library(intergraph)
library(sna)
netrefs80s<-asNetwork(graphrefs8)
netrefs90s<-asNetwork(graphrefs9)

## Centrality ##

# in degree #
indegree80s<-degree(netrefs80s, cmode="indegree", gmode="digraph")
indegree90s<-degree(netrefs90s, cmode="indegree", gmode="digraph")
# out degree #
outdegree80s<-degree(netrefs80s, cmode="outdegree", gmode="digraph")
outdegree90s<-degree(netrefs90s, cmode="outdegree", gmode="digraph")
# total (Freeman) #
freeman80s<-degree(netrefs80s, cmode="freeman", gmode="digraph")
freeman90s<-degree(netrefs90s, cmode="freeman", gmode="digraph")
# betweenness (standard) #
betweenness80s<-betweenness(netrefs80s,gmode="digraph")
betweenness90s<-betweenness(netrefs90s,gmode="digraph")

# store in matrices #
Refs8$indegree<-indegree80s
Refs9$indegree<-indegree90s
Refs8$outdegree<-outdegree80s
Refs9$outdegree<-outdegree90s
Refs8$freeman<-freeman80s
Refs9$freeman<-freeman90s
Refs8$betweenness<-betweenness80s
Refs9$betweenness<-betweenness90s

#plot size based on in degree centrality#
V(graphrefs8)$size=degree(netrefs80s, cmode="indegree", gmode="digraph")
V(graphrefs8)$size
V(graphrefs9)$size=degree(netrefs90s, cmode="indegree", gmode="digraph")
V(graphrefs9)$size

set.seed(5)
plot(graphrefs8,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988: In-degree Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998: In-degree Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)

#plot size based on out degree centrality#
V(graphrefs8)$size=degree(netrefs80s, cmode="outdegree", gmode="digraph")
V(graphrefs8)$size
V(graphrefs9)$size=degree(netrefs90s, cmode="outdegree", gmode="digraph")
V(graphrefs9)$size

set.seed(5)
plot(graphrefs8,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988: Out-degree Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998: Out-degree Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)

#plot size based on freeman (total) centrality#
V(graphrefs8)$size=degree(netrefs80s, cmode="freeman", gmode="digraph")
V(graphrefs8)$size
V(graphrefs9)$size=degree(netrefs90s, cmode="freeman", gmode="digraph")
V(graphrefs9)$size

set.seed(5)
plot(graphrefs8,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988: Freeman Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998: Freeman Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)

#plot size based on betweenness centrality#
V(graphrefs8)$size=betweenness(netrefs80s,gmode="digraph")
V(graphrefs8)$size
V(graphrefs9)$size=betweenness(netrefs90s,gmode="digraph")
V(graphrefs9)$size

set.seed(5)
plot(graphrefs8,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988: Betweenness Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998:  Betweenness Centrality")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)

## GL Reciprocity ##
recip80s<-grecip(netrefs80s, measure="correlation")
recip80s
recip90s<-grecip(netrefs90s, measure="correlation")
recip90s

## Transitivity ##

# global #
transrefs80sg<-transitivity(graphrefs8, type="global")
transrefs80sg
transrefs90sg<-transitivity(graphrefs9, type="global")
transrefs90sg

# local #
transrefs80sl<-transitivity(graphrefs8, type="local")
transrefs90sl<-transitivity(graphrefs9, type="local")

# store in matrices #
Refs8$transitivity<-transrefs80sl
Refs9$transitivity<-transrefs90sl

#plot size based on local transitivity#
transrefs80sl[is.na(transrefs80sl)]<-.00001
transrefs90sl[is.na(transrefs90sl)]<-.00001

V(graphrefs8)$size=transrefs80sl*10
V(graphrefs8)$size
V(graphrefs9)$size=transrefs90sl*10
V(graphrefs9)$size

set.seed(5)
plot(graphrefs8,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1984-1988: Transitivity")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)
set.seed(5)
plot(graphrefs9,  edge.arrow.size=0.2, vertex.label=V(graphrefs8)$country,
     edge.color="black", layout=layout.fruchterman.reingold, 
     vertex.label.cex=0.6, vertex.label.dist=1, main="Refugee Flows 1994-1998: Transitivity")
legend("bottomleft", legend=c("Civil War", "No Civil War"),
       col=c("red","blue"), bty="n", pch=19)


## Assortativity ##

# civil wars #
assortativitycw80s<-assortativity(graphrefs8, types1=V(graphrefs8)$civilwardummy, types2=NULL,directed=T)
assortativitycw80s
assortativitycw90s<-assortativity(graphrefs9, types1=V(graphrefs9)$civilwardummy, types2=NULL,directed=T)
assortativitycw90s





#### ERGM Analysis ####

### packages ###
library(intergraph)
library(statnet)
library(ergm)
refnet8<-asNetwork(graphrefs8)

refnet9<-asNetwork(graphrefs9)

### Notes:
# always set seed for replicability
# this control sequence takes a long time. If you want to play with it a bit, reduce sample size, burnins, and intervals
# default interval and sample size is 1024
# burnins must be less than sample size

set.seed(782566)
ergm80sLEC<-ergm(refnet8~edges+twopath+gwesp(0,fixed=T)+edgecov(cnet)+nodeicov("cw3")+nodeocov("cw3")
                +nodematch("cw3")+nodeicov("stlg")+nodeocov("stlg")+nodeicov("d5")+nodeocov("d5")+edgecov(tdnet),
                control=control.ergm(MCMC.interval=6000, MCMC.burnin=23000, seed=1, MCMC.samplesize=62000))

set.seed(782566)
ergm90sLEC<-ergm(refnet9~edges+twopath+gwesp(0,fixed=T)+edgecov(cnet)+nodeicov("cw3")+nodeocov("cw3")
                +nodematch("cw3")+nodeicov("stlg")+nodeocov("stlg")+nodeicov("d5")+nodeocov("d5")+edgecov(tdnet),
                control=control.ergm(MCMC.interval=6000, MCMC.burnin=23000, seed=1, MCMC.samplesize=62000))


mcmc.diagnostics(ergm80sLEC)
mcmc.diagnostics(ergm90sLEC)

# can specify which parameters to test model against #
gof8<-gof(ergm80sLEC, GOF=~model)
plot(gof8)

gof9<-gof(ergm90sLEC, GOF=~model)
plot(gof9)






### fin ###

