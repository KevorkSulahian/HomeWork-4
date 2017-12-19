 ####  Today we are analyzing player transfer in football (or known as soccer in USA), considered as one of the biggest markets in the world that generates from various stakeholders: Clubs, agents, sponsors , professional players, the fans , etc. 

#### Transfers in football is simply transferring one player from one football club to the other, players can be transferred under a specified fee, loan and an agreed contract between the player and the club. There are two windows to complete a transfer in football, every football assosication decides the beginning and the end of their windows but according to the rules it may not exceed twelve weeks for the summer window (pre-season) and it may not exceed four weeks for the winter window (mid-season.)
 #### Our data set "transfers" provides us with data for all the European leagues (I don't really watch football so this may not be true) from the season 2007/2008 to 2016/2017.
#### This is R based analysis, we will focus on the code a bit more than usual.

### Let us start with the codes

``` First the Librarys

library(devtools)
install_github("Habet/CSE270")
library("SportsAnalytics270")
View(transfers)

library(circlize)
library(plyr)
library(sna)
library(igraph)
library(ggplot2)
library(scales)
library(dplyr)
library(intergraph)


```

## Code prepration
### For this section I want to calculate La Liga's transfer market.
```

ll<- subset(transfers, LEAGUE == "La Liga") # taking only laliga transfers

ll$ew<-ll$PRICE/sum(ll$PRICE) # adding Edge Weight

ll$fee<-ll$PRICE/sum(ll$PRICE) # adding the Fee

mat<-data.frame(FROM=ll$FROM, TO=ll$TO)

mat<-subset(mat, fee = 0.0001)

mat$fee<-100*ll$fee

```
### Plotting
```
plot(ll2, edge.width=E(ll2)$weight)
```
![](https://github.com/KevorkSulahian/HomeWork-4/blob/master/Rplot03.png)

### Second Plot
 
```


net1<-intergraph::asNetwork(ll2)
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


plot(net1, vertex.cex= rescale(sna::degree(net1),1,10),label=ifelse(sna::degree(net1) > 20, network.vertex.names(net1), NA),mode="circle")

```
![](https://github.com/KevorkSulahian/HomeWork-4/blob/master/Rplot02.png)

## Now lets calculate the Degree Centrality 

#### First of all I already calcualted the Desnsity
#### 177/(94 * 93)

```

#degree centrality
sna::degree(net1)

#Indegree Centrality is the club that buys a player and this will show who buys the most players in La Liga

sna::degree(net1, cmode="indegree")

# Outdegree Centrality is the club that sells a player and this will show who sells the most players in La Liga
sna::degree(net1, cmode="outdegree")

# closness

net_i<-intergraph:: asIgraph(net1)
igraph::closeness(net_i,mode="all", normalized=T)

closness<-data.frame(network.vertex.names(net1), sna::degree(net1),sna::degree(net1, cmode="indegree"),sna::degree(net1, cmode="outdegree"),igraph::closeness(net_i,mode="all", normalized=T))

# betweeness
sna::betweenness(net1)

betweennes <-data.frame(network.vertex.names(net1), sna::degree(net1),sna::degree(net1, cmode="indegree"),sna::degree(net1, cmode="outdegree"),igraph::closeness(net_i,mode="all", normalized=T), sna::betweenness(net1))


```

#### From the last couple of lines of codes when can finally get our table of who sells, buys and betweenes centrality in La Liga.

|In-degree      |	Out degree    |	Betweenes       |
| ------------- |:-------------:|----------------:|
|Sevilla	      |Valencia	      |Sevilla          |
|Malaga	        |Benfica	      |Valencia         |
|Atletico Madrid|Atletico Madrid|	Atletico Madrid |
|Valencia	      |Sevilla	      |Villarreal       |

## Conclusion
#### From our analysis we understood that in La Liga the top team (Real Madrid and Barcelona) do not buy or sell much of their players instead they try to work on their composition, and the most active teams are Valencia, Sevilla and Atletico Madrid. But this does not mean that the same is true for other Leagues, this is just what happens in La Liga, to conclude even though the top two teams are not really high on our list yet they are not behind and the teams that are high in our list idicates that these clubs have ambitious to become better in the coming year.

