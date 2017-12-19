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

ll<- subset(transfers, LEAGUE == "La Liga")

View(ll)
ll$ew<-ll$PRICE/sum(ll$PRICE)

ll$fee<-ll$PRICE/sum(ll$PRICE)

mat<-data.frame(FROM=ll$FROM, TO=ll$TO)
mat<-subset(mat, fee = 0.0001)
mat$fee<-100*ll$fee

View(mat)


net<-network(mat,matrix.type="edgelist")


ll2<-graph_from_data_frame(mat, directed=TRUE)
plot(ll2, edge.width=E(ll2)$weight)


net1<-intergraph::asNetwork(ll2)
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}


plot(net1, vertex.cex= rescale(sna::degree(net1),1,10),label=ifelse(sna::degree(net1) > 20, network.vertex.names(net1), NA),mode="circle")


#Density
# 177/(94*93)

#degree centrality
sna::degree(net1)

#indegree (buying players the most)
sna::degree(net1, cmode="indegree")

#outdegree (selling)
sna::degree(net1, cmode="outdegree")



#closeness

net_i<-intergraph:: asIgraph(net1)
igraph::closeness(net_i,mode="all", normalized=T)

dd<-data.frame(network.vertex.names(net1), sna::degree(net1),sna::degree(net1, cmode="indegree"),sna::degree(net1, cmode="outdegree"),igraph::closeness(net_i,mode="all", normalized=T))
View(dd)

#betweennes 
sna::betweenness(net1)

dd<-data.frame(network.vertex.names(net1), sna::degree(net1),sna::degree(net1, cmode="indegree"),sna::degree(net1, cmode="outdegree"),igraph::closeness(net_i,mode="all", normalized=T), sna::betweenness(net1))
View(dd)


