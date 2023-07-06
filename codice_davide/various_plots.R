library(sand) 
library(RColorBrewer)


setwd("/home/davide/università/statistical analysis of networks/project")
rete1215<-readRDS("rete1215.rds")
rete1619<-readRDS("rete1619.rds")


rete1215<-simplify(rete1215)
rete1619<-simplify(rete1619)



statIdx<-which(V(rete1215)$ssd != "OTHER")
rete1215_statistici<-induced.subgraph(rete1215 ,statIdx)
gn.comm_stat <- cluster_edge_betweenness(rete1215_statistici)


mds_layout_adj<-layout_with_mds(rete1215_statistici, dist = get.adjacency(rete1215_statistici, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1215_statistici)$ssd)), "Set3") #create color palette
plot(rete1215_statistici, layout = mds_layout_adj, vertex.size=6, vertex.label="", edge.width=0.8, edge.arrow.size=0.2,  vertex.color = pal[as.numeric(as.factor(V(rete1215_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215_statistici)$ssd)), fill=pal, border=NA)


statIdx1619<-which(V(rete1619)$ssd != "OTHER")
rete1619_statistici<-induced.subgraph(rete1619 ,statIdx1619)

mds_layout_adj<-layout_with_mds(rete1619_statistici, dist = get.adjacency(rete1619_statistici, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1619_statistici)$ssd)), "Set3") #create color palette
plot(rete1619_statistici, layout = mds_layout_adj, vertex.size=6, vertex.label="", edge.width=0.8, edge.arrow.size=0.2,  vertex.color = pal[as.numeric(as.factor(V(rete1619_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_statistici)$ssd)), fill=pal, border=NA)



