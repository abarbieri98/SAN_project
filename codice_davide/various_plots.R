library(sand) 
library(RColorBrewer)


setwd("/home/davide/università/statistical analysis of networks/project")
rete1215<-readRDS("rete1215_clean_update.rds")
rete1619<-readRDS("rete1619_clean.rds")
rete1215_statistici<-readRDS("rete1215_clean_statistici_update.rds")
rete1619_statistici<-readRDS("final_rete1619_statistici")



#plots with multidimensional scaling (based on adjacency matrix)
mds_layout_adj<-layout_with_mds(rete1215, dist = get.adjacency(rete1215, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1215)$ssd)), "Set3") #create color palette
plot(rete1215, layout = mds_layout_adj, vertex.size=4, vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215)$ssd)), fill=pal, border=NA)

#to change also the size of the vertexes
#plot(rete1215, layout = mds_layout_adj, vertex.size=0.4*degree(rete1215), vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])


mds_layout_adj<-layout_with_mds(rete1619, dist = get.adjacency(rete1619, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1619)$ssd)), "Set3") #create color palette
plot(rete1619, layout = mds_layout_adj, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2,  vertex.color = pal[as.numeric(as.factor(V(rete1619)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619)$ssd)), fill=pal, border=NA)

#too slow... and result is ugly
#plot(rete1619, layout = mds_layout_adj, vertex.size=0.4*degree(rete1619), vertex.label="", edge.width=E(rete1619)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1619)$ssd))])

?jpeg

#other layouts
jpeg("rete1215_clean_kk.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1215)$ssd)), "Set3") #create color palette
plot(rete1215, layout=layout_with_kk, vertex.size=4, vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215)$ssd)), fill=pal, border=NA)
dev.off()

#to change also the size of the vertexes
#plot(rete1215, layout = mds_layout_adj, vertex.size=degree(rete1215), vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])

jpeg("rete1619_clean_kk.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1619)$ssd)), "Set3") #create color palette
plot(rete1619, layout=layout_with_kk, vertex.size=4, vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1619)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619)$ssd)), fill=pal, border=NA)
dev.off()






#plots with multidimensional scaling (based on adjacency matrix)
mds_layout_adj<-layout_with_mds(rete1215_statistici, dist = get.adjacency(rete1215_statistici, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1215_statistici)$ssd)), "Set3") #create color palette
plot(rete1215_statistici, layout = mds_layout_adj, vertex.size=4, vertex.label="", edge.width=E(rete1215_statistici)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215_statistici)$ssd)), fill=pal, border=NA)

#to change also the size of the vertexes
#plot(rete1215, layout = mds_layout_adj, vertex.size=degree(rete1215), vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])


mds_layout_adj<-layout_with_mds(rete1619_statistici, dist = get.adjacency(rete1619_statistici, sparse=F), dim = 2) #use adjency matrix as distance
pal <- brewer.pal(length(unique(V(rete1619_statistici)$ssd)), "Set3") #create color palette
plot(rete1619_statistici, layout = mds_layout_adj, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2,  vertex.color = pal[as.numeric(as.factor(V(rete1619_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_statistici)$ssd)), fill=pal, border=NA)




#other layouts
jpeg("rete1215_statistici_kk.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1215_statistici)$ssd)), "Set3") #create color palette
plot(rete1215_statistici, layout=layout_with_kk, vertex.size=4, vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215_statistici)$ssd)), fill=pal, border=NA)
dev.off()

#to change also the size of the vertexes
#plot(rete1215, layout = mds_layout_adj, vertex.size=degree(rete1215), vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])

jpeg("rete1619_statistici_kk.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1619_statistici)$ssd)), "Set3") #create color palette
plot(rete1619_statistici, layout=layout_with_kk, vertex.size=4, vertex.label="", edge.width=E(rete1215)$Weight, vertex.color = pal[as.numeric(as.factor(V(rete1619_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_statistici)$ssd)), fill=pal, border=NA)
dev.off()

















