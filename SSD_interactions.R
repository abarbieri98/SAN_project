#### SSD interactions

library(igraph)
library(intergraph) # needed to convert networks to igraph graphs
library(ergm)

data <- readRDS("rete1215_clean.rds")
data1619<-readRDS("rete1619_clean.rds")

#1215

# We collapse all the statisticians in nodes representing their SSDs,
# summing all the connections to get a single edge per pair of nodes.
SSD <- contract(data, as.numeric(as.factor(V(data)$ssd)),list(ssd="random", "ignore", weight = "sum", people = "sum"))

summary(SSD)

SSD<-simplify(SSD, remove.loops = F, "sum")
SSD<-igraph::delete_edges(SSD,1) # Remove loop within "other"
SSD_weigths_normalized<-rep(NA,18)

# Compute the normalized weight as explained in the report
for(i in 1:18){
  tail<-tail_of(SSD,i)
  head<-head_of(SSD,i)
  den<-V(SSD)[tail]$people+V(SSD)[head]$people
  SSD_weigths_normalized[i] <- E(SSD)[i]$weight/den
}
E(SSD)$weight_norm<-SSD_weigths_normalized
colors <- hcl.colors(length(unique(V(SSD)$ssd)), palette = "Zissou1")


jpeg("SSD_Norm_1215.jpg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
par(mar=rep(.5,4))
igraph::plot.igraph(SSD, vertex.size=ifelse(test = V(SSD)$ssd != "OTHER", V(SSD)$people/8, 50), edge.width=E(SSD)$weight_norm*20, 
                    vertex.label=V(SSD)$ssd,
                    vertex.label.dist = 0,
                    vertex.label.color = "black",
                    vertex.label.cex = .5,
                    vertex.color = colors[(as.numeric(as.factor(V(SSD)$ssd)))],
                    layout = layout_as_star)
legend("topleft",bty = "n",
       legend=levels(as.factor(V(SSD)$people)),
       border=NA)
dev.off()

as_adj(SSD,attr = "weight_norm")
# Total strength of each node
strength(SSD,V(SSD),loops = T, weights = SSD_weigths_normalized)

#1619
SSD1619 <- contract(data1619, as.numeric(as.factor(V(data1619)$ssd)),list(ssd="random", "ignore", weight = "sum", people = "sum"))

summary(SSD1619)
SSD1619<-simplify(SSD1619, remove.loops = F, "sum")
SSD1619<-igraph::delete_edges(SSD1619,1) # Remove loop within "other"
SSD_weigths_normalized<-rep(NA,ecount(SSD1619))

for(i in 1:19){
  tail<-tail_of(SSD1619,i)
  head<-head_of(SSD1619,i)
  den<-V(SSD1619)[tail]$people+V(SSD1619)[head]$people
  SSD_weigths_normalized[i] <- E(SSD1619)[i]$weight/den
}
E(SSD1619)$weight_norm<-SSD_weigths_normalized
colors <- hcl.colors(length(unique(V(SSD1619)$ssd)), palette = "Zissou1")
par(mar=rep(.5,4))
jpeg("SSD_Norm_1619.jpg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
igraph::plot.igraph(SSD1619, vertex.size=ifelse(test = V(SSD1619)$ssd != "OTHER", V(SSD1619)$people/8, 50), edge.width=E(SSD1619)$weight_norm*20, 
                    vertex.label=V(SSD1619)$ssd,
                    vertex.label.color = "black",
                    vertex.label.cex = .5,
                    vertex.color = colors[(as.numeric(as.factor(V(SSD1619)$ssd)))],
                    layout = layout_as_star)
legend("topleft",bty = "n",
       legend=levels(as.factor(V(SSD)$people)),
       border=NA)
dev.off()

as_adj(SSD1619,attr = "weight_norm")
strength(SSD1619,V(SSD1619),loops = T, weights = SSD_weigths_normalized)


