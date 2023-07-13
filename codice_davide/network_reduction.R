library(sand) 
library(RColorBrewer)

setwd("/home/davide/università/statistical analysis of networks/project")

rete1619_clean<-readRDS("rete1619_clean.rds")


#k-core decomposition 

cores <- graph.coreness(rete1619_clean)  #get the k core decomposition
table(cores)


#layout to plot the k-core decomposition 
CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}


ll <- CorenessLayout(rete1619_clean)
pal <- brewer.pal(length(unique(V(rete1619_clean)$ssd)), "Set3") #create color palette
plot(rete1619_clean, layout=ll, vertex.size=4, vertex.label="", edge.width=0.8, vertex.color = pal[as.numeric(as.factor(V(rete1619_clean)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_clean)$ssd)), fill=pal, border=NA)



table(cores)
#create two subgraph, one containing only nodes with high coreness and one low 
rete1619_clean_core_high <- induced.subgraph(rete1619_clean, vids = which(cores>8))
rete1619_clean_core_low <- induced.subgraph(rete1619_clean, vids = which(cores<9))

pal <- brewer.pal(length(unique(V(rete1619_clean_core_high)$ssd)), "Set3") #create color palette
plot(rete1619_clean_core_high, vertex.size=4, vertex.label="", edge.width=0.8, vertex.color = pal[as.numeric(as.factor(V(rete1619_clean_core_high)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_clean_core_high)$ssd)), fill=pal, border=NA)
#with this division there are not enough statistician in high network
table(get.vertex.attribute(rete1619_clean_core_high, name ="ssd"))
table(get.vertex.attribute(rete1619_clean_core_low, name ="ssd"))

#this keep more or less half of the statisticians, would it make sense to work only on this?
rete1619_clean_core_medium <- induced.subgraph(rete1619_clean, vids = which(cores>4))

#table(get.vertex.attribute(induced.subgraph(rete1619_clean, vids = which(cores<5)), name ="ssd"))


#graph compression...


