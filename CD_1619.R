library(igraph)
library(igraphdata)
library(statnet)
library(sand)
library(ggplot2)

setwd("/Users/anna/Desktop/Statistical Analysis of Networks/PROJECT/SAN_project/")
rete1619_clean<-readRDS("rete1619_clean.rds")
rete1619_clean<-simplify(rete1619_clean)

igraph::list.vertex.attributes(rete1619_clean)
igraph::list.edge.attributes(rete1619_clean)

summary(rete1619_clean)  #8085 nodes and 38173 edges

sum(V(rete1619_clean)$ssd=='OTHER') # 7404 nodes are OTHER 
8098-sum(V(rete1619_clean)$ssd=='OTHER') #694 nodes are Statisticians (8.4%)

###### KEEP ONLY STATISTICIANS ######

rete1619_stat<-igraph::delete.vertices(rete1619_clean, V(rete1619_clean)[V(rete1619_clean)$ssd=='OTHER'])

igraph.options(vertex.size=3, vertex.label=NA, vertex.color="orange", edge.size=15, edge.color="grey", edge.arrow.size=0.2)

plot(rete1619_stat, layout=layout_with_kk) 

count_components(rete1619_stat)
comps1619_stat <- decompose.graph(rete1619_stat)
table(sapply(comps1619_stat, vcount))
rete1619_giant_stat<-comps1619_stat[[4]]

plot(rete1619_giant_stat, layout=layout_with_fr) 

is_connected(rete1619_giant_stat)


###### PLOT ######

g<-rete1619_giant_stat

V(g)[ssd == "SECS-S/01"]$color <- "red"
V(g)[ssd == "SECS-S/02"]$color <- "dodgerblue"
V(g)[ssd == "SECS-S/03"]$color <- "pink"
V(g)[ssd == "SECS-S/04"]$color <- "lightblue"
V(g)[ssd == "SECS-S/05"]$color <- "yellow"

par(mar=c(0.5,0.5,0.5,0.5)) ### set margins

deg_g <- igraph::degree(g, mode = "all", normalized = T) 
#V(g)$size<-4*sqrt(graph.strength(g))
#V(g)$size <- V(g)$size * .5
V(g)$size <- deg_g*200 # creates a vector named size, this vector will be used to set the dimension of the nodes
E(g)$width <- E(g)$weight/3
plot(g, layout=layout_with_fr, vertex.label = NA, vertex.label.dist=0, vertex.label.cex=0.8) 


###### LOUVAIN METHOD ######


# tuning resolution parameter
gini <- function(x){
  f <- table(x)/length(x)
  sum(f^2)
}

gini_mean<-function(g, cl){
  n_clusters=length(cl)
  mean_gini<-0
  for(i in 1:n_clusters){
    index<-gini(V(g)$ssd[V(g)$Louv.cluster==i])
    mean_gini<-mean_gini+index
  }
  mean_gini<-mean_gini/n_clusters
  return(mean_gini)
}

best_res_gini<-function(g, res){
  mean_gini<-c()
  for(r in res){
    gini<-c()
    for (i in 1:1000){
      g<-permute(g, sample(vcount(g)))
      cl <- cluster_louvain(g, resolution = r)
      V(g)$Louv.cluster <- membership(cl)
      gini_index<-gini_mean(g, cl)
      gini<-append(gini, gini_index)
    }
    mean_gini<-append(mean_gini,mean(gini))
  }
  best_res<-res[which.max(mean_gini)]
  print(mean_gini)
  return(best_res)
}

res<-seq(0.5, 1.5, length.out=11)


r<-best_res_gini(rete1619_giant_stat, res)

#Louvain method
clusterlouvain <- cluster_louvain(rete1619_giant_stat, weights=E(rete1619_giant_stat)$weight, resolution = r)

print(clusterlouvain)
n_clusters=length(clusterlouvain)
sizes(clusterlouvain)
modularity(clusterlouvain, weight=E(rete1619_giant_stat)$weight)

V(rete1619_giant_stat)$Louv.cluster <- membership(clusterlouvain)

#plot
colors <- rainbow(max(membership(clusterlouvain)))
plot(clusterlouvain, rete1619_giant_stat,layout=layout_with_fr, 
     vertex.size = 2, vertex.color=colors[membership(clusterlouvain)], 
     vertex.label = NA, edge.width = E(rete1619_giant_stat)$weight/3)

#gini index for each cluster
for(i in 1:n_clusters){
  index<-gini(V(rete1619_giant_stat)$ssd[V(rete1619_giant_stat)$Louv.cluster==i])
  print(paste(round(index,digits=4), ' cluster: ', i))}

#intra-cluster density
sapply(unique(membership(clusterlouvain)), function(gg) {
  subg1<-induced.subgraph(rete1619_giant_stat, which(membership(clusterlouvain)==gg))
  edge_density(subg1)
})
edge_density(rete1619_giant_stat)

#inter-cluster edges for each pair of communities
cs <- data.frame(combn(unique(membership(clusterlouvain)),2))
cx <- sapply(cs, function(x) {
  es<-E(rete1619_giant_stat)[V(rete1619_giant_stat)[membership(clusterlouvain)==x[1]] %--% 
             V(rete1619_giant_stat)[membership(clusterlouvain)==x[2]]]    
  length(es)
})
cbind(t(cs),inter.edges=cx)





#stability of the algorithm 
mod_nc<-c()
n_c<-c()
for (i in 1:1000){
  rete1619_giant_stat<-permute(rete1619_giant_stat, sample(vcount(rete1619_giant_stat)))
  cl <- cluster_louvain(rete1619_giant_stat, resolution = r)
  n_c<-append(n_c,length(cl))
  mod_nc<-append(mod_nc,modularity(cl))
}
df<-data.frame(n=n_c, m=mod_nc)


df %>% ggplot(aes(x = n)) +
  geom_histogram(color = "white", fill = "blue", bins = 20) +
  labs(title = "Distribution of number of communities over 1000 trials", x = "number of communities", y = "count") +
  theme_gray()

df %>% ggplot(aes(x = m)) +
  geom_histogram(color = "white", fill = "red", bins = 100) +
  labs(title = "Distribution of modularity over 1000 trials", x = "modularity", y = "count") +
  theme_gray()


df %>% ggplot(aes(x = m, y = n)) +
  geom_point() +
  labs(title = "Modularity vs Number of Communities over 100 trials", y = "number of communities", x = "modularity") +
  theme_gray()
