library(sand) 
setwd("/home/davide/università/statistical analysis of networks/project")


rete1215_clean<-readRDS("rete1215_clean.rds")
rete1619_clean<-readRDS("rete1619_clean.rds")

#in the cleaned data researchers without a direct connection to the statisticians are removed

vcount(rete1215_clean)    # the number of nodes is 1419
ecount(rete1215_clean)    # the number of edges 4606
edge_density(rete1215_clean)  # the density is 0.004, the network  is very sparse

vcount(rete1619_clean)    # the number of nodes is 8085
ecount(rete1619_clean)    # the number of edges 38173
edge_density(rete1619_clean)  # the density is 0.001, the network  is very sparse

#plot(rete1215, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
#plot(rete1619, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
?edge_density

rete1215_clean_deg <- degree(rete1215_clean, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_clean_deg)  #the mean degree is 6.49
max(rete1215_clean_deg)   #the maximum degree is 56
min(rete1215_clean_deg)   #the minimum degree is 0
hist(rete1215_clean_deg, col="lightblue", xlim=c(0, 56), xlab="degree", ylab="Frequency", main="")   
sum(rete1215_clean_deg==0) #get the number of isolated nodes (just one)

  
rete1619_clean_deg <- degree(rete1619_clean, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_clean_deg)  #the mean degree is 9.44
max(rete1619_clean_deg)   #the maximum degree is 483
min(rete1619_clean_deg)   #the minimum degree is 1 
hist(rete1619_clean_deg, breaks = 60, col="lightblue", xlim=c(1, 483), xlab="degree", ylab="Frequency", main="")  #very skewed


#check weigth distribution
weights1215_clean<-E(rete1215_clean)$weight
mean(weights1215_clean)  #the mean weight is 1.15
max(weights1215_clean)   #the maximum weight is 16 
min(weights1215_clean)   #the minimum weight is 1 
strength1215_clean<-graph.strength(rete1215_clean, vids=V(rete1215_clean), weights = weights1215_clean) #get the sum of all the weights of a single node
mean(strength1215_clean)  #the mean strength is 7.52
max(strength1215_clean)   #the maximum strength is 70 
min(strength1215_clean)   #the minimum strength is 0
#maybe a logharitmic scale would be better
par(mfrow=c(1,2))
hist(weights1215_clean, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1215_clean, col="lightblue", xlim=c(0, 70), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

weights1619_clean<-E(rete1619_clean)$weight
mean(weights1619_clean)  #the mean weight is 1.27
max(weights1619_clean)   #the maximum weight is 33 
min(weights1619_clean)   #the minimum weight is 1 
strength1619_clean<-graph.strength(rete1619_clean, vids=V(rete1619_clean), weights = weights1619_clean) #get the sum of all the weights of a single node
mean(strength1619_clean)  #the mean strength is 12.03
max(strength1619_clean)   #the maximum strength is 855
min(strength1619_clean)   #the minimum strength is 1 
par(mfrow=c(1,2))
hist(weights1619_clean, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1619_clean, col="lightblue", xlim=c(1, 869), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   


mean_distance(rete1215_clean)   # The average of all shortest paths 3.06
mean_distance(rete1619_clean)   # The average of all shortest paths 9.16


transitivity(rete1215_clean, type = "global")     #Global Transitivity (clustering coefficient) it is 0.86
#transitivity(rete1215, type = "local")      # local transitivity for each node

transitivity(rete1619_clean, type = "global")     #Global Transitivity (clustering coefficient) it is 0.50
#transitivity(rete1619, type = "local")      # local transitivity for each node


#centrality scores and ego networks of central nodes
degree_centrality_rete1215_clean<-as.vector(degree(rete1215_clean, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1215_clean<-as.vector(betweenness(rete1215_clean, normalized = T, weights = E(rete1215_clean)$weight)) #compute the betweenness centrality of every node
#eigen_centrality_rete1215_clean<-as.vector(eigen_centrality(rete1215_clean, (rete1215_clean)$weight))  #compute the degree centrality of every node

#centrality scores and ego networks of central nodes
degree_centrality_rete1619_clean<-as.vector(degree(rete1619_clean, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1619_clean<-as.vector(betweenness(rete1619_clean, normalized = T)) #compute the betweenness centrality of every node
#eigen_centrality_rete1619_clean<-as.vector(eigen_centrality(rete1619_clean, weights=(rete1619_clean)$weight))  #compute the degree centrality of every node

#check the node with highest degree centrality
top10_degree_1215<-order(degree_centrality_rete1215_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_degree_1619<-order(degree_centrality_rete1619_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

#check if a name is present in both top10 independently of positions
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_degree_1215[i])==get.vertex.attribute(graph=rete1619_clean, name="name", index=top10_degree_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_clean, name="name", index=i)))
    }
  }  
}
#the same author has the highest degree centrality in both datasets id 6508086535
#the 2nd author in 1215 is seventh in 1619 id 24332388800
get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215[2]) #get the info about node in both top ten
get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1619[7]) #get the info about node in both top ten


get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215[1]) #get the info about most central node
get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619[1]) #get the info about most central node


# ego network considering node present in both top 10
rete1215_clean_ego_degree <- induced.subgraph(rete1215_clean, neighborhood(rete1215_clean, order= 1, nodes= top10_degree_1215[2]) [[1]])
transitivity(rete1215_clean, type = "local", vids = top10_degree_1215[1])  #local clustering coefficient of node is 0.195, so 19.5% of neighbour nodes with distance 1 are linked

rete1619_clean_ego_degree <- induced.subgraph(rete1619_clean, neighborhood(rete1619_clean, order= 1, nodes= top10_degree_1619[7]) [[1]])
transitivity(rete1619_clean, type = "local", vids = top10_degree_1619[1])  #local clustering coefficient of most central is 0.025, so 2.5% of neighbour nodes with distance 1 are linked

#ego networks of nodes with highest degree centrality in 1215 and 1619
library(RColorBrewer)
pal <- brewer.pal(length(unique(V(rete1215_clean)$ssd)), "Set3") #create color palette
par(mfrow=c(1,2))
plot(rete1215_clean_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="24332388800 in 12/15", vertex.color = pal[as.numeric(as.factor(V(rete1215_clean)$ssd))])
plot(rete1619_clean_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="24332388800 in 16/19", vertex.color = pal[as.numeric(as.factor(V(rete1215_clean)$ssd))])
#legend("bottomleft", bty = "n", legend=levels(as.factor(V(rete1215_clean)$ssd)), fill=pal, border=NA)

#check the node with highest betweenness centrality
top10_betweenness_1215<-order(degree_centrality_rete1215_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_betweenness_1619<-order(degree_centrality_rete1619_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
#how to check if a name is present in both independently of positions? this cycle works but is ugly
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_betweenness_1215[i])==get.vertex.attribute(graph=rete1619_clean, name="name", index=top10_betweenness_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_clean, name="name", index=i)))
    }
  }  
}
#the same author has the highest degree centrality in both datasets id 6508086535
#the 4th author in 1215 is seventh in 1619 id 24332388800
#they are both the same
top10_betweenness_1215==top10_degree_1215
top10_betweenness_1619==top10_degree_1619



#2)Network decomposition
count_components(rete1215_clean)  #there are 185 components in the graph
comps1215 <- decompose.graph(rete1215_clean) #get the 185 components
table(sapply(comps1215, vcount))  #get the distributions of components size, no giant component 

count_components(rete1619_clean)  #there are 82 components in the graph
comps1619 <- decompose.graph(rete1619_clean) #get the 82 components
table(sapply(comps1619, vcount))  #get the distributions of components size, giant component of 7146 (out of 8085)
rete1619_clean_giant <-comps1619[[2]]


#other indexes

library(netseg) #to calculate the ei index


ei(rete1215_clean, vattr="ssd") #-0.43   #homophily
ei(rete1619_clean, vattr="ssd") #-0.51   #homophily

ei(rete1215_clean, vattr="h_fact") #0.35 #heterophily
ei(rete1619_clean, vattr="h_fact") #0.36

ei(rete1215_clean, vattr="academic_fact") #0.48  #heterophily
ei(rete1619_clean, vattr="academic_fact") #0.45



table(get.vertex.attribute(rete1619_clean, name ="academic_fact"))


