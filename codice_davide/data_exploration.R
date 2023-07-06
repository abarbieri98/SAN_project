library(sand) 

setwd("/home/davide/università/statistical analysis of networks/project")
rete1215<-readRDS("rete1215.rds")
rete1619<-readRDS("rete1619.rds")


rete1215<-simplify(rete1215)
rete1619<-simplify(rete1619)

list.vertex.attributes(rete1215)
list.vertex.attributes(rete1619)

#1) global network statistics

is.directed(rete1215)     #False
is.connected(rete1215)    #False
is.weighted(rete1215)     #True

is.directed(rete1619)     #False
is.connected(rete1619)    #False
is.weighted(rete1619)     #True


vcount(rete1215)    # the number of nodes is 1722
ecount(rete1215)    # the number of edges 5881
edge_density(rete1215)  # the density is 0.003, the network  is very sparse

vcount(rete1619)    # the number of nodes is 8129
ecount(rete1619)    # the number of edges 38513
edge_density(rete1619)  # the density is 0.001, the network  is very sparse

plot(rete1215, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
plot(rete1619, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)


rete1215_deg <- degree(rete1215, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_deg)  #the mean degree is 6.83
max(rete1215_deg)   #the maximum degree is 59
min(rete1215_deg)   #the minimum degree is 1 


V(rete1215)$history
V(rete1215)$academic_range 

rete1619_deg <- degree(rete1619, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_deg)  #the mean degree is 9.47
max(rete1619_deg)   #the maximum degree is 487 (the one with degree 487 is an error? check ego network)
min(rete1619_deg)   #the minimum degree is 1 
which.max(rete1619_deg)  #node 103? has the highest degree  (id? 17434161200)

#check weigth distribution
weights1215<-E(rete1215)$weight
mean(weights1215)  #the mean weight is 1.13
max(weights1215)   #the maximum weight is 16 
min(weights1215)   #the minimum weight is 1 
strength1215<-graph.strength(rete1215, vids=V(rete1215), weights = weights1215) #get the sum of all the weights of a single node
mean(strength1215)  #the mean strength is 7.71
max(strength1215)   #the maximum strength is 71 
min(strength1215)   #the minimum strength is 1 
par(mfrow=c(1,2))
hist(weights1215, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1215, col="lightblue", xlim=c(1, 71), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

weights1619<-E(rete1619)$weight
mean(weights1619)  #the mean weight is 1.27
max(weights1619)   #the maximum weight is 33 
min(weights1619)   #the minimum weight is 1 
strength1619<-graph.strength(rete1619, vids=V(rete1619), weights = weights1619) #get the sum of all the weights of a single node
mean(strength1619)  #the mean strength is 12.09
max(strength1619)   #the maximum strength is 869 
min(strength1619)   #the minimum strength is 1 
par(mfrow=c(1,2))
hist(weights1619, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1619, col="lightblue", xlim=c(1, 869), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

  
diameter(rete1215)  #the diameter (shortest longest distance) is 18
mean_distance(rete1215)   # The average of all shortest paths 3.17

diameter(rete1619)  #the diameter (shortest longest distance) is 28
mean_distance(rete1619)   # The average of all shortest paths 9.15


transitivity(rete1215, type = "global")     #Global Transitivity (clustering coefficient) it is 0.85
#transitivity(rete1215, type = "local")      # local transitivity for each node

transitivity(rete1619, type = "global")     #Global Transitivity (clustering coefficient) it is 0.50
#transitivity(rete1619, type = "local")      # local transitivity for each node


#centrality scores and ego networks of central nodes
degree_centrality_rete1215<-as.vector(degree(rete1215, normalized = T))  #compute the degree centrality of every node
closeness_centrality_rete1215<-as.vector(closeness(rete1215, normalized = T)) #compute the closeness centrality of every node
betweenness_centrality_rete1215<-as.vector(betweenness(rete1215, normalized = T)) #compute the betweenness centrality of every node

which.max(degree_centrality_rete1215)  #node 56 has the highest degree centrality
which.max(closeness_centrality_rete1215)  #node 1 has the highest closeness centrality
which.max(betweenness_centrality_rete1215)  #node 56 has the highest betweenness centrality

#centrality scores and ego networks of central nodes
degree_centrality_rete1619<-as.vector(degree(rete1619, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1619<-as.vector(betweenness(rete1619, normalized = T)) #compute the betweenness centrality of every node

which.max(degree_centrality_rete1619)  #node 103 has the highest degree centrality
which.max(betweenness_centrality_rete1619)  #node 103 has the highest betweenness centrality

#they are the same id in both graphs
get.vertex.attribute(graph=rete1619, name="name", index=103)
get.vertex.attribute(graph=rete1215, name="name", index=56)

top10_degree_1215<-order(degree_centrality_rete1215, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_degree_1619<-order(degree_centrality_rete1619, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

top10_degree_1215_name<-get.vertex.attribute(graph=rete1215, name="name", index=top10_degree_1215) #get name of 10 highest degree centrality
top10_degree_1619_name<-get.vertex.attribute(graph=rete1619, name="name", index=top10_degree_1619) #get name of 10 highest degree centrality


# ego network considering node 7 which has the maximum degree centrality
rete1619_ego_degree <- induced.subgraph(rete1619, neighborhood(rete1619, order= 1, nodes= 103) [[1]])
transitivity(rete1619, type = "local", vids = 103)  #local clustering coefficient of node 103 is 0.025, so 19.4% of neighbour nodes with distance 1 are linked

rete1215_ego_degree <- induced.subgraph(rete1215, neighborhood(rete1215, order= 1, nodes= 56) [[1]])
transitivity(rete1215, type = "local", vids = 56)  #local clustering coefficient of node 56 is 0.194, so 19.4% of neighbour nodes with distance 1 are linked

#ego networks of nodes with highest degree centrality in 1215 and 1619
par(mfrow=c(1,2))
plot(rete1215_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="17434161200 in 12/15")
plot(rete1619_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="17434161200 in 16/19")


#2)Network decomposition
count_components(rete1215)  #there are 212 components in the graph
comps1215 <- decompose.graph(rete1215) #get the 212 components
table(sapply(comps1215, vcount))  #get the distributions of components size, no giant component 

count_components(rete1619)  #there are 83 components in the graph
comps1619 <- decompose.graph(rete1619) #get the 83 components
table(sapply(comps1619, vcount))  #get the distributions of components size, giant component of 7186 (out of 8129)
rete1619_giant <-comps1619[[2]]

