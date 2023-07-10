library(sand) 


setwd("/home/davide/università/statistical analysis of networks/project")
rete1215<-readRDS("final_rete1215")
rete1619<-readRDS("final_rete1619")
rete1215_statistici<-readRDS("final_rete1215_statistici")
rete1619_statistici<-readRDS("final_rete1619_statistici")


#1) global network statistics

is.directed(rete1215)     #False
is.connected(rete1215)    #False
is.weighted(rete1215)     #True

is.directed(rete1619)     #False
is.connected(rete1619)    #False
is.weighted(rete1619)     #True


vcount(rete1215)    # the number of nodes is 1715
ecount(rete1215)    # the number of edges 5822
edge_density(rete1215)  # the density is 0.003, the network  is very sparse

vcount(rete1619)    # the number of nodes is 8123
ecount(rete1619)    # the number of edges 38367
edge_density(rete1619)  # the density is 0.001, the network  is very sparse

#plot(rete1215, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
#plot(rete1619, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)


rete1215_deg <- degree(rete1215, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_deg)  #the mean degree is 6.78
max(rete1215_deg)   #the maximum degree is 58
min(rete1215_deg)   #the minimum degree is 0
hist(rete1215_deg, col="lightblue", xlim=c(0, 58), xlab="degree", ylab="Frequency")   

rete1619_deg <- degree(rete1619, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_deg)  #the mean degree is 9.44
max(rete1619_deg)   #the maximum degree is 486
min(rete1619_deg)   #the minimum degree is 1 
hist(rete1619_deg, col="lightblue", xlim=c(1, 486), xlab="degree", ylab="Frequency")

#check weigth distribution
weights1215<-E(rete1215)$weight
mean(weights1215)  #the mean weight is 1.13
max(weights1215)   #the maximum weight is 16 
min(weights1215)   #the minimum weight is 1 
strength1215<-graph.strength(rete1215, vids=V(rete1215), weights = weights1215) #get the sum of all the weights of a single node
mean(strength1215)  #the mean strength is 7.67
max(strength1215)   #the maximum strength is 70 
min(strength1215)   #the minimum strength is 0
#maybe a logharitmic scale would be better
par(mfrow=c(1,2))
hist(weights1215, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1215, col="lightblue", xlim=c(0, 70), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

weights1619<-E(rete1619)$weight
mean(weights1619)  #the mean weight is 1.27
max(weights1619)   #the maximum weight is 33 
min(weights1619)   #the minimum weight is 1 
strength1619<-graph.strength(rete1619, vids=V(rete1619), weights = weights1619) #get the sum of all the weights of a single node
mean(strength1619)  #the mean strength is 12.02
max(strength1619)   #the maximum strength is 858 
min(strength1619)   #the minimum strength is 1 
par(mfrow=c(1,2))
hist(weights1619, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1619, col="lightblue", xlim=c(1, 869), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

  
diameter(rete1215)  #the diameter (shortest longest distance) is 18
mean_distance(rete1215)   # The average of all shortest paths 3.18

diameter(rete1619)  #the diameter (shortest longest distance) is 28
mean_distance(rete1619)   # The average of all shortest paths 9.15


transitivity(rete1215, type = "global")     #Global Transitivity (clustering coefficient) it is 0.85
#transitivity(rete1215, type = "local")      # local transitivity for each node

transitivity(rete1619, type = "global")     #Global Transitivity (clustering coefficient) it is 0.50
#transitivity(rete1619, type = "local")      # local transitivity for each node

#centrality scores and ego networks of central nodes
degree_centrality_rete1215<-as.vector(degree(rete1215, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1215<-as.vector(betweenness(rete1215, normalized = T, weights = E(rete1215)$weight)) #compute the betweenness centrality of every node
#eigen_centrality_rete1215<-as.vector(eigen_centrality(rete1215, (rete1215)$weight))  #compute the degree centrality of every node

#centrality scores and ego networks of central nodes
degree_centrality_rete1619<-as.vector(degree(rete1619, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1619<-as.vector(betweenness(rete1619, normalized = T)) #compute the betweenness centrality of every node
#eigen_centrality_rete1619<-as.vector(eigen_centrality(rete1619, weights=(rete1619)$weight))  #compute the degree centrality of every node

#check the node with highest degree centrality
top10_degree_1215<-order(degree_centrality_rete1215, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_degree_1619<-order(degree_centrality_rete1619, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

#check if a name is present in both top10 independently of positions
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215, name="name", index=top10_degree_1215[i])==get.vertex.attribute(graph=rete1619, name="name", index=top10_degree_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215, name="name", index=i)))
    }
  }  
}
#the same author has the highest degree centrality in both datasets id 6508086535
#the 4th author in 1215 is seventh in 1619 id 24332388800

get.vertex.attribute(graph=rete1215, index=top10_degree_1619[1]) #get the info about most central node (not a statistician)

# ego network considering nodes which have the maximum degree centrality
rete1619_ego_degree <- induced.subgraph(rete1619, neighborhood(rete1619, order= 1, nodes= top10_degree_1619[1]) [[1]])
transitivity(rete1619, type = "local", vids = top10_degree_1619[1])  #local clustering coefficient of most central is 0.025, so 2.5% of neighbour nodes with distance 1 are linked

rete1215_ego_degree <- induced.subgraph(rete1215, neighborhood(rete1215, order= 1, nodes= top10_degree_1215[1]) [[1]])
transitivity(rete1215, type = "local", vids = top10_degree_1215[1])  #local clustering coefficient of node is 0.192, so 19.2% of neighbour nodes with distance 1 are linked

#ego networks of nodes with highest degree centrality in 1215 and 1619
library(RColorBrewer)
pal <- brewer.pal(length(unique(V(rete1215)$ssd)), "Set3") #create color palette
par(mfrow=c(1,2))
plot(rete1215_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="17434161200 in 12/15", vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])
plot(rete1619_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="17434161200 in 16/19", vertex.color = pal[as.numeric(as.factor(V(rete1215)$ssd))])
#legend("bottomleft", bty = "n", legend=levels(as.factor(V(rete1215)$ssd)), fill=pal, border=NA)


#check the node with highest betweenness centrality
top10_betweenness_1215<-order(degree_centrality_rete1215, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_betweenness_1619<-order(degree_centrality_rete1619, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
#how to check if a name is present in both independently of positions? this cycle works but is ugly
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215, name="name", index=top10_betweenness_1215[i])==get.vertex.attribute(graph=rete1619, name="name", index=top10_betweenness_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215, name="name", index=i)))
    }
  }  
}
#the same author has the highest degree centrality in both datasets id 6508086535
#the 4th author in 1215 is seventh in 1619 id 24332388800



#they are both the same
top10_betweenness_1215==top10_degree_1215
top10_betweenness_1619==top10_degree_1619



#2)Network decomposition
count_components(rete1215)  #there are 213 components in the graph
comps1215 <- decompose.graph(rete1215) #get the 213 components
table(sapply(comps1215, vcount))  #get the distributions of components size, no giant component 

count_components(rete1619)  #there are 84 components in the graph
comps1619 <- decompose.graph(rete1619) #get the 84 components
table(sapply(comps1619, vcount))  #get the distributions of components size, giant component of 7186 (out of 8129)
rete1619_giant <-comps1619[[2]]








#do same stuff considering only statisticians...

#1) global network statistics


vcount(rete1215_statistici)    # the number of nodes is 373 (out of 1715)
ecount(rete1215_statistici)    # the number of edges 215 (out of 5822)
edge_density(rete1215_statistici)  # the density is 0.003, the network  is very sparse

vcount(rete1619_statistici)    # the number of nodes is 681 (out of 8123)
ecount(rete1619_statistici)    # the number of edges is 915 (out of 38367)
edge_density(rete1619_statistici)  # the density is 0.003, the network  is very sparse


#plot(rete1215_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
#plot(rete1619_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)


rete1215_statistici_deg <- degree(rete1215_statistici, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_statistici_deg)  #the mean degree is 1.15
max(rete1215_statistici_deg)   #the maximum degree is 6
min(rete1215_statistici_deg)   #the minimum degree is 0
hist(rete1215_statistici_deg, col="lightblue", xlim=c(0, 6), xlab="degree", ylab="Frequency")


rete1619_statistici_deg <- degree(rete1619_statistici, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_statistici_deg)  #the mean degree is 2.68
max(rete1619_statistici_deg)   #the maximum degree is 20 
min(rete1619_statistici_deg)   #the minimum degree is 0 
hist(rete1619_statistici_deg, col="lightblue", xlim=c(1, 486), xlab="degree", ylab="Frequency")


#check weigth distribution
weights1215_statistici<-E(rete1215_statistici)$weight
mean(weights1215_statistici)  #the mean weight is 1.44
max(weights1215_statistici)   #the maximum weight is 16 
min(weights1215_statistici)   #the minimum weight is 1 
strength1215_statistici<-graph.strength(rete1215_statistici, vids=V(rete1215_statistici), weights = weights1215_statistici) #get the sum of all the weights of a single node
mean(strength1215_statistici)  #the mean strength is 1.66
max(strength1215_statistici)   #the maximum strength is 18 
min(strength1215_statistici)   #the minimum strength is 0
#maybe a logharitmic scale would be better
par(mfrow=c(1,2))
hist(weights1215_statistici, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1215_statistici, col="lightblue", xlim=c(0, 18), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

weights1619_statistici<-E(rete1619_statistici)$weight
mean(weights1619_statistici)  #the mean weight is 2.09
max(weights1619_statistici)   #the maximum weight is 21 
min(weights1619_statistici)   #the minimum weight is 1 
strength1619_statistici<-graph.strength(rete1619_statistici, vids=V(rete1619_statistici), weights = weights1619_statistici) #get the sum of all the weights of a single node
mean(strength1619_statistici)  #the mean strength is 5.63
max(strength1619_statistici)   #the maximum strength is 45 
min(strength1619_statistici)   #the minimum strength is 0 
par(mfrow=c(1,2))
hist(weights1619_statistici, col="lightblue", xlim=c(1, 21), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1619_statistici, col="lightblue", xlim=c(1, 45), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   
#in both cases the mean weight is higher but the strength is lower, so...

diameter(rete1215_statistici)  #the diameter (shortest longest distance) is 18
mean_distance(rete1215_statistici)   # The average of all shortest paths 4.14

diameter(rete1619_statistici)  #the diameter (shortest longest distance) is 28
mean_distance(rete1619_statistici)   # The average of all shortest paths 9.70


transitivity(rete1215_statistici, type = "global")     #Global Transitivity (clustering coefficient) it is 0.51
#transitivity(rete1215, type = "local")      # local transitivity for each node

transitivity(rete1619_statistici, type = "global")     #Global Transitivity (clustering coefficient) it is 0.36
#transitivity(rete1619, type = "local")      # local transitivity for each node


#centrality scores and ego networks of central nodes
degree_centrality_rete1215_statistici<-as.vector(degree(rete1215_statistici, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1215_statistici<-as.vector(betweenness(rete1215_statistici, normalized = T)) #compute the betweenness centrality of every node

#centrality scores and ego networks of central nodes
degree_centrality_rete1619_statistici<-as.vector(degree(rete1619_statistici, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1619_statistici<-as.vector(betweenness(rete1619_statistici, normalized = T)) #compute the betweenness centrality of every node

#check the node with highest degree centrality
top10_degree_1215_statistici<-order(degree_centrality_rete1215_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_degree_1619_statistici<-order(degree_centrality_rete1619_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
#how to check if a name is present in both independently of positions? this cycle works but is ugly
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215_statistici, name="name", index=top10_degree_1215_statistici[i])==get.vertex.attribute(graph=rete1619_statistici, name="name", index=top10_degree_1619_statistici[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_statistici, name="name", index=i)))
    }
  }  
}
#the 3rd author in 1619 is seventh in 1215 id 24467831700

get.vertex.attribute(graph=rete1215_statistici, index=top10_degree_1619_statistici[3]) 


# ego network considering node present in both top 10
rete1619_ego_degree_statistici <- induced.subgraph(rete1619_statistici, neighborhood(rete1619_statistici, order= 1, nodes= top10_degree_1619_statistici[3]) [[1]])
transitivity(rete1619_statistici, type = "local", vids = top10_degree_1619_statistici[3])  #local clustering is 0.17,

rete1215_ego_degree_statistici <- induced.subgraph(rete1215_statistici, neighborhood(rete1215_statistici, order= 1, nodes= top10_degree_1215_statistici[9]) [[1]])
transitivity(rete1215_statistici, type = "local", vids = top10_degree_1215_statistici[9])  #local clustering coefficient 0.66

#ego networks of nodes in both top10
par(mfrow=c(1,2))
plot(rete1215_ego_degree_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="24467831700 in 12/15")
plot(rete1619_ego_degree_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, main="24467831700 in 16/19")
#legend("bottomleft", bty = "n", legend=levels(as.factor(V(rete1215_statistici)$ssd)), fill=pal, border=NA)


#check the node with highest betweenness centrality
top10_betweenness_1215_statistici<-order(degree_centrality_rete1215_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_betweenness_1619_statistici<-order(degree_centrality_rete1619_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

#they are both the same
top10_betweenness_1215_statistici==top10_degree_1215_statistici
top10_betweenness_1619_statistici==top10_degree_1619_statistici


count_components(rete1215_statistici)  #there are 199 components in the graph
comps1215_statistici <- decompose.graph(rete1215_statistici) #get the 199 components
table(sapply(comps1215_statistici, vcount))  #get the distributions of components size, no giant component, 117 isolated out of(373) 

count_components(rete1619_statistici)  #there are 154 components in the graph
comps1619_statistici <- decompose.graph(rete1619_statistici) #get the 154 components
table(sapply(comps1619_statistici, vcount))  #get the distributions of components size, giant component of 463 (out of 681) and 123 isolated

#in the complete dataset there are no isolated nodes and in the statistician case there are many
#is it because there are way more others than statistician (and) or because statistician tend to collaborate less with each other?

#OTHER INDEXES
library(netseg) #to calculate the ei index
#?ei
#ei=-1 homophily
#ei=1 heterophily
#ei=0 balance

#to get members of groups
table(get.vertex.attribute(graph=rete1215, name="h_index")) 
#table(get.vertex.attribute(graph=rete1215_statistici, name="h_index")) #the highest h-index is for non statistician (upt to 58 vs up to 164)

ei(rete1215_statistici, vattr="ssd") #-0.61
ei(rete1619_statistici, vattr="ssd") #-0.44
#so statisticians tend to work with other statisticians belonging to same sector


ei(rete1215, vattr="ssd") #-0.55
ei(rete1619, vattr="ssd") #-0.51
#there are too many others to get info from these numbers? (#others>>stats so can't compare to result of only stat?)


ei(rete1215_statistici, vattr="role") #0.58 heterophily
ei(rete1619_statistici, vattr="role") #0.42 heterophily

ei(rete1215_statistici)


#this is because for non statistician there are a lot of elements with role: other
ei(rete1215, vattr="role") #-0.51 homophily    
ei(rete1619, vattr="role") #-0.49 homophily

#eterophily because there are not many statisticians witha a high h-index?
ei(rete1215_statistici, vattr="h_index_level") #-0.22
ei(rete1619_statistici, vattr="h_index_level") #-0.24

table(get.vertex.attribute(rete1215_statistici, name ="h_index_level"))
table(get.vertex.attribute(rete1619_statistici, name ="h_index_level"))


ei(rete1215, vattr="h_index_level") #0.15
ei(rete1619, vattr="h_index_level") #0.13


ei(rete1215_statistici, vattr="academic_range_level") #-0.20
ei(rete1619_statistici, vattr="academic_range_level") #-0.19

table(get.vertex.attribute(rete1215_statistici, name ="academic_range_level"))
table(get.vertex.attribute(rete1619_statistici, name ="academic_range_level"))


ei(rete1215, vattr="academic_range_level")  #0.22
ei(rete1619, vattr="academic_range_level")  #0.19
