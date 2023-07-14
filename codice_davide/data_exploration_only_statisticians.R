library(sand) 

#

setwd("/home/davide/università/statistical analysis of networks/project")
rete1215_statistici<-readRDS("rete1215_clean_statistici_update.rds")
rete1619_statistici<-readRDS("final_rete1619_statistici")



#1) global network statistics


vcount(rete1215_statistici)    # the number of nodes is 562 (out of 4460)
ecount(rete1215_statistici)    # the number of edges 573 (out of 18631)
edge_density(rete1215_statistici)  # the density is 0.003, the network  is very sparse

vcount(rete1619_statistici)    # the number of nodes is 681 (out of 8123)
ecount(rete1619_statistici)    # the number of edges is 915 (out of 38367)
edge_density(rete1619_statistici)  # the density is 0.003, the network  is very sparse


#plot(rete1215_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
#plot(rete1619_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)


rete1215_statistici_deg <- degree(rete1215_statistici, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_statistici_deg)  #the mean degree is 2.03
max(rete1215_statistici_deg)   #the maximum degree is 16
min(rete1215_statistici_deg)   #the minimum degree is 0
sum(rete1215_statistici_deg==0) #get the number of isolated nodes 108
hist(rete1215_statistici_deg, col="lightblue", xlim=c(0, 6), xlab="degree", ylab="Frequency")


rete1619_statistici_deg <- degree(rete1619_statistici, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_statistici_deg)  #the mean degree is 2.68
max(rete1619_statistici_deg)   #the maximum degree is 20 
min(rete1619_statistici_deg)   #the minimum degree is 0 
sum(rete1619_statistici_deg==0) #get the number of isolated nodes 123
hist(rete1619_statistici_deg, col="lightblue", xlim=c(0, 20), xlab="degree", ylab="Frequency")


#check weigth distribution
weights1215_statistici<-E(rete1215_statistici)$weight
mean(weights1215_statistici)  #the mean weight is 2.38
max(weights1215_statistici)   #the maximum weight is 20
min(weights1215_statistici)   #the minimum weight is 1 
strength1215_statistici<-graph.strength(rete1215_statistici, vids=V(rete1215_statistici), weights = weights1215_statistici) #get the sum of all the weights of a single node
mean(strength1215_statistici)  #the mean strength is 4.86
max(strength1215_statistici)   #the maximum strength is 48 
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

mean_distance(rete1215_statistici)   # The average of all shortest paths 17.66

mean_distance(rete1619_statistici)   # The average of all shortest paths 9.70


transitivity(rete1215_statistici, type = "global")     #Global Transitivity (clustering coefficient) it is 0.37
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
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_statistici, name="name", index=top10_degree_1215_statistici[i])))
    }
  }  
}
#the 9t author in 1215 is 3rd in 1619 id 24467831700

get.vertex.attribute(graph=rete1215_statistici, index=top10_degree_1215_statistici[9]) 


# ego network considering node present in both top 10
rete1619_ego_degree_statistici <- induced.subgraph(rete1619_statistici, neighborhood(rete1619_statistici, order= 1, nodes= top10_degree_1619_statistici[3]) [[1]])
transitivity(rete1619_statistici, type = "local", vids = top10_degree_1619_statistici[3])  #local clustering is 0.17,

rete1215_ego_degree_statistici <- induced.subgraph(rete1215_statistici, neighborhood(rete1215_statistici, order= 1, nodes= top10_degree_1215_statistici[9]) [[1]])
transitivity(rete1215_statistici, type = "local", vids = top10_degree_1215_statistici[9])  #local clustering coefficient 0.66

#ego networks of nodes in both top10
library(RColorBrewer)
pal <- brewer.pal(length(unique(V(rete1215_ego_degree_statistici)$ssd)), "Set3") #create color palette
plot(rete1215_ego_degree_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, vertex.color = pal[as.numeric(as.factor(V(rete1215_clean_ego_degree)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215_ego_degree_statistici)$ssd)), fill=pal, border=NA)

pal <- brewer.pal(length(unique(V(rete1619_ego_degree_statistici)$ssd)), "Set3") #create color palette
plot(rete1619_ego_degree_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2,  vertex.color = pal[as.numeric(as.factor(V(rete1619_ego_degree_statistici)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_ego_degree_statistici)$ssd)), fill=pal, border=NA)


#check the node with highest betweenness centrality
top10_betweenness_1215_statistici<-order(degree_centrality_rete1215_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_betweenness_1619_statistici<-order(degree_centrality_rete1619_statistici, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

#they are both the same
top10_betweenness_1215_statistici==top10_degree_1215_statistici
top10_betweenness_1619_statistici==top10_degree_1619_statistici


count_components(rete1215_statistici)  #there are 165 components in the graph
comps1215_statistici <- decompose.graph(rete1215_statistici) #get the 165 components
table(sapply(comps1215_statistici, vcount))  #get the distributions of components size, giant component 281/562

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

ei(rete1215_statistici, vattr="ssd") #-0.61 homophily
ei(rete1619_statistici, vattr="ssd") #-0.44
#so statisticians tend to work with other statisticians belonging to same sector


#role only in statisticians data because for others the role is unknown
ei(rete1215_statistici, vattr="role") #0.54 heterophily
ei(rete1619_statistici, vattr="role") #0.42 heterophily

ei(rete1215_statistici)

list.vertex.attributes(rete1619_statistici)


ei(rete1215_statistici, vattr="h_fact") #0.20 #heterophily
ei(rete1619_statistici, vattr="h_fact") #0.27

table(get.vertex.attribute(rete1215_statistici, name ="h_fact"))
table(get.vertex.attribute(rete1619_statistici, name ="h_fact"))


ei(rete1215_statistici, vattr="academic_fact") #0.33 #heterophily
ei(rete1619_statistici, vattr="academic_fact") #0.44

table(get.vertex.attribute(rete1215_statistici, name ="academic_fact"))
table(get.vertex.attribute(rete1619_statistici, name ="academic_fact"))




