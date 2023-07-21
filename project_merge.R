library(sand) 
library(igraph)
library(intergraph) # needed to convert networks to igraph graphs
library(ergm)
library(ggplot2)

####### PREPROCESSING PIPELINE ########

data <- readRDS("rete1215.rds")
data1619<-readRDS("rete1619.rds")


# Drop nodes containing NAs
attributes<- igraph::list.vertex.attributes(data1619)
for(i in 3:9){
  to_remove <- which(is.na(igraph::get.vertex.attribute(data,attributes[i])))
  to_remove1619 <- which(is.na(igraph::get.vertex.attribute(data1619,attributes[i])))
  if(length(to_remove)+length(to_remove1619) != 0){
    data<-igraph::delete.vertices(data,to_remove)
    data1619<-igraph::delete.vertices(data1619,to_remove1619)
  }
}


# Add the "people" attribute (needed for aggregation later)
data<-set_vertex_attr(data, "people", value = 1)
data1619<-set_vertex_attr(data1619, "people", value = 1)

# Remove researchers without direct stat connections
n<- vcount(data)
nodes<-V(data)
to_delete<-list()
for(i in 1:n){
  if(nodes[i]$ssd == "OTHER"){
    count<-0
    neigh<-adjacent_vertices(data,i)[[1]]
    for (j in 1:length(neigh)){
      if(neigh[j]$ssd == "OTHER"){
        
        count<-count+1
      }
    }
    if(count==length(neigh)){
      
      to_delete<-append(to_delete,i,length(to_delete))
    }
  }
}

data<-igraph::delete.vertices(data,to_delete)
# 291 nodes dropped

n<- vcount(data1619)
nodes<-V(data1619)
to_delete<-list()
for(i in 1:n){
  if(nodes[i]$ssd == "OTHER"){
    count<-0
    neigh<-adjacent_vertices(data1619,i)[[1]]
    for (j in 1:length(neigh)){
      if(neigh[j]$ssd == "OTHER"){
        
        count<-count+1
      }
    }
    if(count==length(neigh)){
      
      to_delete<-append(to_delete,i,length(to_delete))
    }
  }
}
data1619<-igraph::delete.vertices(data1619,to_delete)
# 13 nodes dropped

# Add categorical value for academic range
n <-vcount(data)
quantile(V(data)$academic_range,c(.25,.5,.75))
quantile_factors <- rep(NA,n)

for(i in 1:n){
  curr<- V(data)[i]$academic_range
  if(curr <= 15){
    # first quartile
    quantile_factors[i]<- 1
  }
  else if (curr > 15 && curr <= 21) {
    # second quartile (median)
    quantile_factors[i]<- 2
    
  }
  else if(curr > 21 && curr <= 31){
    # third quartile
    quantile_factors[i] <- 3
  }
  else{
    #  fourth quartile
    quantile_factors[i]<- 4
  }
}

V(data)$academic_fact <- quantile_factors


n <-vcount(data1619)
quantile(V(data1619)$academic_range,c(.25,.5,.75))
quantile_factors <- rep(NA,n)

for(i in 1:n){
  curr<- V(data1619)[i]$academic_range
  if(curr <= 10){
    # first quartile
    quantile_factors[i]<- 1
  }
  else if (curr > 10 && curr <= 17) {
    # second quartile (median)
    quantile_factors[i]<- 2
    
  }
  else if(curr > 17 && curr <= 27){
    # third quartile
    quantile_factors[i] <- 3
  }
  else{
    #  fourth quartile
    quantile_factors[i]<- 4
  }
}

V(data1619)$academic_fact <- quantile_factors


# Add categorical value for h index
n <-vcount(data)
quantile(V(data)$h_index,c(.25,.5,.75))
quantile_factors <- rep(NA,n)

for(i in 1:n){
  curr<- V(data)[i]$h_index
  if(curr <= 7){
    # first quartile
    quantile_factors[i]<- 1
  }
  else if (curr > 7 && curr <= 13) {
    # second quartile (median)
    quantile_factors[i]<- 2
    
  }
  else if(curr > 13 && curr <= 24){
    # third quartile
    quantile_factors[i] <- 3
  }
  else{
    #  fourth quartile
    quantile_factors[i]<- 4
  }
}

V(data)$h_fact <- quantile_factors

n <-vcount(data1619)
quantile(V(data1619)$h_index,c(.25,.5,.75))
quantile_factors <- rep(NA,n)

for(i in 1:n){
  curr<- V(data1619)[i]$h_index
  if(curr <= 6){
    # first quartile
    quantile_factors[i]<- 1
  }
  else if (curr > 6 && curr <= 12) {
    # second quartile (median)
    quantile_factors[i]<- 2
    
  }
  else if(curr > 12 && curr <= 24){
    # third quartile
    quantile_factors[i] <- 3
  }
  else{
    #  fourth quartile
    quantile_factors[i]<- 4
  }
}


V(data1619)$h_fact <- quantile_factors

# Drop attributes not needed

data<-delete_vertex_attr(data,"history")
data<-delete_vertex_attr(data,"affiliation_id")
data<-delete_vertex_attr(data,"department/unversity")

data1619<-delete_vertex_attr(data1619,"history")
data1619<-delete_vertex_attr(data1619,"affiliation_id")
data1619<-delete_vertex_attr(data1619,"department/unversity")

# Save the nets

saveRDS(data,"rete1215_clean.rds")
saveRDS(data1619,"rete1619_clean.rds")

rm(list=ls())

########## FULL NETWORKS ANALYSIS #############
rete1215_clean<-readRDS("rete1215_clean.rds")
rete1619_clean<-readRDS("rete1619_clean.rds")

#in the cleaned data researchers without a direct connection to the statisticians are removed
#distribution of ssd
table(get.vertex.attribute(rete1215_clean, name = "ssd")) 
table(get.vertex.attribute(rete1619_clean, name = "ssd"))

vcount(rete1215_clean)    # the number of nodes is 4460
ecount(rete1215_clean)    # the number of edges 18631
edge_density(rete1215_clean)  # the density is 0.001, the network  is very sparse

vcount(rete1619_clean)    # the number of nodes is 8085
ecount(rete1619_clean)    # the number of edges 38173
edge_density(rete1619_clean)  # the density is 0.001, the network  is very sparse

#plot(rete1215, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)
#plot(rete1619, vertex.size=8, vertex.label="", edge.width=0.8, edge.arrow.size=0.2)

rete1215_clean_deg <- degree(rete1215_clean, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1215_clean_deg)  #the mean degree is 8.35
max(rete1215_clean_deg)   #the maximum degree is 94
min(rete1215_clean_deg)   #the minimum degree is 0
#jpeg("deg_hist_1215.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
hist(rete1215_clean_deg, col="lightblue", xlim=c(0, 94), xlab="degree", ylab="Frequency", main="")   
dev.off()
sum(rete1215_clean_deg==0) #get the number of isolated nodes (just one)

  
rete1619_clean_deg <- degree(rete1619_clean, mode="all")  #only total degree is considered since the graph is undirected
mean(rete1619_clean_deg)  #the mean degree is 9.44
max(rete1619_clean_deg)   #the maximum degree is 483
min(rete1619_clean_deg)   #the minimum degree is 1 
#jpeg("deg_hist_1619.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
hist(rete1619_clean_deg, breaks = 60, col="lightblue", xlim=c(1, 483), xlab="degree", ylab="Frequency", main="")  #very skewed
dev.off()


#check weigth distribution
weights1215_clean<-E(rete1215_clean)$weight
mean(weights1215_clean)  #the mean weight is 1.30
max(weights1215_clean)   #the maximum weight is 29 
min(weights1215_clean)   #the minimum weight is 1 
strength1215_clean<-graph.strength(rete1215_clean, vids=V(rete1215_clean), weights = weights1215_clean) #get the sum of all the weights of a single node
mean(strength1215_clean)  #the mean strength is 10.93
max(strength1215_clean)   #the maximum strength is 157 
min(strength1215_clean)   #the minimum strength is 0

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


mean_distance(rete1215_clean)   # The average of all shortest paths 11.67
mean_distance(rete1619_clean)   # The average of all shortest paths 9.16

transitivity(rete1215_clean, type = "global")     #Global Transitivity (clustering coefficient) it is 0.66
#transitivity(rete1215, type = "local")      # local transitivity for each node

transitivity(rete1619_clean, type = "global")     #Global Transitivity (clustering coefficient) it is 0.50
#transitivity(rete1619, type = "local")      # local transitivity for each node


#centrality scores and ego networks of central nodes
degree_centrality_rete1215_clean<-as.vector(degree(rete1215_clean, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1215_clean<-as.vector(betweenness(rete1215_clean, normalized = T, weights = E(rete1215_clean)$weight)) #compute the betweenness centrality of every node

#centrality scores and ego networks of central nodes
degree_centrality_rete1619_clean<-as.vector(degree(rete1619_clean, normalized = T))  #compute the degree centrality of every node
betweenness_centrality_rete1619_clean<-as.vector(betweenness(rete1619_clean, normalized = T)) #compute the betweenness centrality of every node

#check the node with highest degree centrality
top10_degree_1215<-order(degree_centrality_rete1215_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_degree_1619<-order(degree_centrality_rete1619_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality

#get the ssd of the nodes in top ten
table(get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215, name = "ssd"))
table(get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619, name = "ssd")) 


#check if a name is present in both top10 independently of positions
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_degree_1215[i])==get.vertex.attribute(graph=rete1619_clean, name="name", index=top10_degree_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_degree_1215[i])))
    }
  }  
}
#the 3rd author in 1215 is seventh in 1619 id 6602790290
#the 5th author in 1215 is sixth in 1619 id 24921727400

get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215[3]) #get the info about node in both top ten
get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619[7]) #get the info about node in both top ten

get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215[5]) #get the info about node in both top ten
get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619[6]) #get the info about node in both top ten



get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215[1]) #get the info about most central node
get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619[1]) #get the info about most central node

mean(get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215, name = "h_index")) #24.4 
mean(get.vertex.attribute(graph=rete1215_clean, index=top10_degree_1215, name = "academic_range")) #27.6

mean(get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619, name = "h_index")) #31.1 
mean(get.vertex.attribute(graph=rete1619_clean, index=top10_degree_1619, name = "academic_range")) #21.7

# ego network considering node present in both top 10
rete1215_clean_ego_degree <- induced.subgraph(rete1215_clean, neighborhood(rete1215_clean, order= 1, nodes= top10_degree_1215[3]) [[1]])
transitivity(rete1215_clean, type = "local", vids = top10_degree_1215[2])  #local clustering coefficient of node is 0.09, so 35% of neighbour nodes with distance 1 are linked

rete1619_clean_ego_degree <- induced.subgraph(rete1619_clean, neighborhood(rete1619_clean, order= 1, nodes= top10_degree_1619[7]) [[1]])
transitivity(rete1619_clean, type = "local", vids = top10_degree_1619[7])  #local clustering coefficient of most central is 0.09, so 9% of neighbour nodes with distance 1 are linked

#ego networks of node present in both top10
library(RColorBrewer)
#jpeg("ego_1215.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1215_clean_ego_degree)$ssd)), "Set3") #create color palette
plot(rete1215_clean_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, vertex.color = pal[as.numeric(as.factor(V(rete1215_clean_ego_degree)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1215_clean_ego_degree)$ssd)), fill=pal, border=NA)
dev.off()

#jpeg("ego_1619.jpeg", width = 2000, height = 1800, res = 200, quality = 100, pointsize = 20)
pal <- brewer.pal(length(unique(V(rete1619_clean_ego_degree)$ssd)), "Set3") #create color palette
plot(rete1619_clean_ego_degree, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, vertex.color = pal[as.numeric(as.factor(V(rete1619_clean_ego_degree)$ssd))])
legend("topleft", bty = "n", legend=levels(as.factor(V(rete1619_clean_ego_degree)$ssd)), fill=pal, border=NA)
dev.off()

get.vertex.attribute(rete1215_clean_ego_degree, index = which(V(rete1215_clean_ego_degree)$ssd != "OTHER"))
get.vertex.attribute(rete1619_clean_ego_degree, index = which(V(rete1619_clean_ego_degree)$ssd != "OTHER"))


#check the node with highest betweenness centrality
top10_betweenness_1215<-order(degree_centrality_rete1215_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
top10_betweenness_1619<-order(degree_centrality_rete1619_clean, decreasing=TRUE)[1:10]  #get 10 highest degree centrality
#how to check if a name is present in both independently of positions? this cycle works but is ugly
for (i in 1:10){
  for (j in 1:10){
    if (get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_betweenness_1215[i])==get.vertex.attribute(graph=rete1619_clean, name="name", index=top10_betweenness_1619[j])){
      print(paste0(i, "=", j, " id ", get.vertex.attribute(graph=rete1215_clean, name="name", index=top10_betweenness_1215[i])))
    }
  }  
}

#they are both the same
top10_betweenness_1215==top10_degree_1215
top10_betweenness_1619==top10_degree_1619



#2)Network decomposition
count_components(rete1215_clean)  #there are 88 components in the graph
comps1215 <- decompose.graph(rete1215_clean) #get the 88 components
table(sapply(comps1215, vcount))  #get the distributions of components size, giant component of 3604 (out of 4460)  

count_components(rete1619_clean)  #there are 82 components in the graph
comps1619 <- decompose.graph(rete1619_clean) #get the 82 components
table(sapply(comps1619, vcount))  #get the distributions of components size, giant component of 7146 (out of 8085)
rete1619_clean_giant <-comps1619[[2]]

table(get.vertex.attribute(rete1619_clean_giant, name="ssd"))
table(get.vertex.attribute(rete1619_clean, name="ssd"))

#other indexes

library(netseg) #to calculate the ei index


ei(rete1215_clean, vattr="ssd") #-0.46   #homophily
ei(rete1619_clean, vattr="ssd") #-0.51   #homophily

ei(rete1215_clean, vattr="h_fact") #0.36 #heterophily
ei(rete1619_clean, vattr="h_fact") #0.36

ei(rete1215_clean, vattr="academic_fact") #0.44  #heterophily
ei(rete1619_clean, vattr="academic_fact") #0.45

############## STATISTICIANS COLLABORATION ##################
#1) global network statistics

rete1215_statistici<-delete.vertices(rete1215_clean, which(V(rete1215_clean)$ssd=="OTHER")) #to delete non statisticians
rete1619_statistici<-delete.vertices(rete1619_clean, which(V(rete1619_clean)$ssd=="OTHER"))

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
hist(rete1215_statistici_deg, col="lightblue", xlim=c(0, 16), xlab="degree", ylab="Frequency")


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
par(mfrow=c(1,2))
hist(weights1215_statistici, col="lightblue", xlim=c(1, 16), xlab="edge weight", ylab="Frequency", main="edge weight distribution")   
hist(strength1215_statistici, col="lightblue", xlim=c(0, 48), xlab="vertex strength", ylab="Frequency", main="vertex strength distribution")   

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
#the 6th author in 1215 is 3rd in 1619 id 12645849000

#get info about him
get.vertex.attribute(graph=rete1215_statistici, index=top10_degree_1215_statistici[6]) 

rete1215_ego_degree_statistici <- induced.subgraph(rete1215_statistici, neighborhood(rete1215_statistici, order= 1, nodes= top10_degree_1215_statistici[6]) [[1]])
transitivity(rete1215_statistici, type = "local", vids = top10_degree_1215_statistici[6])  #local clustering coefficient 0.66

# ego network considering node present in both top 10
rete1619_ego_degree_statistici <- induced.subgraph(rete1619_statistici, neighborhood(rete1619_statistici, order= 1, nodes= top10_degree_1619_statistici[3]) [[1]])
transitivity(rete1619_statistici, type = "local", vids = top10_degree_1619_statistici[3])  #local clustering is 0.17,

#ego networks of nodes in both top10
library(RColorBrewer)
pal <- brewer.pal(length(unique(V(rete1215_ego_degree_statistici)$ssd)), "Set3") #create color palette
plot(rete1215_ego_degree_statistici, vertex.size=4, vertex.label="", edge.width=0.8, edge.arrow.size=0.2, vertex.color = pal[as.numeric(as.factor(V(rete1215_ego_degree_statistici)$ssd))])
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

rete1619_statistici_giant<-comps1619_statistici[[4]]
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

ei(rete1215_statistici, vattr="ssd") #-0.63 homophily
ei(rete1619_statistici, vattr="ssd") #-0.44
#so statisticians tend to work with other statisticians belonging to same sector



ei(rete1215_statistici, vattr="h_fact") #0.20 #heterophily
ei(rete1619_statistici, vattr="h_fact") #0.27

table(get.vertex.attribute(rete1215_statistici, name ="h_fact"))
table(get.vertex.attribute(rete1619_statistici, name ="h_fact"))


ei(rete1215_statistici, vattr="academic_fact") #0.32 #heterophily
ei(rete1619_statistici, vattr="academic_fact") #0.34

ei(rete1215_statistici, vattr="role") #0.54 #heterophily
ei(rete1619_statistici, vattr="role") #0.42

table(get.vertex.attribute(rete1215_statistici, name="role"))

#to measure ei based on pair of attributes instead of all
#which(V(rete1215_statistici)$role=="Associate" | V(rete1215_statistici)$role=="Full professor")  #to select only the nodes with specific attribute
ei(induced.subgraph(rete1215_statistici, vids=which(V(rete1215_statistici)$role=="Associate" | V(rete1215_statistici)$role=="Full professor")), vattr="role")                       

table(get.vertex.attribute(rete1619_statistici, name="role"))
ei(induced.subgraph(rete1619_statistici, vids=which(V(rete1619_statistici)$role=="Associate" | V(rete1619_statistici)$role=="Full professor")), vattr="role")                       
#ei(induced.subgraph(rete1215_statistici, vids=which(V(rete1215_statistici)$ssd=="SECS-S/01" | V(rete1215_statistici)$ssd=="SECS-S/03")), vattr="ssd")                       

############# SSD INTERACTIONS ###############
#2012-2015

# We collapse all the statisticians in nodes representing their SSDs,
# summing all the connections to get a single edge per pair of nodes.
SSD <- contract(rete1215_clean, as.numeric(as.factor(V(data)$ssd)),list(ssd="random", "ignore", weight = "sum", people = "sum"))

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

#2016-2019
SSD1619 <- contract(rete1619_clean, as.numeric(as.factor(V(rete1619_clean)$ssd)),list(ssd="random", "ignore", weight = "sum", people = "sum"))

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

rm(list=ls())


########### COMMUNITY DETECTION ###############

rete1619_clean<-readRDS("rete1619_clean.rds")
rete1619_clean<-simplify(rete1619_clean)

igraph::list.vertex.attributes(rete1619_clean)
igraph::list.edge.attributes(rete1619_clean)

summary(rete1619_clean)  #8085 nodes and 38173 edges

sum(V(rete1619_clean)$ssd=='OTHER') # 7404 nodes are OTHER 
8098-sum(V(rete1619_clean)$ssd=='OTHER') #694 nodes are Statisticians (8.4%)

###### KEEP ONLY STATISTICIANS 

rete1619_stat<-igraph::delete.vertices(rete1619_clean, V(rete1619_clean)[V(rete1619_clean)$ssd=='OTHER'])

igraph.options(vertex.size=3, vertex.label=NA, vertex.color="orange", edge.size=15, edge.color="grey", edge.arrow.size=0.2)

plot(rete1619_stat, layout=layout_with_kk) 

count_components(rete1619_stat)
comps1619_stat <- decompose.graph(rete1619_stat)
table(sapply(comps1619_stat, vcount))
rete1619_giant_stat<-comps1619_stat[[4]]

plot(rete1619_giant_stat, layout=layout_with_fr) 

is_connected(rete1619_giant_stat)


###### PLOT 

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


###### LOUVAIN METHOD 
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

rm(list=ls())
########### ERGM ###############

###### 2012-2015 ######

# Import dataset, filter out non statisticians and convert the object to statnet graph
data1215<-readRDS("rete1215_clean.rds")
statIdx <-which(V(data1215)$ssd != "OTHER")
stat <-induced.subgraph(data1215,statIdx)
rm(data1215)
comp<-decompose.graph(stat)
sapply(comp, vcount)
gi_comp <- comp[[3]] # We isolate here the giant component
gi_comp<-delete_edge_attr(gi_comp,"weight") # Remove weights from the edges and keep raw connections
data <- asNetwork(gi_comp)

# Base Model
base_model <- ergm(data~edges)
summary(base_model)

# Best model, according to BIC and significance of the coefficients
model_best<- ergm(data ~edges + nodematch("ssd", diff=T)+nodecov("h_index"))
summary(model_best)
best_gof <- gof(model_best, control = control.gof.ergm(nsim = 100))
par(mar=rep(4,4))
plot(best_gof)

###### 2016-2019 ######

# Import dataset, filter out non statisticians and convert the object to statnet graph
data1619<-readRDS("rete1619_clean.rds")
statIdx <-which(V(data1619)$ssd != "OTHER")
stat <-induced.subgraph(data1619,statIdx)
rm(data1619)
comp<-decompose.graph(stat)
sapply(comp, vcount)

gi_comp <- comp[[4]] # We isolate here the giant component
gi_comp<-delete_edge_attr(gi_comp,"weight")
data <- asNetwork(gi_comp)

# Base Model
model <- ergm(data~edges)
summary(model)

# Best model
model_best<- ergm(data ~edges + nodemix("ssd", levels=c(1,3,4,5))+nodemix("h_fact", levels=c(2,3,4)))
summary(model_best)
gof_best <- gof(model_best, control = control.gof.ergm(nsim = 100))
par(mar=rep(4,4))
plot(gof_best)
