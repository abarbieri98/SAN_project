library(igraph)
library(intergraph) # needed to convert networks to igraph graphs
library(ergm)

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
