library(igraph)
library(intergraph) # needed to convert networks to igraph graphs
library(ergm)

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
attr_gof <- gof(model_best, control = control.gof.ergm(nsim = 100))
par(mar=rep(4,4))
plot(attr_gof)