# LAB PROJECT 2: SAMPSON'S MONKS 
# NAME: Berge Sievers
# Part 0 - Setting Up
install.packages("lda")
install.packages("igraph")
install.packages("networkD3")
library(lda)
library(igraph)
library(networkD3)
## Having loaded all the required libraries, we now need to load the data from sampson as follows;
data("sampson")
#we chek and observed the data
print(names(sampson))
## Part 1 - Visualizing the Network
# Forming the desired matrix that would neccessitate running dataframes of nodes and edges as required by the networkD3
part1<-setdiff(names(sampson),"SAMPLK1")[1]
monk<-sampson[[part1]]
cat("using sociomatrix:", part1,"\n")
monk<-as.matrix(monk)
## We plot the graph using the Igraph
static_plot<-graph_from_adjacency_matrix(monk, mode = "Directed",weighted = NULL)
plot.igraph(static_plot, vertex.size = 8, vertex.label.cex  = 0.8, vertex.label.color = "black",
            vertex.color = "green",edge.arrow.size = 0.5,
            main = paste("Network plot of", part1))
## this code shows the preparation of interactive plot 
edges<-which(monk>0, arr.ind = TRUE)
links<-data.frame(source = edges[,1] -1,target = edges[,2] -1,value = monk[edges])
nodes<-data.frame(name=paste("monk",1:nrow(monk)), group =1)

networkD3::forceNetwork(Links = links,Nodes = nodes,Source = "source", Target = "target",NodeID = "name",Value = "value",
             Group = "group",opacity = 0.9, zoom = TRUE)

## PART 2 - Summary Statistics on a Sociomatrix
degree_out<-rowSums(monk>0)
degree_in<-colSums(monk>0)
## we create the averages and name them average_
average_<-mean(monk[monk>0])
## we ceate summary statistics in a combined table
summary_statistics<-list(degree_out= degree_out, degree_in=degree_in, average_=average_)
print(summary_statistics)
# we create a bargraph to see the visual represetation of degree in
barplot(degree_in, main = "Number of likes received in in-degree", xlab = "monk", ylab = "degree in",
        names.arg = 1:length(degree_in), col = "maroon")
barplot(degree_in, main = "Number of likes received in out-degree", 
        xlab ="monk", ylab= "degree out", 
        names.arg = 1:length(degree_out), col = "green")
## least and most liked
mostliked<-which.max(degree_in)
leastliked<-which.min(degree_in)
mostliked
leastliked
## PART 3 - simple model of a social network
set.seed(123)
n<-nrow(net)
net<-network_generated(n = 18, prob_like = 0.3, leastliked=3)
n<-nrow(net)
network_generated<-function(n, prob_like = 0.3, leastliked = 4){
  net<-matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      if(i!=j){
        if(runif(1)<prob_like){
          net[i,j]<-sample(1:leastliked, 1)
        }
      }
    }
  }
  rownames(net)<-paste0("monk", 1:n)
  colnames(net)<-paste0("monk", 1:n)
  return(net)
}
print(net[1:18, 1:18]) ## this allows us to review the first 18

## Part 4 - Comparison: Model vs Observations
part4<-graph_from_adjacency_matrix(net, mode = "Directed",weighted = TRUE)
plot.igraph(part4, vertex.size = 8, vertex.label.cex  = 0.8, vertex.label.color = "black",
            vertex.color = "lightblue",edge.arrow.size = 0.5,
            main = "Random Network")
### interactive chart
edges2<-which(net>0, arr.ind = TRUE)
links2<-data.frame(source = edges2[,1] -1,target = edges2[,2] -1,value = net[edges2])
nodes2<-data.frame(name = paste("net",1:nrow(net)), group =1)

networkD3::forceNetwork(Links = links2,Nodes = nodes2,Source = "source", Target = "target",NodeID = "name",Value = "value",
                        Group = "group",opacity = 0.9, zoom = TRUE)
#
degree_out2<-rowSums(net>0)
degree_in2<-colSums(net>0)

average_2<-mean(net[net>0])
summary_statistics2<-list(degree_out2= degree_out2, degree_in2=degree_in2, average_2=average_2)
print(summary_statistics2)
barplot(degree_in2, main = "Number of likes received in in-degree 2", xlab = "generated network", ylab = "degree in",
        names.arg = 1:length(degree_in2), col = "orange")
## least and most liked
mostliked2<-which.max(degree_in2)
leastliked2<-which.min(degree_in2)
mostliked2

leastliked2
