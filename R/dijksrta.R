#graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
   #                 v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
   #                w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

#'  Dijkstras algorithm
#'
#' @param graph a data frame including with three variables (v1, v2 and w) 
#' that contains the edges of the graph (from 
#' v1 to v2) with the weight of the edge (w)
#' @param init_node start point
#'
#' @return calculates the shortest path from the initial node to every other node in the graph
#' @export  
#'
#' @examples wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)

# @source \url{https://en.wikipedia.org/wiki/Dijkstra's algorithm}
#' 
  dijkstra <- function(graph, init_node){
  stopifnot(length(init_node) == 1, is.numeric(init_node) == TRUE, is.data.frame(graph) == TRUE, length(graph[1,]) == 3, names(graph) == c("v1", "v2", "w"), is.numeric(graph[,1]), is.numeric(graph[,2]), is.numeric(graph[,3]), init_node %in% unique(graph$v1))
  Q <- c()
  dist <- c()
  prev <- c()
  
  for (i in 1:length(unique(graph[[1]]))){
    dist[i] <- Inf
    prev[i] <- NA
    Q[i] <- unique(graph[[1]])[i]
  }
  
  dist[init_node] <- 0 # if(init_node==i) dist[i]<-0
  
  #Init node closest neighbor
  while (length(Q) > 1){
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q!=u]
    for(i in 1:sum(graph[graph$v1 == u,]$v2 %in% Q)){
      alt <- dist[u] + graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$w[i]
      if (alt < dist[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]]){
        dist[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]] <- alt
        prev[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]] <- u
      }
    }
  }
  return (dist)
}



