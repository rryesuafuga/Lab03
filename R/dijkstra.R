dijkstra <- function (wiki_graph, init_node)
{
  #create vertex set Q
  a <- max(wiki_graph[,1])
  b <- max(wiki_graph[,2])
  Q <- c(1:max(a,b))
  stopifnot(init_node %in% Q)
  stopifnot(is.data.frame(wiki_graph)==TRUE)
  stopifnot(length(colnames(wiki_graph)) == 3)
  stopifnot(colnames(wiki_graph)  == c("v1","v2","w")) 
  
  
  #Initialization
  dist <- as.vector(matrix(Inf,nrow=length(Q)))                
  dist[init_node] <- 0                        
  while (length(Q) > 0)
  {
    u <- which.min(dist[Q])
    #h <- which(Q==u)
    p <- Q[u]
    Q <- Q[-u]
    edges <- which(wiki_graph[,1] == p)
    for (i in edges)
    {
      alt <- dist[p] + wiki_graph[i,3]  
      if (alt < dist[[wiki_graph[i,2]]])
      {
        dist[[wiki_graph[i,2]]] <- alt
      }
    }
  }
  return(dist)
}
