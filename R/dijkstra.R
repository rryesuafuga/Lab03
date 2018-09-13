dijkstra <- function (wiki_graph, init_node)
{
  #create vertex set Q
  a <- max(wiki_graph[,1])
  b <- max(wiki_graph[,2])
  Q <- c(1:max(a,b))
  
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
