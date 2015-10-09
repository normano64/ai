smartWC=function(moveInfo,readings,positions,edges,probs) {
  rangerPos = positions[3]
  currentBelif = get.observations(readings, probs, positions)
#   if(length(moveInfo$mem) == 0){
#     moveInfo$mem[[1]] = currentBelif
#   } else {
#     pastBelif = moveInfo$mem[[1]]
#     currentBelif = normalize(currentBelif * pastBelif)
#     moveInfo$mem[[1]] = currentBelif
#   }
  crocPos = which.max(currentBelif)
  path = shortest.path(rangerPos, crocPos, edges)
  if(length(path)==1){moveInfo$moves=c(0,0)}
  else if(length(path)==2){moveInfo$moves=c(tail(path,n=1),0)}
  else{moveInfo$moves=path[1:2]}
  return(moveInfo)
}

shortest.path = function(start, goal, edges){
  if(start == goal){
    return(start)
  }else if(any(apply(edges, 1, function(x, y) isTRUE(all.equal(x, y)), c(start,goal)))|
             any(apply(edges, 1, function(x, y) isTRUE(all.equal(x, y)), c(goal,start)))){
    return(c(start,goal))
  }else {
    temp.edges = c()
    for(i in 1:nrow(edges)){temp.edges=c(temp.edges, edges[i,1:2])}
    g = make_empty_graph(n = 40, directed = FALSE) %>% add_edges(temp.edges)
    return(get.all.shortest.paths(g, start, goal)$res[[1]])
  }
}

## Returns the probability distribution
get.observations = function(reads, probs, positions){
  # Readings 
  salinity  = dnorm(reads[1], mean=probs$salinity[1:40,1], sd=probs$salinity[1:40,2])
  phosphate = dnorm(reads[2], mean=probs$phosphate[1:40,1], sd=probs$phosphate[1:40,2])
  nitrogen  = dnorm(reads[3], mean=probs$nitrogen[1:40,1], sd=probs$nitrogen[1:40,2])
  unNormalized = salinity + phosphate + nitrogen
  return(normalize(unNormalized)) 
}

## Simple normalizing function
normalize = function(x){
  return(x/sum(x))
}

test.run <- replicate(500,runWheresCroc(smartWC, plot=FALSE, pause=0))
cumulative.average = cumsum(test.run) / seq_along(test.run)
qplot(1:500, cumulative.average, geom=c("line","smooth"), ylab="Turns, Cumulatice Average", 
      xlab="Test runs", main="Performance, only current obesrvations (smartWC)")
tail(cumulative.average)
