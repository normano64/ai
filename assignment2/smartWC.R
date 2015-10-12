library("DeliveryMan")
library("ggplot2")
library("igraph")

smartWC = function(moveInfo, readings, positions, edges, probs) {
    ## Creates and stores the transition matrix if not saved in mem,
    ## else load saved one
    if(length(moveInfo$mem$tmax) == 0) {
        transitionMatrix = makeTransitionMatrix(edges)
        moveInfo$mem$tmax = transitionMatrix
    } else {
        transitionMatrix = moveInfo$mem$tmax
    }

    ## Current obeservations
    currentObservation = get.observations(readings, probs)

    ## If backpacker dies set observation to 1, else if still alive
    ## set 0
    for(i in 1:length(positions[1:2])) {
        if(is.na(positions[i]) == FALSE) {
            if(positions[i] < 0) {
                currentObservation[-positions[i]] = 1
            }
             else {
                currentObservation[positions[i]] = 0
            }
        }
    }

    ## Store current observations to mem if no past exists, else load
    ## past and multiply with current and store new
    if(length(moveInfo$mem$obs) == 0) {
        moveInfo$mem$obs = currentObservation #%*% transitionMatrix
    } else {
        pastObservation = moveInfo$mem$obs
        currentObservation = normalize((pastObservation %*% transitionMatrix) * currentObservation)
        moveInfo$mem$obs = currentObservation
    }

    ## Get path from position to propable croc position
    path = shortest.path(positions[3], which.max(currentObservation), edges)

    ## Return next 2 moves from path
    if(length(path) == 1) {
        moveInfo$moves = c(0, 0)
    } else if(length(path) == 2) {
        moveInfo$moves = c(path[2], 0)
    } else {
        moveInfo$moves = path[2:3]
    }
    return(moveInfo)
}

shortest.path = function(start, goal, edges){
    if(start == goal){
        return(start)
    } else if(any(apply(edges, 1, function(x, y) isTRUE(all.equal(x, y)), c(start,goal))) |
              any(apply(edges, 1, function(x, y) isTRUE(all.equal(x, y)), c(goal,start)))) {
        return(c(start,goal))
    } else {
        temp.edges = c()
        for(i in 1:nrow(edges)){
            temp.edges = c(temp.edges, edges[i,1:2])
        }
        g = make_empty_graph(n = 40, directed = FALSE) %>% add_edges(temp.edges)
        return(get.all.shortest.paths(g, start, goal)$res[[1]])
    }
}

## Returns the probability distribution
get.observations = function(reads, probs){
    numProbs = length(probs$salinity[,1])

    salinity  = dnorm(reads[1], mean=probs$salinity[1:numProbs,1], sd=probs$salinity[1:numProbs,2])
    phosphate = dnorm(reads[2], mean=probs$phosphate[1:numProbs,1], sd=probs$phosphate[1:numProbs,2])
    nitrogen  = dnorm(reads[3], mean=probs$nitrogen[1:numProbs,1], sd=probs$nitrogen[1:numProbs,2])

    return(salinity * phosphate * nitrogen)
}

## Simple normalizing function
normalize = function(x){
    return(x/sum(x))
}

## Creates a transition matrix with edges
makeTransitionMatrix = function(edges) {
    numPoints = edges[which.max(edges[,2]), 2]
    transitionMatrix = matrix(0, ncol=numPoints, nrow=numPoints)

    for(point in 1:numPoints) {
        to = c(edges[which(edges[, 1] == point), 2],
               edges[which(edges[, 2] == point), 1], point)
        for(edge in 1:length(to)) {
            transitionMatrix[point, to[edge]] = 1 / length(to)
        }
    }

    return(transitionMatrix)
}

test.run <- replicate(1000,runWheresCroc(smartWC, pause=0))
cumulative.average = cumsum(test.run) / seq_along(test.run)
## qplot(1:1000, cumulative.average, geom=c("line","smooth"), ylab="Turns, Cumulatice Average", 
##       xlab="Test runs", main="Performance (smartWC)")
print(tail(cumulative.average, n=1))
