library("DeliveryMan")
library("ggplot2")
library("iterpc")

smartDM = function(roads, car, packages) {
    toGo=0

    ## Load optimal order if in mem
    if(length(car$mem) > 0) {
        optimalOrder = car$mem[[1]]
    }

    ## Find next package to pick up if not loaded
    if(car$load == 0) {
        ## Compute optimal path and save to mem if not set
        if(length(car$mem) == 0) {
            optimalOrder = computeOptimalOrder(packages)
            car$mem = list(optimalOrder)
        }
        ## Next package
        for(i in optimalOrder) {
            if(packages[i,5] == 0 | packages[i,5] == 1) {
                toGo=i
                break
            }
        }
    } else {
        toGo = car$load
    }

    ## Different coordinates if loaded or pick-up
    if(packages[toGo,5] == 0) {
        offset = 0
    } else {
        offset = 2
    }

    ## Use A* for next node towards desination
    path = astar(car$x, car$y, packages[toGo, 1 + offset], packages[toGo, 2 + offset], roads$hroads, roads$vroads)
    if(length(path) > 1) {
        nextNode = path[length(path) - 1][[1]]
    } else {
        nextNode = path[1][[1]]
    }

    ## 2 down, 4 left, 6 right, 8 up and 5 stay still
    if (all(c(car$x + 1, car$y) == nextNode)) {
        nextMove=6
    } else if (all(c(car$x - 1, car$y) == nextNode)) {
        nextMove=4
    } else if (all(c(car$x, car$y + 1) == nextNode)) {
        nextMove=8
    } else if (all(c(car$x, car$y - 1) == nextNode)) {
        nextMove=2
    } else {
        nextMove=5
    }
    car$nextMove = nextMove

    return (car)
}

reconstruct = function(nodes, start) {
    ## List for reconstructed path
    path = list()

    ## Current node as list(node, parent node))
    current = nodes[[length(nodes)]]
    path[[1]] = current[[1]]

    if(all(current[[1]] == start)) {
        return(path)
    }

    ## Loop until path has been reconstructed
    while(!(all(current[[2]] == start))) {
        for(i in 1:length(nodes)) {
            ## Current parent in nodes
            if(all(nodes[[i]][[1]] == current[[2]])) {
                ## Set current node and save it in path
                current = nodes[[i]]
                path[[length(path) + 1]] = current[[1]]
                break
            }
            ## Sys.sleep(1)
        }
    }

    ## Add start to path
    path[[length(path) + 1]] = c(x=start[1], y=start[2])

    return(path)
}

astar = function(sx, sy, gx, gy, hroads, vroads) {
    ## Create frontier queue and push start node
    frontier = queue()
    frontier$push(manhattan(sx, sy, gx, gy), list(c(x=sx, y=sy),c(x=0,y=0)))

    ## Lists of visited nodes
    visited = list()

    ## Number of rows & cols
    nrow = nrow(hroads)
    ncol = ncol(vroads)

    while(TRUE) {
        ## Pop nod with lowest cost
        pop = frontier$pop()
        curcost = pop[[1]]
        current = pop[[2]]
        ## Add node to visited
        visited[[length(visited) + 1]] = list(current, pop[[3]])

        ## Break loop if node is goal
        if(all(current == c(gx, gy))) {
            break;
        }

        ## List of nodes not yet visited
        nodes = list()
        ## List of cost to those nodes
        costs = list()

        ## If node to left exists and not yet visited
        if(current[1] > 1 && !(list(c(current[1] - 1, current[2])) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1] - 1, current[2])
            costs = append(costs, hroads[current[2], current[1] - 1])
        }
        ## If node to right exists and not yet visited
        if(current[1] < nrow && !(list(c(current[1] + 1, current[2])) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1] + 1, current[2])
            costs = append(costs, hroads[current[2], current[1]])
        }
        ## If node below exists and not yet visited
        if(current[2] > 1 && !(list(c(current[1], current[2] - 1)) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1], current[2] - 1)
            costs = append(costs, vroads[current[2] - 1, current[1]])
        }
        ## If node above exists and not yet visited
        if(current[2] < ncol && !(list(c(current[1], current[2] + 1)) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1], current[2] + 1)
            costs = append(costs, vroads[current[2], current[1]])
        }

        ## Loop over found nodes
        for(i in 1:length(nodes)) {
            ## Cost to found node
            cost = (curcost + manhattan(nodes[[i]][1], nodes[[i]][2], gx, gy) - manhattan(current[1], current[2], gx, gy) + costs[[i]])

            ## Push node to frontier or update existing one if lower cost
            if(frontier$exist(nodes[[i]])) {
                if(frontier$peek(nodes[[i]]) > cost) {
                    frontier$update(cost, list(nodes[[i]],current))
                }
            } else {
                frontier$push(cost, list(nodes[[i]],current))
            }
        }
    }

    ## Reconstruct path and return it
    return(reconstruct(visited, c(sx, sy)))
}

## Manhattan distance from (c1, c2) to (g1, g2)
manhattan = function(c1, c2, g1, g2) {
    return (abs(c1 - g1) + abs(c2 - g2))
}

## costs <- c()
## computeOptimalOrder: returns the optimal order of packages in p
computeOptimalOrder=function(p){
    distanceToFirst <- mapply(manhattan, 1, 1, p[1:5,1], p[1:5,2])
    ## Compute costs btw all destinations and origins
    rows <- c()
    for(i in 1:5){
        rows <- c(rows, mapply(manhattan, p[i,3], p[i,4], p[1:5,1], p[1:5,2]))
    }
    routeCosts <- matrix(rows, nrow=5, byrow=TRUE)
    routes <- cbind(distanceToFirst, routeCosts)
    combinations = getall(iterpc(5, 5, ordered=TRUE))
    ## Compute costs for all combinations of paths
    pathCosts <- c()
    for(i in 1:120){
        pathCosts <- c(pathCosts, evalRoute(routes, combinations[i,]))
    }
    ## costs <<- c(costs, min(pathCosts))
    optimal = combinations[which.min(pathCosts),]
    return(optimal)
}

## evalRoute: returns the sum of manhattan distances btw nodes in "deliveryOrder"
evalRoute=function(costTable, deliveryOrder){
    sum = costTable[deliveryOrder[1], 1]
    for(d in 1:(length(deliveryOrder) - 1)){
        sum = sum + costTable[deliveryOrder[d], deliveryOrder[d + 1] + 1]
    }
    return(sum)
}

## modified priority queue from
## http://rosettacode.org/wiki/Priority_queue#R
queue = function() {
    keys <<- values <<- NULL
    ## pushes value with weight as key
    push = function(key, value) {
        temp <- c(keys, key)
        ord <- order(temp)
        keys <<- temp[ord]
        values <<- c(values, list(value))[ord]
    }
    ## pops lowest weight
    pop = function() {
        head <- c(keys[[1]], values[[1]])
        keys <<- keys[-1]
        values <<- values[-1]
        return (head)
    }
    ## updates value with new key
    update = function(key, value) {
        index <- seek(value[[1]])
        keys <<- keys[-index]
        values <<- values[-index]
        push(key, value)
    }
    ## returns index of value, private function
    seek = function(value) {
        index <- NULL
        if(!empty()){
            for(i in 1:size()) {
                if(all(values[[i]][[1]] == value)) {
                    index <- i
                    break
                }
            }
        }
        return (index)
    }
    ## look up weight of value
    peek = function(value) keys[seek(value)]
    ## TRUE if exists, else FALSE
    exist = function(value) !is.null(seek(value))
    ## returns length of the queue
    size = function() length(keys)
    ## TRUE if queue is empty, else FALSE
    empty = function() length(keys) == 0
    list(push = push, pop = pop, update = update, peek = peek, exist = exist, size = size, empty = empty)
}
