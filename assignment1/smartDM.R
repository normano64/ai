## smartDM = function(car, roads, packages) {
##   toGo = 0
##   if(car$load == 0) {

##   } else {
##     toGo = 
##   }
## }

reconstruct = function(nodes, start) {
    ## List for reconstructed path
    path = list()

    ## Current node as list(node, parent node))
    current = nodes[[length(nodes)]]
    path[[1]] = current[[1]]

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
    frontier$push(manhattan(c(sx, sy), c(gx, gy)), list(c(x=sx, y=sy),c(x=0,y=0)))
    
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
            costs = append(costs, vroads[current[2], current[1] - 1])
        }   
        ## If node to right exists and not yet visited
        if(current[1] < ncol && !(list(c(current[1] + 1, current[2])) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1] + 1, current[2])
            costs = append(costs, vroads[current[2], current[1] + 1])
        }   
        ## If node below exists and not yet visited
        if(current[2] > 1 && !(list(c(current[1], current[2] - 1)) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1], current[2] - 1)
            costs = append(costs, hroads[current[2] - 1, current[1]])
        }   
        ## If node above exists and not yet visited
        if(current[2] < nrow && !(list(c(current[1], current[2] + 1)) %in% visited)) {
            nodes[[length(nodes) + 1]] = c(current[1], current[2] + 1)
            costs = append(costs, hroads[current[2], current[1]])
        }
        
        ## Loop over found nodes
        for(i in 1:length(nodes)) {
            ## Cost to found node
            cost = (curcost + manhattan(nodes[[i]], c(gx, gy)) - manhattan(current, c(gx, gy)) + costs[[i]])
            
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
manhattan = function(c, g) {
    return (abs(c[1] - g[1]) + abs(c[2] - g[2]))
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
