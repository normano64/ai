## smartDM = function(car, roads, packages) {
##   toGo = 0
##   if(car$load == 0) {

##   } else {
##     toGo = 
##   }
## }

astar = function(sx, sy, gx, gy, hroads, vroads) {
  frontier = queue()
  frontier$push(manhattan(sx, sy, gx, gy), c(x = sx, y = sy))
  visited = list()
  costs = list()

  nrow = nrow(hroads)
  ncol = ncol(vroads)
  costmatrix = matrix(rep(-1, nrow * ncol), ncol = ncol)
  
  while(TRUE) {
    pop = frontier$pop()
    if(all(c(pop[2], pop[3]) == c(gx, gy))) {
      break
    }
    costs[[length(costs) + 1]] = pop[1]
    visited[[length(visited) + 1]] = c(pop[2], pop[3])

    current = c(cost = pop[1], pop[2], pop[3])

    nodes = list()
    paths = list()

    # x is larger than 1
    if(current['x'] > 1) {
      nodes[[length(nodes) + 1]] = c(current['x'] - 1, current['y'])
      paths = append(paths, hroads[current['y'], current['x'] - 1])
    }
    # x is smaller than ncol
    if(current['x'] < ncol) {
      nodes[[length(nodes) + 1]] = c(current['x'] + 1, current['y'])
      paths = append(paths, hroads[current['y'], current['x'] + 1])
    }
    # y is larger than 1
    if(current['y'] > 1) {
      nodes[[length(nodes) + 1]] = c(current['x'], current['y'] - 1)
      paths = append(paths, hroads[current['y'] - 1, current['x']])
    }
    # y is smaller than nrow
    if(current['y'] < nrow) {
      nodes[[length(nodes) + 1]] = c(current['x'], current['y'] + 1)
      paths = append(paths, hroads[current['y'] + 1, current['x']])
    }
    for(i in 1:length(nodes)) {
      cost = (current['cost'] + manhattan(nodes[[i]]['x'], nodes[[i]]['y'], gx, gy) - manhattan(current['x'], current['y'], gx, gy) + paths[[i]])
      if(frontier$exist(c(nodes[[i]]['x'], nodes[[i]]['y']))) {
        if(frontier$peek(c(nodes[[i]]['x'], nodes[[i]]['y'])) > cost) {
          frontier$update(cost, c(nodes[[i]]['x'], nodes[[i]]['y']))
        }
      } else {
        frontier$push(cost, c(nodes[[i]]['x'], nodes[[i]]['y']))
      }
    }
  }
  return (c(visited, costs))
}

manhattan = function(c1, c2, g1, g2) {
  return (abs(c1 - g1) + abs(c2 - g2))
}

# modified priority queue from
# http://rosettacode.org/wiki/Priority_queue#R
queue = function() {
  keys <<- values <<- NULL
  # pushes value with weight as key
  push = function(key, value) {
    temp <- c(keys, key)
    ord <- order(temp)
    keys <<- temp[ord]
    values <<- c(values, list(value))[ord]
  }
  # pops lowest weight
  pop = function() {
    head <- c(keys[[1]], values[[1]])
    keys <<- keys[-1]
    values <<- values[-1]
    return (head)
  }
  # updates value with new key
  update = function(key, value) {
    index <- seek(value)
    keys <<- keys[-index]
    values <<- values[-index]
    push(key, value)
  }
  # returns index of value, private function
  seek = function(value) {
    index <- NULL
    if(!empty()){
      for(i in 1:size()) {
        if(all(values[[i]] == value)) {
          index <- i
          break
        }
      }
    }
    return (index)
  }
  # look up weight of value
  peek = function(value) keys[seek(value)]
  # TRUE if exists, else FALSE
  exist = function(value) !is.null(seek(value))
  # returns length of the queue
  size = function() length(keys)
  # TRUE if queue is empty, else FALSE
  empty = function() length(keys) == 0
  list(push = push, pop = pop, update = update, peek = peek, exist = exist, size = size, empty = empty)
}
