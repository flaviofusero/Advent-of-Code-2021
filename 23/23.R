library(purrr)
library(stringr)
library(collections)

cost <- c('A' = 1, 'B' = 10, 'C' = 100, 'D' = 1000)
entrance = c('A' = 3, 'B' = 5, 'C' = 7, 'D' = 9)

# moves from room r to corridor tile c, returns -1 is the move is illegal
move_r2c <- function(rooms, corridor, r, c) {
  i_max <- length(rooms[[1]])
  if (c %in% c(3, 5, 7, 9) |
      all(rooms[[r]] == '.') | 
      (2 * r + 1 >= c & any(corridor[c:(2 * r + 1)] != '.')) | 
      (c >= 2 * r + 1 & any(corridor[(2 * r + 1):c] != '.'))) {
    return(-1)
  }
  steps <- abs(2 * r + 1 - c)
  
  found <- FALSE
  for (i in 1:i_max) {
    if (rooms[[r]][i] != '.' & !found) {
      pawn <- rooms[[r]][i]
      rooms[[r]][i] <- '.'
      steps <- steps + i
      found <- TRUE
    }
  }
  
  if ( (pawn == 'A' & r == 1 & all(rooms[[r]] %in% c('.', 'A'))) |
       (pawn == 'B' & r == 2 & all(rooms[[r]] %in% c('.', 'B'))) |
       (pawn == 'C' & r == 3 & all(rooms[[r]] %in% c('.', 'C'))) |
       (pawn == 'D' & r == 4 & all(rooms[[r]] %in% c('.', 'D'))) ) {
    return(-1)
  }
  
  corridor[c] <- pawn
  
  return(list(rooms, corridor, steps * cost[pawn]))
}

# moves from room r to room s, returns -1 is the move is illegal
move_r2r <- function(rooms, corridor, r, s) {
  i_max <- length(rooms[[1]])
  if (all(rooms[[r]] == '.') | 
      r == s |
      (r < s & any(corridor[(2 * r + 1):(2 * s + 1)] != '.')) | 
      (r > s & any(corridor[(2 * s + 1):(2 * r + 1)] != '.'))) {
    return(-1)
  }
  steps <- abs(2 * abs(r - s))
  
  found <- FALSE
  for (i in 1:i_max) {
    if (rooms[[r]][i] != '.' & !found) {
      pawn <- rooms[[r]][i]
      rooms[[r]][i] <- '.'
      steps <- steps + i
      found <- TRUE
    }
  }
  
  if ( (pawn == 'A' & r == 1 & all(rooms[[r]] %in% c('.', 'A'))) |
       (pawn == 'B' & r == 2 & all(rooms[[r]] %in% c('.', 'B'))) |
       (pawn == 'C' & r == 3 & all(rooms[[r]] %in% c('.', 'C'))) |
       (pawn == 'D' & r == 4 & all(rooms[[r]] %in% c('.', 'D'))) ) {
    return(-1)
  }
  
  if ( (pawn == 'A' & !(s == 1 & all(rooms[[s]] %in% c('.', 'A')))) |
       (pawn == 'B' & !(s == 2 & all(rooms[[s]] %in% c('.', 'B')))) |
       (pawn == 'C' & !(s == 3 & all(rooms[[s]] %in% c('.', 'C')))) |
       (pawn == 'D' & !(s == 4 & all(rooms[[s]] %in% c('.', 'D')))) ) {
    return(-1)
  }
  
  dots <- which(rooms[[s]] == '.')
  rooms[[s]][dots[length(dots)]] <- pawn
  steps <- steps + dots[length(dots)]
  
  return(list(rooms, corridor, steps * cost[pawn]))
}

# moves from corridor tile c to room r, returns -1 if the move is illegal
move_c2r <- function(rooms, corridor, c, r) {
  i_max <- length(rooms[[1]])
  if(corridor[c] == '.' |
     all(rooms[[r]] != '.') |
     (c < 2 * r + 1 & any(corridor[(c + 1):(2 * r + 1)] != '.')) |
     (c > 2 * r + 1 & any(corridor[(2 * r + 1):(c - 1)] != '.'))){
    return(-1)
  }
  
  steps <- abs(c - (2 * r + 1))
  pawn <- corridor[c]
  corridor[c] <- '.'
  
  if ( !((pawn == 'A' & r == 1 & all(rooms[[r]] %in% c('.', 'A'))) |
         (pawn == 'B' & r == 2 & all(rooms[[r]] %in% c('.', 'B'))) |
         (pawn == 'C' & r == 3 & all(rooms[[r]] %in% c('.', 'C'))) |
         (pawn == 'D' & r == 4 & all(rooms[[r]] %in% c('.', 'D')))) ) {
    return(-1)
  }
  
  found <- FALSE
  for (i in i_max:1) {
    if (rooms[[r]][i] == '.' & !found) {
      rooms[[r]][i] <- pawn
      steps <- steps + i
      found <- TRUE
    }
  }
  
  return(list(rooms, corridor, steps * cost[pawn]))
}

# Gets neighboring states, prioritising c2r.
# Returns c2r if any are found, else r2r if any are found, else r2c.
get_neighbors <- function(rooms, corridor) {
  neighbors <- list()
  
  found_c2r <- FALSE
  for (r in 1:length(rooms)) {
    for (c in 1:length(corridor)) {
      c2r <- move_c2r(rooms, corridor, c, r)
      if (!is.numeric(c2r)) {
        found_c2r <- TRUE
        neighbors[[length(neighbors) + 1]] <- c2r
      }
    }
  }
  
  if (!found_c2r) {
    found_r2r <- FALSE
    for (r in 1:length(rooms)) {
      for (s in 1:length(rooms)) {
        r2r <- move_r2r(rooms, corridor, r, s)
        if (!is.numeric(r2r)) {
          found_r2r <- TRUE
          neighbors[[length(neighbors) + 1]] <- r2r
        }
      }
    }
    
    if (!found_r2r) {
      for (r in 1:length(rooms)) {
        for (c in 1:length(corridor)) {
          r2c <- move_r2c(rooms, corridor, r, c)
          if (!is.numeric(r2c)) {
            neighbors[[length(neighbors) + 1]] <- r2c 
          }
        }
      }
    }
  }
  
  return(neighbors)
}

# convenience functions for going from lists to arrays and viceversa
as.flat <- function(rooms, corridor) {
  paste0(paste0(map_chr(rooms, ~ paste0(., collapse = '')), collapse = ''),
         paste0(corridor, collapse = ''),
         collapse = '')
}

unflat <- function(flat, n = 2) {
  split <- unlist(str_split(flat, ''))
  
  if (n == 2) {
    rooms <- list(c(split[1], split[2]), c(split[3], split[4]),
                  c(split[5], split[6]), c(split[7], split[8]))
    corridor <- split[9:19]
  } else if (n == 4) {
    rooms <- list(c(split[1], split[2], split[3], split[4]), 
                  c(split[5], split[6], split[7], split[8]), 
                  c(split[9], split[10], split[11], split[12]),
                  c(split[13], split[14], split[15], split[16])
    )
    corridor <- split[17:27]
  }
  
  return(list(rooms, corridor))
}

# Heuristic for the A* algorithm. 
# Masures how far the corridor pawns are from their room.
heuristic <- function(current, n) {
  corridor <- unflat(current, n = n)[[1]]
  h <- 0
  for (pawn in c('A', 'B', 'C', 'D')) {
    pos <- which(corridor == pawn)
    if (length(pos)) {
      h <- h + abs(pos - entrance[pawn]) * cost[pawn]
    }
  }
  h
}

a_star <- function(start_rooms, start_corridor, goal) {
  start <- as.flat(start_rooms, start_corridor)
  frontier <- priority_queue(start, 0)
  came_from <- 'none' %>% setNames(start)
  cost_so_far <- c(0) %>% setNames(start)
  n <- length(start_rooms[[1]])
  
  i = 0
  while (frontier$size()) {
    if (i %% 1000 == 0) print(paste0('iter = ', i, '; max_cost = ', max(cost_so_far)))
    current <- frontier$pop()
    if (current == goal) {
      return(cost_so_far[goal])
    }
    
    neighbors <- get_neighbors(unflat(current, n = n)[[1]], unflat(current, n = n)[[2]])
    for (neighbor in neighbors) {
      flat_neighbor <- as.flat(neighbor[[1]], neighbor[[2]])
      new_cost <- cost_so_far[current] + neighbor[[3]]
      if (!(flat_neighbor %in% names(cost_so_far)) | new_cost < cost_so_far[flat_neighbor]) {
        cost_so_far[flat_neighbor] <- new_cost
        priority <- new_cost + heuristic(current, n = n)
        frontier <- frontier$push(flat_neighbor, -priority) 
        came_from[flat_neighbor] <- current
      }
    }
    i = i + 1
  }
}

# part 1
t0 <- Sys.time()
a_star(start_rooms = list(c('D', 'B'), c('D', 'A'), c('C', 'A'), c('B','C')),
         start_corridor = rep('.', 11),
         goal = 'AABBCCDD...........')
print(Sys.time() - t0)

# part 2
t0 <- Sys.time()
  a_star(start_rooms = list(c('D', 'D', 'D', 'B'),
                          c('D', 'C', 'B', 'A'),
                          c('C','B', 'A', 'A'),
                          c('B', 'A', 'C', 'C')),
       start_corridor = rep('.', 11),
       goal = 'AAAABBBBCCCCDDDD...........')
print(Sys.time() - t0)