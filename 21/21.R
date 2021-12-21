library(purrr)
library(combinat)

pos <- c(3, 5)
scores <- c(0, 0)
rolls <- 0

# part 1 

play_practice <- function(pos, scores, rolls) {
  while (max(scores) < 1000) {
    player <- ifelse(rolls %% 2 == 0, 1, 2)
    rolls <- rolls + 3
    move <- 3 * (rolls - 1) 
    
    pos[player] <- ifelse((pos[player] + move) %% 10 == 0, 10, (pos[player] + move) %% 10)
    scores[player] <- scores[player] + pos[player]
  }
  
  return(min(scores) * rolls)
}

play_practice(pos, scores, rolls)

# part 2

# calculates how many combinations yield a sum of 3 die rolls equal to 3, 4, ..., 9
outcomes <- combn(rep((1:3), 3), 3) %>% 
  unique(MARGIN = 2) %>% 
  apply(2, sum) %>% 
  table

play_dirac <- (function() {
  # preparing the cache
  cache <- NULL
  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
    cache_set('0', 0)
    cache_set('1', 1)
  }
  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
  }
  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }
  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }
  # Initialize the cache
  cache_reset()
  
  # closure (i.e. cached function)
  function(player, pos, scores, move) {
    args <- paste(player, paste0(pos, collapse = '_'), paste0(scores, collapse = '_'), move, sep = '_')
    
    # cached cases
    if (cache_has_key(args)) { 
      return(cache_get(args))
    }
    
    # non-cached cases
    pos[player] <- ifelse((pos[player] + move) %% 10 == 0, 10, (pos[player] + move) %% 10)
    scores[player] <- scores[player] + pos[player]
    
    if (scores[player] >= 21) {
      if (player == 1) {
        return(c(1, 0))
      } else {
        return(c(0, 1))
      }
    }
    
    player <- ifelse((player + 1) %% 2 == 0, 2, 1)
    
    winners <- map(names(outcomes),
                   ~ outcomes[.] * play_dirac(player, pos, scores, as.numeric(.))
    )
    result <- c(sum(map_dbl(winners, 1)), sum(map_dbl(winners, 2)))

    cache_set(args, result)    
    return(result)
  }
})()

r <- map(names(outcomes),
         ~ outcomes[.] * play_dirac(1, pos, scores, as.numeric(.))
)

max(c(sum(map_dbl(r, 1)), sum(map_dbl(r, 2))))