library(purrr)
library(stringr)

path = 'input.txt'

parse_data <- function(path) {
  templ <- readLines(path)[[1]]
  
  pairs <- substring(templ, first = 1:(nchar(templ) - 1), last = 2:nchar(templ)) %>% 
    table
  
  rules <- readLines(path)[-1:-2] %>% 
    str_extract_all('[A-Z]+')
  
  for (l in unique(unlist(map(rules,2)))) {
    for (m in unique(unlist(map(rules,2)))) {
      if (!paste0(l, m) %in% names(pairs)) {
        pairs[paste0(l, m)] <- 0
      }
    }
  }
  
  return(list(pairs = pairs, rules = rules))
}

play_loop <- function(rules, pairs) {
  new_pairs <- pairs
  
  for (i in 1:length(pairs)) {
      pair <- names(pairs[i])
      new_pairs[pair] <- new_pairs[pair] - pairs[i]
      pairs2add <- c(
        paste0(substr(pair, 1, 1), map(rules, 2)[[which(map(rules, 1) == pair)]]),
        paste0(map(rules, 2)[[which(map(rules, 1) == pair)]], substr(pair, 2, 2))
      )
      new_pairs[pairs2add] <- new_pairs[pairs2add] + pairs[i]
  }
  
  new_pairs
}

play_game <- function(rules, pairs, n) {
  for (iter in 1:n) {
    pairs <- play_loop(rules, pairs) 
  }
  
  pairs
}

ans <- function(dat, n) {
  freq <- c()
  pairs <- play_game(dat$rules, dat$pairs, n)
  
  for (L in unique(unlist(map(dat$rules, 2)))) {
    freq[L] <- ceiling(0.5 * (sum(pairs[grepl(L, names(pairs))]) + pairs[paste0(L, L)]))
    # We sum the value pairs containing L, where pair 'LL' has twice the weight. However, we have
    # to divide by two because every pair (except the first and the last in the template) double count the letter.
    # The 'ceiling' counteracts the division by two for the initial or final pair. It would only fail (be off by one) if the first 
    # and last letter of the template were the same but that is not the case in my input.
  }
  
  max(freq) - min(freq)
}

# part 1
ans(parse_data(path), 10)

# part 2 
options(scipen = 999)
ans(parse_data(path), 40)