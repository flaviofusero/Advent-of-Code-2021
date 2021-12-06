library(stringr)

parse_input <- function(path) {
  dat <- readLines(path)
  dat %>% 
    str_extract_all('[:digit:]+') %>% 
    lapply(as.numeric) %>% 
    lapply(setNames, c('x0', 'y0', 'x1', 'y1')) %>% 
    lapply(function(x) x + 1) # R indexes start at 1
}

path = 'input.txt'
dat <- parse_input(path)

initialize_map <- function(dat) matrix(0, max(unlist(dat)), max(unlist(dat)))

expand_line_coord <- function(p_coord, compute_diags = TRUE) {
  is_diag <- (p_coord['x0'] != p_coord['x1']) & (p_coord['y0'] != p_coord['y1'])
  
  if (compute_diags == FALSE & is_diag) {
    x_line = -1
    y_line = -1
  } else { 
    x_line <- p_coord['x0']:p_coord['x1']
    y_line <- p_coord['y0']:p_coord['y1']
  }
  
  data.frame(x_line, y_line) 
  %>% as.matrix
}

mark_line <- function(map, line) {
  map[line] <- map[line] + 1
  map
}

ans <- function(dat, compute_diags) {
  map <- initialize_map(dat)
  lines <- lapply(dat, expand_line_coord, compute_diags)
  
  for (i in 1:length(dat)) {
    if (compute_diags == TRUE | lines[[i]][1] > 0) map <- mark_line(map, lines[[i]])
  }
  
  sum(map >= 2)
}

# part 1 
ans(dat, compute_diags = FALSE)

# part 2
ans(dat, compute_diags = TRUE)