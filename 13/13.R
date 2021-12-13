library(purrr)
library(stringr)
library(data.table)
library(ggplot2)

path = 'input.txt'

parse_data <- function(path) {
  dat <- read_file(path) %>% 
    str_split('\r\n\r\n') %>% 
    unlist
  
  dt <- fread(text = dat[1], col.names = c('x', 'y'))
  folds <- list('axes' = str_extract_all(dat[2], '[xy]') %>% unlist,
                'xy' = str_extract_all(dat[2], '[:digit:]+') %>% unlist %>% as.numeric
  )
  
  return(list('dt' = dt, 'folds' = folds))
}

dat <- parse_data(path)

fold_along <- function(dt, fold_ax, xy) {
  df <- copy(dt)
  df[get(fold_ax) >= xy, (fold_ax) := 2 * xy - get(fold_ax)]
  
  return(unique(df))
}

make_folds <- function(dt, folds) {
  for (i in 1:length(folds[['axes']])) {
    dt <- fold_along(dt, map(folds, i)[['axes']], map(folds, i)[['xy']])
  }
  
  return(dt)
}

dt <- make_folds(dat[['dt']], dat[['folds']])

ggplot(dt) +
  geom_point(aes(x, - y)) +
  ylim(-15, 15)
