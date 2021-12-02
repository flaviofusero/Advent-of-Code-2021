library(dplyr)
library(roll)

parse_data <- function(path) {
  dat <- readLines(path) %>% as.numeric
}

ans <- function(data) {
  sum(diff(data, lag = 1) > 0, na.rm = TRUE)
}

data <- parse_data('input.txt')
print(ans(data))
print(ans(roll_sum(data, 3)))
