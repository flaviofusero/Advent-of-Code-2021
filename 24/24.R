library(purrr)
library(stringr)
library(data.table)

lines <- readLines('input.txt')
inpw_lines <- which(lines == 'inp w')

max_block <- 0

zdiv <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 4], ' ')[[1]][3]))
xadd <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 5], ' ')[[1]][3]))
yadd <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 15], ' ')[[1]][3]))

solve <- function(block, w, z, all_w) {
  all_w <- c(all_w, w)
  # print(paste0('block: ', block, '; w: ', w, '; z:', z, '; all_w: ', all_w))
  if (xadd[block] < 0 & ((z %% 26) + xadd[block] != w)) {
    return(FALSE) 
  }
  z <- trunc(z / zdiv[block]) * (25 * ((z %% 26) + xadd[block] != w) + 1) + 
    ((z %% 26) + xadd[block] != w) * (w + yadd[block])
  
  if (block == 14) {
    if (z == 0) {
      return(all_w)
    } else {
      return(FALSE)
    }
  }
  
  for (w_new in w_range) {
    r <- solve(block + 1, w_new, z, all_w)
    if (is.numeric(r)) {
      return(r)
    }
  }
  
  return(FALSE)
}

t0 <- Sys.time()

w_range <- 9:1 # reverse for part 2
for (w in w_range) {
  r <- solve(1, w, 0, NULL)
  if (is_numeric(r)) {
    print(paste0(r, collapse = ''))
    break
  }
}
