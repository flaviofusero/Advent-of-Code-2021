library(purrr)
library(stringr)
library(data.table)

# The main observations are the following:
#
#   1. The code blocks (i.e. the lines of code delimited by two "inp w") are actually
#   all very similar. In fact they differ only by three constants: the first one divides the z, 
#   the second one is added to the x, and the last one is added to the y.
#
#   2. The final value of z in each code block is given by the following formula, 
#   which I wrote by "decompiling" the block instructions:
#       z <- trunc(z / zdiv[block]) * (25 * ((z %% 26) + xadd[block] != w) + 1) + 
#            ((z %% 26) + xadd[block] != w) * (w + yadd[block])
#   where zdiv, xadd, yadd are the three constants I mentioned earlier.
#
#   3. If we express the z variable in base 26, then each code block adds one digit 
#   to the z precisely when zdiv = 1, and can either remove one digit from the z 
#   or keep the number of digits untouched when zdiv = 26. This comes from the fact that the condition
#        (z %% 26) + xadd[block] != w)
#   is always verified in the blocks where zdiv = 1 (because xadd is always >= 10 in those 
#   blocks, but w is always less than 10).
#   Since there are 7 code chunks where zdiv = 1 and 7 where zdiv = 26, and since the final number of 
#   digits of z must be 1 (because z must be 0), then it must be that every code block where zdiv = 26 
#   removes one digit. This happens exactly when
#       ((z %% 26) + xadd[block] == w)
#   So this condition can be used to discard the great majority of the possibilities, and leaves only 
#   a few which can be bruteforced.

lines <- readLines('input.txt')
inpw_lines <- which(lines == 'inp w')

zdiv <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 4], ' ')[[1]][3]))
xadd <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 5], ' ')[[1]][3]))
yadd <- map_dbl(1:14, ~ as.numeric(strsplit(lines[inpw_lines[.] + 15], ' ')[[1]][3]))

solve <- function(block, w, z, all_w) {
  all_w <- c(all_w, w)
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

w_range <- 9:1 # reverse for part 2
for (w in w_range) {
  r <- solve(1, w, 0, NULL)
  if (is_numeric(r)) {
    print(paste0(r, collapse = ''))
    break
  }
}
