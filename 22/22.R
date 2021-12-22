library(purrr)
library(stringr)

path <- 'input.txt'

parse_data <- function(path) {
  on <- readLines(path) %>% 
    str_extract_all('(on)|(off)') %>% 
    map_lgl(~ . == 'on')
  
  xyz <- readLines(path) %>% 
    str_extract_all('(-?[:digit:])+') %>% 
    map(as.numeric) %>% 
    map(~ setNames(., c('x0', 'x1', 'y0', 'y1', 'z0', 'z1')))
  
  return(map2(on, xyz, ~ list('on' = .x, 'xyz' = .y)))
}

# part 1

initialize_reactor <- function(dat) {
  grid <- array(F, dim = c(101, 101, 101))
  xyz <- map(dat, 'xyz') %>% 
    map(~ . + 51)
  map(1:length(xyz), function(x) {
    if (all(xyz[[x]] >= 1) & all(xyz[[x]] <= 101)) {
      grid[xyz[[x]][[1]]:xyz[[x]][[2]], xyz[[x]][[3]]:xyz[[x]][[4]], xyz[[x]][[5]]:xyz[[x]][[6]]] <<- 
        map(dat, 'on')[[x]]
    }
  })
  
  sum(grid)
}

initialize_reactor(parse_data(path))

# part 2 

# If the distance between the center of the boxes is greater than the sum of the half-widths
# along any of the axes, then the box don't intersect, otherwise they do
box_interesect <- function(box0, box1) {
  xyz0 <- box0[['xyz']]
  xyz1 <- box1[['xyz']]
  for (coord in c('x', 'y', 'z')) {
    half_width0 <- (xyz0[[paste0(coord, '1')]] - xyz0[[paste0(coord, '0')]]) / 2
    center0 <- xyz0[[paste0(coord, '0')]] + half_width0
    half_width1 <- (xyz1[[paste0(coord, '1')]] - xyz1[[paste0(coord, '0')]]) / 2
    center1 <- xyz1[[paste0(coord, '0')]] + half_width1
    dist <- abs(center1 - center0)
    if (dist > half_width0 + half_width1) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# intersects two boxes, except if the two boxes are the same, or box1 is one of the starting "off" boxes
intersect_boxes <- function(box0, box1) {
  if (!box_interesect(box0, box1) | 
      identical(box0, box1) | 
      !box1[['on']]) {
    return(NULL)
  }
  
  xyz0 <- box0[['xyz']]
  xyz1 <- box1[['xyz']]
  xyz01 <- c()
  for (coord in c('x', 'y', 'z')) {
    sorted <- sort(c(xyz0[[paste0(coord, 0)]], xyz0[[paste0(coord, 1)]], xyz1[[paste0(coord, 0)]], xyz1[[paste0(coord, 1)]]))
    xyz01[paste0(coord, 0)] <- sorted[[2]]
    xyz01[paste0(coord, 1)] <- sorted[[3]]
  }
  
  return(list('on' = TRUE, 'xyz' = xyz01))
}

# Returns list of the intersections of n distinct boxes.
# Intersections[[1]] contains the starting boxes, intersections[[2]] contains the intersections of
# two distinct boxes, intersections[[3]] the intersections of three distinct boxes, etc.
# Skips intersections where the second box is one of the starting "off" boxes as we don't need them.
make_intersections <- function(dat) {
  intersections <- vector('list', 50)
  
  for (j in 1:length(dat)) {
    intersections[[1]] <- append(intersections[[1]], dat[j])
    intersections_new <- intersections
    i <- 1
    while(length(intersections[[i]])) {
      intersections_new[[i + 1]] <- append(
        intersections[[i + 1]],
        compact(map(intersections[[i]], 
                    ~ intersect_boxes(dat[[j]], .)
        ))
      )
      i <- i + 1
    }
    intersections <- intersections_new
  }
  
  return(compact(intersections))
}

box_vol <- function(box) { 
  (box[['xyz']][['x1']] - box[['xyz']][['x0']] + 1) * 
    (box[['xyz']][['y1']] - box[['xyz']][['y0']] + 1) * 
    (box[['xyz']][['z1']] - box[['xyz']][['z0']] + 1)
}

# Sums volumes of a list of boxes. 
# If 'exclude_off' == T, excludes the 'off' boxes (in our case, only relevant for intersections[[1]]).
sum_vol <- function(boxes, exclude_off = T) {
  if (exclude_off) {
    boxes <- keep(boxes, ~ !isFALSE(.[['on']]))
  }
  sum(map_dbl(boxes, ~ box_vol(.)))
}

# The answer to the problem is equal to the sum of the volumes of intersections[[1]], minus the sums of the volumes of
# intersections[[2]], plus the sum of the volumes of intsersection[[3]], etc. (alternating sign).
# We have to make sure to ignore the starting 'off' boxes in intersections[[1]].
# This comes from the inclusion-exclusion formula.

ans2 <- function(dat) {
  intersections <- make_intersections(dat)
  sum(map_dbl(1:length(intersections), function(i) {
    ifelse(i %% 2 == 1, 1, -1) * sum_vol(intersections[[i]])
  }))
}

ans2(parse_data(path))
