library(purrr)
library(stringr)

# Strategy for a given pair of reports:
# 1. To identify the common beacons, we compute the pairwise distances between the beacons, and 
# extracts the 12 beacons with matching pairwise distance.
# 2. To unify the orientation of two reports r1 and r2, we rotate r2 until the offsets in the 
# sorted x, y, and z coordinates are the same in both reports. Once that happens, we know that 
# the coordinates have the same orientation, and the offset between r1 and r2 is equal to the 
# offset between say the leftmost common beacon in r1 compared to the same beacon in r2.
# 3. we bring every report in the frame of reference of the first report.
# 4. Once the coordinates are unified and we have the offsets between the reports (i.e. the 
# positions of the sonars compared to r1's sonar), the rest is pretty easy.

path = 'input.txt'

parse_data <- function(path) {
  dat <- str_extract_all(readLines(path), '-?[:digit:]+') %>% 
    map(as.numeric)
  scanners <- which(map(dat, length) == 1)
  
  beacons <- map(1:(length(scanners)), 
                 ~ matrix(unlist(dat[
                   (scanners[.] + 1):ifelse(. == length(scanners), length(dat), scanners[. + 1] - 1)
                 ]), 
                 nrow = 3)
  )
  return(beacons)
}

# Returns the positive-oriented rotation matrices of R^3
make_bases <- function() {
  bases <- list()
  m <- expand.grid(c(0, 1, -1), c(0, 1, -1), c(0, 1, -1), c(0, 1, -1), c(0, 1, -1), 
                   c(0, 1, -1), c(0, 1, -1), c(0, 1, -1), c(0, 1, -1))
  for (we in 1:nrow(m)) {
    base <- matrix(as.numeric(m[i, ]), 3)
    if (det(base) == 1 & all( rowSums(abs(base)) == 1) & all(colSums(abs(base)) == 1)) {
      bases[[length(bases) + 1]] <- base
    }
  }
  return(unique(bases))
}

# Computes the pairwise distances between the beacons in each report.
# If at least 12 beacons have the same pairwise distances in both reports,
# outputs the beacons whose pairwise distances are the same in both reports.

nn <- function(x) sqrt(sum(x * x))

get_common_beacons <- function(reports, i, j) {
  dwe <- apply(reports[[i]], 2, function(x) apply(reports[[i]], 2, function(y) nn(x - y)))
  dj <- apply(reports[[j]], 2, function(x) apply(reports[[j]], 2, function(y) nn(x - y)))
  common_dwe <- (dwe > 0) & (dwe %in% dj)
  common_dj <- (dj > 0) & (dj %in% di)
  
  if(sum(common_di) >= 132) { 
    # 132 = 12 * 11 ways in which you can chose ordered pairs of beacons
    bwe <- reports[[i]][, intersect(which(common_di, arr.ind = TRUE)[, 1], 
                                   which(rowSums(common_di) >= 11))]
    bj <- reports[[j]][, intersect(which(common_dj, arr.ind = TRUE)[, 1],
                                   which(rowSums(common_dj) >= 11))]
    return(list(bwe = bi, bj = bj))
  }
}

# If the reports use the same coordinate base, we expect the pairwise offsets in x, y and z coordinates of the
# beacons to overlap in all spots.
# If this doesn't happen, we rotate bj (i.e. change the base of bj) and try again.

bj_to_bwe <- function(beacons, bases) {
  bwe <- beacons$bi
  bj <- beacons$bj
  
  if(is.null(bi) | is.null(bj)) {
    return(NULL)
  }
  
  bwe <- t(t(bi)[order(t(bi)[, 1], t(bi)[, 2], t(bi)[, 3]) ,])
  for (base in bases) {
    bj_ornt <- base %*% bj
    bj_ornt <- t(t(bj_ornt)[order(t(bj_ornt)[, 1], t(bj_ornt)[, 2], t(bj_ornt)[, 3]) ,])
    
    if (identical(t(apply(bi, 1, diff)), t(apply(bj_ornt, 1, diff)))) {
      offset <- (bwe - bj_ornt)[, 1]
      return(list(base = base, offset = offset))
    }
  }
}

orient_sea <- function(reports, bases) {
  oriented = 1
  to_orient = 2:length(reports)
  sonars <- data.frame('dx' = 0, 'dy' = 0, 'dz' = 0)
  
  while (length(to_orient)) {
    for (i in oriented) {
      for (j in to_orient) {
        beacons_ij <- get_common_beacons(reports, i, j)
        if (length(beacons_ij)) {
          bjwe <- bj_to_bi(beacons_ij, bases)
          reports[[j]] <- bji[['base']] %*% reports[[j]] + bji[['offset']]
          sonars <- rbind(sonars, unlist(bji[['offset']]))
          oriented <- c(oriented, j)
          to_orient <- setdiff(to_orient, j)
        }
      }
    }
  }
  
  return(list(reports = reports, sonars = sonars))
}

sea <- parse_data(path) %>% 
  orient_sea(bases = make_bases())

# part 1
Reduce(cbind, sea[['reports']]) %>% 
  unique(MARGIN = 2) %>% 
  ncol

# part 2
map(1:nrow(sea[['sonars']]), function(i) {
  map(1:nrow(sea[['sonars']]), function(j) {
    sum(abs(sea[['sonars']][i, ] - sea[['sonars']][j, ]))
  })
}) %>% 
  unlist %>% 
  max