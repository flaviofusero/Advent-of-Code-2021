library(purrr)
library(stringr)

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
  for (i in 1:nrow(m)) {
    base <- matrix(as.numeric(m[i, ]), 3)
    if (det(base) == 1 &all( rowSums(abs(base)) == 1) & all(colSums(abs(base)) == 1)) {
      bases[[length(bases) + 1]] <- base
    }
  }
  return(unique(bases))
}

# Computes the pairwise distances between the beacons in each report.
# If at least 13 beacons have the same pairwise distances in both reports,
# outputs the beacons whose pairwise distances are the same in both reports.

nn <- function(x) sqrt(sum(x * x))

get_common_beacons <- function(reports, i, j) {
  di <- apply(reports[[i]], 2, function(x) apply(reports[[i]], 2, function(y) nn(x - y)))
  dj <- apply(reports[[j]], 2, function(x) apply(reports[[j]], 2, function(y) nn(x - y)))
  common_di <- (di > 0) & (di %in% dj)
  common_dj <- (dj > 0) & (dj %in% di)
  
  if(sum(common_di) >= 132) { 
    # 132 = 12 * 11 ways in which you can chose ordered pairs of beacons
    bi <- reports[[i]][, intersect(which(common_di, arr.ind = TRUE)[, 1], 
                                   which(rowSums(common_di) >= 11))]
    bj <- reports[[j]][, intersect(which(common_dj, arr.ind = TRUE)[, 1],
                                   which(rowSums(common_dj) >= 11))]
    return(list(bi = bi, bj = bj))
  }
}

# If the reports use the same coordinate base, we expect the pairwise offsets in x, y and z coordinates of the
# beacons to overlap in all spots.
# If this doesn't happen, we rotate / flip c (i.e. change c's base) and try again.

bj_to_bi <- function(beacons, bases) {
  bi <- beacons$bi
  bj <- beacons$bj
  
  if(is.null(bi) | is.null(bj)) {
    return(NULL)
  }
  
  bi <- t(t(bi)[order(t(bi)[, 1], t(bi)[, 2], t(bi)[, 3]) ,])
  for (base in bases) {
    bj_ornt <- base %*% bj
    bj_ornt <- t(t(bj_ornt)[order(t(bj_ornt)[, 1], t(bj_ornt)[, 2], t(bj_ornt)[, 3]) ,])
    
    if (identical(t(apply(bi, 1, diff)), t(apply(bj_ornt, 1, diff)))) {
      offset <- (bi - bj_ornt)[, 1]
      return(list(base = base, offset = offset))
    }
  }
}

orient_reports <- function(reports, bases) {
  oriented = 1
  to_orient = 2:length(reports)
  sonars <- data.frame('dx' = 0, 'dy' = 0, 'dz' = 0)
  
  while (length(to_orient)) {
    for (i in oriented) {
      for (j in to_orient) {
        beacons_ij <- get_common_beacons(reports, i, j)
        if (length(beacons_ij)) {
          bji <- bj_to_bi(beacons_ij, bases)
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
  orient_reports(bases = make_bases())

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