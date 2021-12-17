library(tidyverse)
library(Rmpfr)

path <- 'input.txt'

hex2bin <- function(hex) {
  dict <- c('0' = '0000',
            '1' = '0001',
            '2' = '0010',
            '3' = '0011',
            '4' = '0100',
            '5' = '0101',
            '6' = '0110',
            '7' = '0111',
            '8' = '1000',
            '9' = '1001',
            'A' = '1010',
            'B' = '1011',
            'C' = '1100',
            'D' = '1101',
            'E' = '1110',
            'F' = '1111')
  
  dict[unlist(str_split(hex, ''))] %>% 
    paste0(collapse = '')
}

parse_lit <- function(lit) {
  bin <- c()
  parsed_bits <- 0
  
  repeat({
    last_five <- (str_sub(lit, 1, 1) == '0')
    str_sub(lit, 1, 1) <- ''
    
    bin <- append(bin, str_sub(lit, 1, 4))
    str_sub(lit, 1, 4) <- ''
    parsed_bits <- parsed_bits + 5
    
    if (last_five) {
      return(list(pack = lit, 
                  parsed_bits = parsed_bits, 
                  value = mpfr(paste0(bin, collapse = ''), 1024, base = 2)
                  )) 
    }
  })
}

parse_pack <- function(pack, versions = NULL, parsed_bits = 0) {
  versions <- c(versions, strtoi(str_sub(pack, 1, 3), 2))
  str_sub(pack, 1, 3) <- ''
  parsed_bits <- parsed_bits + 3
  
  pack_type <- strtoi(str_sub(pack, 1, 3), 2)
  str_sub(pack, 1, 3) <- ''
  parsed_bits <- parsed_bits + 3
  
  if (pack_type == 4) {
    parsed_lit <- parse_lit(pack)
    return(list(pack = parsed_lit[['pack']], 
                versions = versions, 
                parsed_bits = parsed_bits + parsed_lit[['parsed_bits']], 
                value = parsed_lit[['value']]
    ))
  } else if (pack_type != 4) {
    
    length_type_id <- str_sub(pack, 1, 1)
    str_sub(pack, 1, 1) <- ''
    parsed_bits <- parsed_bits + 1
    
    subpacks <- list()
    if (length_type_id == '0') {
      # length indicates the number of bits of the following sub-packets
      length <- strtoi(str_sub(pack, 1, 15), 2)
      str_sub(pack, 1, 15) <- ''
      parsed_bits <- parsed_bits + 15
      
      counter <- parsed_bits
      while ((parsed_bits - counter) != length) {
        parsed_pack <- parse_pack(pack, versions, parsed_bits)
        parsed_bits <- parsed_pack[['parsed_bits']]
        versions <- parsed_pack[['versions']]
        pack <- parsed_pack[['pack']]
        subpacks[[length(subpacks) + 1]] <- parsed_pack
      }
    } else if (length_type_id == '1') {
      # length indicates the number of following sub-packets
      length <- strtoi(str_sub(pack, 1, 11), 2)
      str_sub(pack, 1, 11) <- ''
      parsed_bits <- parsed_bits + 11
      
      for (i in 1:length) {
        parsed_pack <- parse_pack(pack, versions, parsed_bits)
        parsed_bits <- parsed_pack[['parsed_bits']]
        versions <- parsed_pack[['versions']]
        pack <- parsed_pack[['pack']]    
        subpacks[[length(subpacks) + 1]] <- parsed_pack
      }
    }
    
    if (pack_type == 0) value <- Reduce(sum, subpacks %>% map('value'))
    if (pack_type == 1) value <- Reduce(prod, subpacks %>% map('value'))
    if (pack_type == 2) value <- Reduce(min, subpacks %>% map('value'))
    if (pack_type == 3) value <- Reduce(max, subpacks %>% map('value'))
    if (pack_type == 5) value <- mpfr(map(subpacks, 'value')[[1]] > map(subpacks, 'value')[[2]])
    if (pack_type == 6) value <- mpfr(map(subpacks, 'value')[[1]] < map(subpacks, 'value')[[2]])
    if (pack_type == 7) value <- mpfr(map(subpacks, 'value')[[1]] == map(subpacks, 'value')[[2]])
    
    return(list(pack = pack, versions = versions, parsed_bits = parsed_bits, value = value))
  }
}

pack <- readLines(path) %>%
  hex2bin %>%
  parse_pack

sum(pack$versions)
pack$value
