parse_ins <- function(ins) {
  parse <- as.integer(regmatches(ins, regexec("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)", ins, perl = TRUE))[[1]][-1])

  list(id     = parse[1],
       left   = parse[2],
       right  = parse[3],
       width  = parse[4],
       length = parse[5])
}


fabric <- function(input, mat_size) {
  ret <- matrix(0, nrow = mat_size, ncol = mat_size)
  for (ins in input) {
    ret[(ins$right+1):(ins$right+ins$length),(ins$left+1):(ins$left+ins$width)] <- ret[(ins$right+1):(ins$right+ins$length),(ins$left+1):(ins$left+ins$width)] + 1
  }
  ret
}

overlap <- function(input, claims) {
  for (ins in input) {
    if ( all(claims[(ins$right+1):(ins$right+ins$length),(ins$left+1):(ins$left+ins$width)] == 1 ) ) {
      return(ins$id)
    }
  }
}

input <- lapply(readLines("../input.txt"), parse_ins)
claims <- fabric(input, 1000)

cat("Part 1 answer:", sum(claims > 1)       , "\n")
cat("Part 2 answer:", overlap(input, claims), "\n")
