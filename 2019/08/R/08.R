#!/usr/bin/env Rscript

input <- readLines("../input.txt")[1]
parsed <- as.integer(strsplit(input, "")[[1]])

# split into even groups
wide <- 25
tall <- 6

layers <- length(parsed) / (wide*tall)
arr    <- array(parsed, c(tall, wide, layers))

# layer with least number of zeroes
min_z  <- arr[,,which.min(sapply(1:layers, function(x) sum(0==arr[,,x])))]

cat("Part 1 answer:", sum(min_z==1)*sum(min_z==2), "\n")

# first non-transparent for each set of layers under a pixel
res <- c()

for (col in 1:wide) {
  for (row in 1:tall) {
    pixels <- arr[row,col,1:layers]
    res <- c(res, pixels[which(pixels != 2)[1]])
  }
}

mat <- matrix(res, byrow = TRUE, ncol = wide, nrow = tall)

cat("Part 2 answer:\n")

prmatrix(mat, rowlab=rep("",tall), collab=rep("",wide))

