#!/usr/bin/env Rscript

options(scipen=999)

# get the initial counts for each position
input <- unlist(read.table("../input.txt", sep = ","))
init  <- sapply(0:8, function(num) sum(input == num))

# this is a transition matrix for how fish spawn
# this matrix is transposed to make it easier to read

transition <- matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 1, # 0 -> 6, 8
                       1, 0, 0, 0, 0, 0, 0, 0, 0, # 1 -> 0
                       0, 1, 0, 0, 0, 0, 0, 0, 0, # 2 -> 1
                       0, 0, 1, 0, 0, 0, 0, 0, 0, # etc.
                       0, 0, 0, 1, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 1, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 1, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 1, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 1, 0), 
                     ncol = 9)

cat("Part 1 answer:", sum(Reduce("%*%", replicate(80 , transition, simplify = FALSE)) %*% init), "\n")
cat("Part 2 answer:", sum(Reduce("%*%", replicate(256, transition, simplify = FALSE)) %*% init), "\n")
