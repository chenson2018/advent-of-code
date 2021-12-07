#!/usr/bin/env Rscript

input   <- unlist(read.table("../input.txt", sep = ","))
max_idx <- max(input)

p1_ans  <- min(sapply(1:max_idx, function(align) sum(abs(input-align))))

# here we use the fact that sum(1+2+...+n) = n(n+1)/2
p2_ans  <- min(sapply(1:max_idx, 
                      function(align) { 
                        dist <- abs(input-align)
                        sum(.5*dist*(dist+1))
                      })) 

cat("Part 1 answer:", p1_ans, "\n")
cat("Part 2 answer:", p2_ans, "\n")
