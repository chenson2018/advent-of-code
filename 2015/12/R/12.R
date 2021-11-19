#!/usr/bin/env R

library(jsonlite)

p1 <- function(input){
  flat   <- unlist(input, recursive = TRUE, use.names = FALSE)
  nums   <- flat[grepl("\\-?\\d+", flat)]
  sum(as.integer(nums))
}

input  <- jsonlite::read_json("../input.txt")
cat("Part 1 answer:", p1(input), "\n")

