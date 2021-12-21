#!/usr/bin/env Rscript


options(scipen=999)

poly_counts <- function(input, init_str, rep) {
  # unique polymer letters
  letters <- unique(strsplit(gsub("[^A-Z]", "", Reduce(paste, input)), "+")[[1]])

  # parse input into a named list
  destination <- strsplit(gsub("([A-Z])([A-Z]) -> ([A-Z])", "\\1\\3 \\3\\2", input, perl = TRUE), " ")
  names(destination) <- gsub("([A-Z]{2}).*", "\\1", input, perl = TRUE)

  
  # create a matrix where each row is named with the initial LHS of AB -> C
  # and the columns are the destinations AC BC 
  t <- matrix(0, nrow = length(input), ncol = length(input))
  rownames(t) <- names(destination)
  colnames(t) <- names(destination)

  for (name in names(destination)) {
    for ( dest in destination[name] ) {
      t[name,dest] <- 1
    }
  }
  

  # initial vector
  init <- sapply(names(destination), function(name) if (length(grep(name, init_str))) 1 else 0)

  # raise transition matrix to a power  
  iter <- Reduce("%*%", replicate(rep, t(t), simplify = FALSE)) %*% init

  # a bit of a hack
  # I count all the the times a character appear in the front half of a pair
  # this gets everything except the last character
  # because of the way the pairs insert, the last character has remained the same from the init string,
  # so I just add that to the count
  counts <- sapply(letters, function(l) sum(iter[grepl(paste0("^", l), rownames(iter)),]))
  last <- substr(init_str, nchar(init_str), nchar(init_str))
  counts[last] <- counts[last] + 1

  max(counts) - min(counts)
}

input    <- readLines("../input.txt")
init_str <- "BSONBHNSSCFPSFOPHKPK"

cat("Part 1 answer:", poly_counts(input, init_str, 10), "\n")
cat("Part 2 answer:", poly_counts(input, init_str, 40), "\n")

