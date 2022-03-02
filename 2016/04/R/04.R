#!/usr/bin/env Rscript

check_valid <- function(s) {
  parts <- regmatches(s, regexec("([a-z\\-]*)(\\d+)\\[(.*)\\]", s))

  body  <- parts[[1]][2]
  id    <- parts[[1]][3]
  check <- parts[[1]][4]

  df    <- as.data.frame(table(strsplit(gsub("-", "", body), "")[[1]]))
  df    <- df[order(-df["Freq"], -df["Var1"]),]

  if (check == paste(df[1:5,"Var1"], collapse = '')) {
    return(as.integer(id))
  } else {
    return(0)
  }
}

input <- readLines("../input.txt")
vals  <- sapply(input, check_valid)
cat("Part 1 answer:", sum(vals), "\n")
