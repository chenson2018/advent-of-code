parse_matching <- function(text) {
  m <- regmatches(text,regexec(".*?:\\s+(.*?)\\s\\|\\s+(.*)",text))[[1]]
  wins <- eval(parse(text=paste0("c(", gsub("\\s+", ",", m[2]), ")")))
  nums <- eval(parse(text=paste0("c(", gsub("\\s+", ",", m[3]), ")")))
  length(intersect(wins, nums))
}

p1_score <- function(n_match) { if (n_match == 0) { 0 } else { 2 ^ (n_match - 1) } }

# function for creating the matrices used in part 2

p2_mat <- function(len, matches, idx) {
  mat <- diag(len)
  m <- matches[idx]
  if (m > 0) {
    range <- (idx+1):(idx+m)
    mat[idx,range] <- mat[idx,range] + rep(1, m)
  }
  mat
}

p2_calc <- function(matches) {
  len <- length(matches)
  mats <- lapply(1:len, function(i) { p2_mat(len, matches, i) })
  vec <- Reduce("%*%", mats) %*% rep(1, len)
  sum(vec)
}

input <- readLines("../input.txt")
matches <- sapply(input, parse_matching, USE.NAMES = FALSE)
p1_ans <- sum(sapply(matches, p1_score))
p2_ans <- p2_calc(matches)

cat("Part 1 answer:", p1_ans, "\n")
cat("Part 2 answer:", p2_ans, "\n")
