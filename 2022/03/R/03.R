input<-readLines("../input.txt")
clist<-function(s){strsplit(s,'')}
split_half<-function(l){split(l,rep(1:2,rep(length(l)/2,2)))}
score<-function(s){which(s == c(letters, LETTERS))}
p1_ans<-Reduce("+",Map(function(l) score(Reduce(intersect, split_half(l))),clist(input)))
p2_ans<-Reduce("+",Map(score, apply(matrix(clist(input), ncol = 3, byrow = TRUE), 1, function(ls) Reduce(intersect,ls))))

cat("Part 1 answer:", p1_ans, "\n")
cat("Part 2 answer:", p2_ans, "\n")
