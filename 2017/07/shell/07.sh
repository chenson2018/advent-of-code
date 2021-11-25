# a one-liner solution to part one
# first grep gets the entries on the lhs,
# second gets the rhs,
# then comm finds the difference

comm -23                                            \
  <(cat <<<$(grep -oP "^\w+"   ../input.txt | sort)) \
  <(cat <<<$(grep -oP " \K\w+" ../input.txt | sort))  \
