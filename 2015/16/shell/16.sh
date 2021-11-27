# check that we match one of the acceptable lines three times in a row
grep -P "((children: 3|cats: 7|samoyeds: 2|pomeranians: 3|akitas: 0|vizslas: 0|goldfish: 5|trees: 3|cars: 2|perfumes: 1)(,\s|$)){3}" ../input.txt |
grep -oP "Sue \K\d+"                                                                                                                              | 
xargs echo "Part 1 answer:"

# First we filter out the lines that do not match the greater/less than conditions
# Note that I do this by matching the unacceptable lines, then inverting the match
# Now that these lines are filtered, we use the same regex as part 1,
# but allow the items with greater/less than conditions to match any number (since we already filtered them)

grep -vP "cats: [0-6](,|$)" ../input.txt                                                                                                      | 
grep -vP "trees: [0-2](,|$)"                                                                                                                  | 
grep -vP "goldfish: ([5-9]|10)"                                                                                                               | 
grep -vP "pomeranians: ([3-9]|10)"                                                                                                            |
grep -P  "((children: 3|cats: \d+|samoyeds: 2|pomeranians: \d+|akitas: 0|vizslas: 0|goldfish: \d+|trees: \d+|cars: 2|perfumes: 1)(,\s|$)){3}" |
grep -oP "Sue \K\d+"                                                                                                                          | 
xargs echo "Part 2 answer:"
