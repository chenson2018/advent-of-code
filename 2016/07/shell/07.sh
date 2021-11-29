# more regex tomfoolery
# first, filter out entries that have an abba pattern inside []
# then find remaining with abba pattern

grep -vP '\[[^]]*(.)(.)(?!\1)\2\1' ../input.txt | grep -P '(.)(.)(?!\1)\2\1' | wc -l | xargs echo "Part 1 answer:"

# WIP, on part 2, not quite right
# grep -P '\[[^]]*(.)(.)\1.*?\].*?\2\1\2' ../input.txt | wc -l
# grep -P '(.)(.)\1.*?\[[^]]*\2\1\2' ../input.txt | wc -l
