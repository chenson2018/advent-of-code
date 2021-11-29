# more regex tomfoolery and generally just me pushing how much I can do in just the command line
# YMMV if I missed an edge case not in my input

# Part 1
# filter out entries that have an abba pattern inside [],
# then find remaining with abba pattern

grep -vP '\[[^]]*(.)(.)(?!\1)\2\1' ../input.txt |
grep  -P        '(.)(.)(?!\1)\2\1'              |
wc -l                                           |
xargs echo "Part 1 answer:"

# Part 2
# this is extremely inefficient in how it passes around the whole file as a string variable
# I move all the [] blocks to the end of the string so they are easier to match,
# leaving a pipe in their place so that I don't add extra matches

move_to_end='match($0,/\[[^]]*\]/){
  print substr($0,1,RSTART-1)"|"substr($0,RSTART+RLENGTH)substr($0,RSTART,RLENGTH)
}' 

# max number of [.*]  that appear in a line
max_bracket=$(sed 's/[^[]//g' ../input.txt | awk '{ print length }' | sort | tail -n1)

input=$(cat ../input.txt)

for ((i=1;i<=$max_bracket;i++)); do
  input=$(echo "$input" | awk "$move_to_end")
done

# now that we have [] all at the end, our capture groups are greatly simplified
grep -P '^[^[]*(.)(.)\1.*?\[.*?\2\1\2' <<< $input | 
wc -l                                             | 
xargs echo "Part 2 answer:"
