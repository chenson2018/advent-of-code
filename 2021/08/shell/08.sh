# get to the right of the pipe, including the space
# then match each signal pattern with the length desired

grep -oP ".*?\|\K.*" ../input.txt           | 
grep -oP "(?<=\s)(\w{2,4}|\w{7})(?=(\s|$))" | 
wc -l                                       | 
xargs echo "Part 1 answer:"
