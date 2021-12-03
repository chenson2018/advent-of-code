# adapted from Day 6, 2016

ncol=$(head -n1 ../input.txt | awk '{print length}') # count length of first line
gamma=""
epsilon=""

for ((i=1;i<=$ncol;i++)); do
    counts=$(cut -c $i ../input.txt | sort | uniq -c | sort -rn) # sort and count the ith column
    gamma+=$(grep -m1 -oP "(\d)$" <<< $counts)                    # append the first line match (will be most often)
    epsilon+="${counts: -1}"                                      # append the last character (will be least often)
done

# fun bash binary conversion!
echo "Part 1 answer: $(("$((2#$gamma))*$((2#$epsilon))"))"

