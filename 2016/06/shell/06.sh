# I'm just trolling a bit here because this one looked easy...

ncol=$(head -n1 ../input.txt | awk '{print length}') # count length of first line
p1_ans=""
p2_ans=""

for ((i=1;i<=$ncol;i++)); do
    counts=$(cut -c $i ../input.txt | sort | uniq -c | sort -rn) # sort and count the ith column
    p1_ans+=$(grep -o -m1 "[a-z]" <<< $counts)                   # append the first letter match (will be most often)
    p2_ans+="${counts: -1}"                                      # append the last character (will be least often)
done

echo "Part 1 answer: $p1_ans"
echo "Part 2 answer: $p2_ans"
