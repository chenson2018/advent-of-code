# Here's a version specific to successive differences
awk 'NR > 1 {s+=($0-prev)>0} {prev = $0} END {print "Part 1 answer: " s}' ../input.txt

# here's a more general version for any window size

awk -v N=3 '{if (NR > N) {s+=($0-arr[NR-N])>0}} {arr[NR] = $0} END {print "Part 2 answer: " s}' ../input.txt
