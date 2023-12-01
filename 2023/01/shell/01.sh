in="../input.txt"
pat="(\d).*"
paste <(pcregrep -o1 $pat $in) <(rev $in | pcregrep -o1 $pat) -d "" | paste -sd+ | bc

nums="one|two|three|four|five|six|seven|eight|nine"
pat1="(${nums}|\d).*"
pat2="($(rev <<<$nums)|\d).*"
paste <(pcregrep -o1 $pat1 $in) <(rev $in | pcregrep -o1 $pat2 | rev) -d "" | paste -sd+ |
	sed 's/one/1/g' |
	sed 's/two/2/g' |
	sed 's/three/3/g' |
	sed 's/four/4/g' |
	sed 's/five/5/g' |
	sed 's/six/6/g' |
	sed 's/seven/7/g' |
	sed 's/eight/8/g' |
	sed 's/nine/9/g' |
	bc
