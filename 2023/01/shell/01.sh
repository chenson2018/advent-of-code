pcregrep -o1 -o2 "(\d.*\d|(\d))" ../input.txt | pcregrep -o1 -o2 "(.).*(.)" | paste -sd+ | bc

nums="one|two|three|four|five|six|seven|eight|nine|\d"
pcregrep -o1 -o4 "((${nums}).*(${nums})|(${nums}))" ../input.txt | 
	pcregrep -o1 -o2 "(${nums}).*(${nums})" | 
	paste -sd+ |
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
