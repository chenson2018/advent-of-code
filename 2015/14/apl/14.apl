input←↓9 3⍴8 8 53 13 4 49 20 7 132 12 4 43 9 5 38 10 4 37 3 37 76 9 12 97 37 1 36
travel←{(⍵[2]⍴⍵[1]),(⍵[3]⍴0)}
speed←↑{2503 ⍴ travel ⍵}¨input
dist←+\speed

⎕←p1_ans←⌈/+/speed
⎕←p2_ans←⌈/+/((⍴dist)⍴⌈⌿dist)=dist
