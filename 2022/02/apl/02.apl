a ←'ABC'                                ⍝ useful constants
x ←'XYZ'

raw←{(~⍵∊' ')/⍵}¨⊃⎕NGET'../input.txt'1  ⍝ read input and remove the spaces
p1_interp←{({(⍸⍵⍷x)⌷a}@2) ⍵}¨raw        ⍝ part 1 data interpretation

win←{⍺=⍵:3⋄(⊂⍺,⍵)∊↓(⊢⌺2)4⍴a:6⋄0}        ⍝ component of score for which player won
score←{(⍸⍵⍷a)+⍺ win ⍵}                  ⍝ total score, adding in points for action used
calc←{+⌿{(⊃score/⍺)×⍴⍵}⌸↑⍵}             ⍝ for input, count how many of each of 9 possible games and score

p1_ans←⎕←calc p1_interp

weak←{(⍸⍵⍷a)⌷1⌽a}                       ⍝ get weakness 
pick←{(weak⍣(¯1+⊃⍸⍺⍷1⌽x))⍵}             ⍝ interpret an X/Y/Z apply weak as many times as needed


p2_repalce←{((pick/⌽⍵)@2)⍵}             ⍝ part 2 data interpretation

p2_interp←p2_repalce¨raw
p2_ans←⎕←calc p2_interp

