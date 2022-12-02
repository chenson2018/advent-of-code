raw←{(~⍵∊' ')/⍵}¨⊃⎕NGET'../input.txt'1                     ⍝ read input and remove the spaces
p1_interp←{'C'@{'Z'=⍵}⍵}¨{'B'@{'Y'=⍵}⍵}¨{'A'@{'X'=⍵}⍵}¨raw ⍝ part 1 data interpretation

win←{⍺=⍵:3⋄(⊂⍺,⍵)∊'CA' 'BC' 'AB':6⋄0}                      ⍝ component of score for which player won
score←{(⍸⍵⍷'ABC')+(⍺ win ⍵)}                               ⍝ total score, adding in points for action used
calc←{+⌿{(⊃score/⍺)×(⍴⍵)}⌸↑⍵}                              ⍝ for input, count how many of each of 9 possible games and score

p1_ans←⎕←calc p1_interp

weak←{⍵='A':'B'⋄⍵='B':'C'⋄⍵='C':'A'}                       ⍝ get weakness 
pick←{⍺='Y':⍵⋄⍺='Z':weak ⍵⋄⍺='X':weak weak ⍵}              ⍝ interpret an X/Y/Z symbol, note weak weak is strong

p2_interp←{((pick/⌽⍵)@2)⍵}¨raw                             ⍝ part 2 data interpretation
p2_ans←⎕←calc p2_interp

