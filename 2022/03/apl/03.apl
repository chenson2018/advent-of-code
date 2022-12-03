in←⊃⎕NGET'../input.txt'1
score←{⍸⍵⍷⎕UCS⊃,/96 64+∘⍳¨26}              ⍝ score for a letter
split←{⍵⊂⍨⍸⍣¯1+\¯1↓1,(⌈(⍴⍵)÷⍺)⍴⍺}            ⍝ split into equal groups
score_group←{score⊃(∪∩)/⍵}                   ⍝ for a collection of strings, find an intersection (assumed unique)
p1_ans←⎕←+/score_group¨{(.5×⍴⍵) split ⍵}¨in  ⍝ groups of each line split in half
p2_ans←⎕←+/score_group¨3 split in            ⍝ groups of three lines each
