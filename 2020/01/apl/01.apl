n←⍎⍤1⊢↑⊃⎕NGET'../input.txt'1
{(⊃⍸2020=⍵∘.+⍵)⌷⍵∘.×⍵}n
{(⊃⍸2020=⍵∘.+,⍵∘.+⍵)⌷⍵∘.×,⍵∘.×⍵}n

⍝ a little inefficient because of the space allocated
⍝ but here is a general function (assumes there is a match)
⍝ ⍺: number of entries
⍝ ⍵: vector of numbers

gen←{×/n[⊃⍸2020=⊃(∘.+⍨)/⍺⍴⊂n]}

⍝ answers are then:
2 gen n
3 gen n
