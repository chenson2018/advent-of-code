iter←{⍵,0,~⌽⍵}
checksum←{(=/)¨(⌈0.5×⍳⍴⍵)⊆⍵}
rep_until←{⍵⍵ ⍵:⍵ ⋄ ∇ ⍺⍺ ⍵}
p1_slow←{n←⍺⋄ checksum rep_until {2|⊃⍴⍵} n ↑ iter rep_until {n<⊃⍴⍵} ⍵}

input←1 0 0 0 1 1 1 0 0 1 1 1 1 0 0 0 0
⎕←p1_ans←272 p1_slow input
