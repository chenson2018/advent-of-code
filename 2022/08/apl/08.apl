input←↑⍎¨¨⊃⎕NGET '../input.txt' 1
rn←{(⌽∘⍉⍣⍺)⍵}

p1←{(-⍺) rn {∧/⊃>/1 1∘⊂⌽⍵}¨(2⍴⊂1+⍳¯2+≢⍵)⌷,\(⍺rn⍵)}
p1_ans←⎕←(¯4+4×≢input)++/,0<⊃+/{⍵ p1 input}¨¯1+⍳4

p2←{(-⍺) rn 0,{s←⊃≥/⌽1 1∘⊂⌽⍵⋄0=⍴⍸s:≢s⋄⊃⍸s}¨(⍳≢⍵)(1+⍳¯1+≢⍵)⌷,\(⍺ rn ⍵)}
p2_ans←⎕←⌈/,⊃×/{⍵ p2 input}¨¯1+⍳4
