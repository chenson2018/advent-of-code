input←⊃⊃⎕NGET'../input.txt'1
f←{¯1+⍺+⊃⍸⍺=(≢∪)¨(⌈¯1+⍺÷2)⌽↓(⊢⌺⍺)⍵}
p1_ans←⎕← 4 f input
p2_ans←⎕←14 f input
