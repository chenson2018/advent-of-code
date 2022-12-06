input←⊃⊃⎕NGET'../input.txt'1
f←{¯1+⍺+⊃⍸⍺=(≢∪)¨(⌈¯1+⍺÷2)⌽↓(⊢⌺⍺)⍵}
p1_ans←⎕← 4 f input
p2_ans←⎕←14 f input

⍝ alternate and better idea from u/jaybosamiya on Reddit w/o having to fight Stencil's whitespace, is to replace the Stencil/Rotate before the map with just:
⍝ ⍺,/⍵
⍝ I often forget that you can "chunk" reduce in this way.
⍝ I'll leave the above because I don't want to plagiarize
