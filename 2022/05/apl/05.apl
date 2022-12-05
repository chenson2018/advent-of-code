⍝ lmao at this parsing
raw←⊃⎕NGET'../input.txt'1
init←{(~⍵∊' ')/⍵}¨↓(⊂¯2+4×⍳9)⌷⍉↑(⊂⍳8)⌷raw
ins←{⊃⍎¨'move (\d+) from (\d+) to (\d+)'⎕S'\1 \2 \3'⊢⍵}¨{(10<⍳⍴⍵)⊂⍵}raw

split←{s←(1+⍺<⍳⍴⍵)⊆⍵⋄1=≢s:s,(⊂'')⋄s}
foldl←{⎕ML←1⋄↑⍺⍺⍨/(⌽⍵),⊂⍺}

crane←{
    move from to←⍵
    add left←move split (⊃from⌷⍺)
    add←⍺⍺ add
    ({⊂add,⊃⍵}@to) ((⊂left)@from) ⍺
 }

p1_ans←⎕←⊃¨init (⌽ crane) foldl ins
p2_ans←⎕←⊃¨init (⊢ crane) foldl ins
