⍝ lmao at this parsing
raw←⊃⎕NGET'../input.txt'1
init←{(~⍵∊' ')/⍵}¨↓(⊂¯2+4×⍳9)⌷⍉↑(⊂⍳8)⌷raw
ins←{⊃⍎¨'move (\d+) from (\d+) to (\d+)'⎕S'\1 \2 \3'⊢⍵}¨{(10<⍳⍴⍵)⊂⍵}raw

split←{s←(1+⍺<⍳⍴⍵)⊆⍵⋄1=≢s:s,(⊂'')⋄s}
foldl←{⎕ML←1⋄↑⍺⍺⍨/(⌽⍵),⊂⍺}

crane←{
    move←1⌷⍵
    from←2⌷⍵
    to←3⌷⍵

    s←move split (⊃from⌷⍺)

    ({⊂((mrev⊃s),⊃⍵)}@to) ((2⌷s)@from) ⍺
 }

⍝ I want to pass mrev as ⍺⍺ or ⍵⍵ but I can't quite figure out how

mrev←⌽
p1_ans←⎕←⊃¨init crane foldl ins

mrev←⊢
p2_ans←⎕←⊃¨init crane foldl ins
