raw←⊃⎕NGET'../input.txt'1
v←(+⌿⍎¨)¨((≢¨)⊆⊢)raw
p1_ans←⎕←⌈/v
p2_ans←⎕←+/(3↑⍒v)⌷¨⊂v
