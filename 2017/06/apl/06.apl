x←5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6

iter←{max←⌈/⍵⋄wmax←⊃⍸⍵=max⋄ncol←⍴⍵⋄mrow←⌈2+(+/⍵)÷ncol⋄add←(mrow,ncol)⍴(wmax⍴0),(max⍴1),((mrow×⍴⍵)⍴0)⋄((0@wmax)⍵)++⌿add}
until←{res←(⊂ iter (⊃⍵)),⍵⋄(∪res)≢res:res⋄until res}
rep←until (⊂x)

⎕←p1_ans←¯1+⍴rep
⎕←p2_ans←--/⍸(⊂⊃rep)≡¨rep
