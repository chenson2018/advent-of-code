input←⊃⎕NGET '../input.txt' 1
grid←6 50⍴0

parse_rect←{((⍳¨(⍎⊃'rect (\d+)x(\d+)'⎕S'\2 \1'⊢⍵))⌷grid)←1}
parse_col←{v←⍎⊃'rotate column x=(\d+) by (\d+)'⎕S'\1 \2'⊢⍵⋄grid[;1+⊃v]←(-⊃⌽v)⌽grid[;1+⊃v]⋄}
parse_row←{v←⍎⊃'rotate row y=(\d+) by (\d+)'⎕S'\1 \2'⊢⍵⋄grid[1+⊃v;]←(-⊃⌽v)⌽grid[1+⊃v;]⋄}

parse←{+/'rect'⍷⍵:parse_rect ⍵⋄+/'column'⍷⍵:parse_col ⍵⋄+/'row'⍷⍵:parse_row ⍵}

parse¨input

⎕←p1_ans←+/,grid
⎕←p2_ans←('■'@{1=⍵} grid)
