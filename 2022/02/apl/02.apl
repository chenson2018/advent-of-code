a ←'ABC'                                ⍝ useful constants
x ←'XYZ'

raw←{(~⍵∊' ')/⍵}¨⊃⎕NGET'../input.txt'1  ⍝ read input and remove the spaces
p1_interp←{({(⍸⍵⍷x)⌷a}@2) ⍵}¨raw        ⍝ part 1 data interpretation

win←{⍺=⍵:3⋄(⊂⍺,⍵)∊↓(⊢⌺2)4⍴a:6⋄0}        ⍝ component of score for which player won
score←{(⍸⍵⍷a)+⍺ win ⍵}                  ⍝ total score, adding in points for action used
calc←{+⌿{(⊃score/⍺)×⍴⍵}⌸↑⍵}             ⍝ for input, count how many of each of 9 possible games and score

p1_ans←⎕←calc p1_interp

weak←{(⍸⍵⍷a)⌷1⌽a}                       ⍝ get weakness 
pick←{(weak⍣(¯1+⊃⍸⍵⍷1⌽x))⍺}             ⍝ to interpret an X/Y/Z, apply weak as many times as needed

p2_interp←{((pick/⍵)@2)⍵}¨raw           ⍝ part 2 data interpretation
p2_ans←⎕←calc p2_interp

⍝ A brief explanation of how this works.
⍝
⍝ We start by reading the file, removing the space from each line:
⍝ 
⍝ raw←{(~⍵∊' ')/⍵}¨⊃⎕NGET'../input.txt'1
⍝ 
⍝                  ⊃⎕NGET'../input.txt'1   ⍝ get content from file
⍝ 		  ¨                        ⍝ apply function to each entry
⍝     {(~⍵∊' ')/⍵}                         ⍝ function that...
⍝              /⍵                              ⍝ applys boolean mask to function right argument ⍵
⍝       ~⍵∊' '                                 ⍝ negation of containing space
⍝ 
⍝ Which gives us:
⍝ 
⍝ ┌→─────────────────────────
⍝ │ ┌→─┐ ┌→─┐ ┌→─┐ ┌→─┐ ┌→─┐ 
⍝ │ │AX│ │AX│ │BY│ │BY│ │AY│ 
⍝ │ └──┘ └──┘ └──┘ └──┘ └──┘  ....
⍝ └∊─────────────────────────
⍝ 
⍝ For part 1, we interpret X/Y/Z as equivalent to A/B/C, just a simple mapping:
⍝ 
⍝ p1_interp←{({(⍸⍵⍷x)⌷a}@2) ⍵}¨raw
⍝ 
⍝                             ¨raw     ⍝ map each entry from the input data
⍝           {                }         ⍝ function that...
⍝ 	                  ⍵             ⍝ takes right argument ⍵ (X/Y/Z)
⍝                       @2                ⍝ replace the second element using
⍝             {        }                  ⍝ function that...
⍝                    ⌷a                	   ⍝ selects an index from 'ABC'
⍝ 	       ⍸⍵⍷x	                   ⍝ corresponding to the index of argument ⍵ in 'XYZ'
⍝ 
⍝ 
⍝ Which gives us:
⍝ 
⍝ ┌→─────────────────────────
⍝ │ ┌→─┐ ┌→─┐ ┌→─┐ ┌→─┐ ┌→─┐ 
⍝ │ │AA│ │AA│ │BB│ │BB│ │AB│ 
⍝ │ └──┘ └──┘ └──┘ └──┘ └──┘  ...
⍝ └∊─────────────────────────
⍝ 
⍝ Next we need functions for calculating the score. Here the left arg ⍺ is our opponent, right arg ⍵ is us. First, a function to determine the winner:
⍝ 
⍝ win←{⍺=⍵:3⋄(⊂⍺,⍵)∊↓(⊢⌺2)4⍴a:6⋄0}
⍝ 
⍝     {                          }   ⍝ function that...
⍝      ⍺=⍵:3                         ⍝ returns 3 if arguments are equal
⍝                            :6      ⍝ return 6 if...
⍝ 			4⍴a           ⍝ reshape 'ABC' to length 4, constructing 'ABCA'
⍝ 	          ↓(⊢⌺2)              ⍝ use stencil of length 2 to get ('AB' 'BC' 'CA'), the winning possibilities
⍝            (⊂⍺,⍵)∊                    ⍝ check if our inputs are within these
⍝                              ⋄0    ⍝ returns 0 otherwise
⍝ 
⍝ and the overall score:
⍝ 
⍝ score←{(⍸⍵⍷a)+⍺ win ⍵}
⍝ 
⍝       {              }    ⍝ function that...
⍝               ⍺ win ⍵     ⍝ apply function win
⍝ 	     +            ⍝ added to...
⍝        (⍸⍵⍷a)             ⍝ index of our action in 'ABC'
⍝ 
⍝ 
⍝ Now we apply this to all of our inputs. To reduce calculations, I use ⌸, which can count how many of each possible of the 9 games occurs. For example we have
⍝ 
⍝       {(⊃⍺) (⍴⍵)}⌸p1_interp
⍝ 
⍝ ┌→────────────┐
⍝ ↓ ┌→─┐ ┌→───┐ │
⍝ │ │AA│ │1162│ │
⍝ │ └──┘ └~───┘ │
⍝ │ ┌→─┐ ┌→──┐  │
⍝ │ │BB│ │474│  │
⍝ │ └──┘ └~──┘  │
⍝ │ ┌→─┐ ┌→──┐  │
⍝ │ │AB│ │167│  │
⍝ │ └──┘ └~──┘  │
⍝ │ ┌→─┐ ┌→──┐  │
⍝ │ │CA│ │270│  │
⍝ │ └──┘ └~──┘  │
⍝ │ ┌→─┐ ┌→──┐  │
⍝ │ │CC│ │194│  │
⍝ │ └──┘ └~──┘  │
⍝ │ ┌→─┐ ┌→─┐   │
⍝ │ │CB│ │25│   │
⍝ │ └──┘ └~─┘   │
⍝ │ ┌→─┐ ┌→──┐  │
⍝ │ │AC│ │117│  │
⍝ │ └──┘ └~──┘  │
⍝ │ ┌→─┐ ┌→─┐   │
⍝ │ │BA│ │44│   │
⍝ │ └──┘ └~─┘   │
⍝ │ ┌→─┐ ┌→─┐   │
⍝ │ │BC│ │47│   │
⍝ │ └──┘ └~─┘   │
⍝ └∊────────────┘
⍝ 
⍝ So the compute all the scores we use:
⍝ 
⍝ calc←{+⌿{(⊃score/⍺)×⍴⍵}⌸↑⍵}
⍝ 
⍝      {                    }    ⍝ function that...
⍝         {             }⌸↑⍵     ⍝ creates a table from input ⍵ that...
⍝   	            ⍴⍵             ⍝ takes the length of counts
⍝ 		   ×               ⍝ multiplied by
⍝ 	   ⊃score/⍺                ⍝ scoring applied to inputs
⍝       +⌿                       ⍝ add results
⍝
⍝ Here you can see why I removed the whitespace. By having something like 'AC' with length two, I can reduce over this instead of having to split apart into two pieces and pass individually to left and right args.
⍝ 
⍝ Finally, we find our answer:
⍝ 
⍝     p1_ans←⎕←calc p1_interp
⍝ 
⍝ For part 2, I need a couple of new helper functions. The first finds the weakness of a given A/B/C:
⍝ 
⍝ weak←{(⍸⍵⍷a)⌷1⌽a}             
⍝ 
⍝      {          }    ⍝ function that...
⍝              1⌽a     ⍝ rotate 'ABC' by 1 element to get 'BCA'
⍝ 	    ⌷        ⍝ get index
⍝       (⍸⍵⍷a)	     ⍝ index of arg ⍵ in 'ABC'
⍝ 
⍝ 
⍝ The second, given an ⍺ A/B/C, and an ⍵ X/Y/Z, interpret what X/Y/Z means:
⍝ 
⍝ pick←{(weak⍣(¯1+⊃⍸⍵⍷1⌽x))⍺}   
⍝ 
⍝      {                    }    ⍝ function that...
⍝                          ⍺     ⍝ given starting X/Y/Z...
⍝ 	              1⌽x           ⍝ rotate to get 'YZX'
⍝ 	          ⊃⍸⍵⍷              ⍝ index of ⍵ in 'YZX'
⍝              ¯1+                  ⍝ add ¯1
⍝ 	  weak⍣                  ⍝ repeat function weak this many times
⍝ 
⍝ 
⍝ Then as above, we use this to replace the second letter from each input line and calculate our answer using the same calc function:
⍝ 
⍝     p2_interp←{((pick/⍵)@2)⍵}¨raw 
⍝     p2_ans←⎕←calc p2_interp











