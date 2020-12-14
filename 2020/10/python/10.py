#!/usr/bin/python
from numpy import diff

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().split()
      input_list = list(map(int,input_list))
      input_list.append(0)
      input_list.append(max(input_list)+3)
      input_list=sorted(input_list)

   diffs = list(diff(input_list))   
   p1    = diffs.count(1)*diffs.count(3)


   """
   Explanation of part 2

   Our input breaks into chunks that look like the following examples.
   This relies on the input having "runs" of at most 5, then a jump of 3.

   (0) 1 2 3 4 (7) 
      ->

      012347, 
      0 2347, 
      01 347, 
      012 47, 
      0  347,   
      01  47, 
      0 2 47,       

   (0) 1 2 3   (6)

      ->

      01236, 
      0 236, 
      01 36, 
      0  36      

   (0) 1 2     (5)   
      ->
      0125,
      0 25


   """

   str_rep = ''.join(map(str, diffs)).split('3')
   p2      = (7**str_rep.count('1111'))*(4**str_rep.count('111'))*(2**str_rep.count('11'))


   print(f"Part 1 answer: {p1}")
   print(f"Part 1 answer: {p2}")
