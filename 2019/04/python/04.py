#!/usr/bin/env python
from itertools import groupby

def valid(passwd, mode):
   #check adjacent characters
   non_decrease = all([a <= b for a, b in zip(passwd, passwd[1:])])

   #get repeated characters
   lengths = [len(''.join(g)) for _, g in groupby(passwd)]

   if mode == 'p1':
      repeat = max(lengths) >= 2
   elif mode == 'p2':
      repeat = 2 in lengths
   else:
      raise ValueError

   return (repeat and non_decrease)

if __name__ == '__main__':
   input_list = map(str, range(264360, 746325+1))
   input_list = list(input_list)

   p1 = [passwd for passwd in input_list if valid(passwd, 'p1')]
   p2 = [passwd for passwd in input_list if valid(passwd, 'p2')]

   print(f"Part 1 answer: {len(p1)}")
   print(f"Part 2 answer: {len(p2)}")
