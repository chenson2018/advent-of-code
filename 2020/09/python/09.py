#!/usr/bin/env python
from itertools import combinations
from itertools import product

#find first invalid entry with preamble size p
def invalid(input_list, p):
   for i in range(p, len(input_list)):
      #window of size p
      previous = input_list[i-p:i]
      n = input_list[i]

      #combinations of length 2
      combos = [sum(combo) for combo in combinations(previous, 2)]

      #return non-match
      if n not in combos:
         return n     

def sum_to(input_list, n):
   size = len(input_list)

   #for each index, iterate through sequences starting there
   for (win, index) in product(range(size), repeat = 2):
      #exclude the singleton match
      part = input_list[index:index+2+win]

      if sum(part) == n:
         return min(part)+max(part)

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().split()
      input_list = [int(x) for x in input_list]

   p1 = invalid(input_list, 25)
   p2 = sum_to(input_list, p1)

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
