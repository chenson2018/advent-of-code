#!/usr/bin/env python
from operator  import mul
from functools import reduce

def trees(input_list, dx, dy):
   #rectangle shape
   width  = len(input_list[0])
   length = len(input_list)

   #initial position   
   x, y, count = [0]*3

   #iterate vertical length of input
   #as far right as need captured by modulus
   while (y <= length - 1):
      if input_list[y][(x % width)] == "#":
         count += 1

      x+=dx
      y+=dy

   return(count)

if __name__ == "__main__":
   with open('../input.txt', 'r') as f:
      input_list = f.read().splitlines()

   #problem 1 input
   p1 = trees(input_list, dx = 3, dy = 1)

   #problem 2 inputs
   slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   tree   = map(lambda x: trees(input_list, *x), slopes)
   p2     = reduce(mul, tree, 1)

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
