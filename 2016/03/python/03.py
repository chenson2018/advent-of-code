#!/usr/bin/env python
import numpy as np
import re

def triangle(sides):
   a, b, c = sorted(sides)
   return a+b>c

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().splitlines() 
      input_list = [re.findall("\d+", line) for line in input_list]
      input_list = [list(map(int, line))    for line in input_list]

   p1 = sum(map(triangle, input_list))

   #reshape to use columns
   input_list  = np.array(input_list)
   input_list  = input_list.T.flatten().reshape((-1, 3))
   p2 = sum(map(triangle, input_list))

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
