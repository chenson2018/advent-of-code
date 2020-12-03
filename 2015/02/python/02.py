#!/usr/bin/env python
from operator import mul
from functools import reduce

#parse and sort box sides
def parse(line):
   line = line.strip().split('x')
   line = list(map(lambda x: int(x), line))
   return sorted(line)

#use the fact that sides are sorted
def paper(sides):
   paper = (
            3*sides[0]*sides[1] +
            2*sides[0]*sides[2] +
            2*sides[1]*sides[2]
            )
   return paper

def ribbon(sides):
   ribbon = (
             2*(sides[0]+sides[1]) +
             reduce(mul, sides, 1)
            )

   return ribbon

if __name__ == "__main__":
   with open('../input.txt', 'r') as f:
      input_list = f.readlines()
      input_list = [parse(box) for box in input_list]

   p1 = sum(map(paper , input_list))
   p2 = sum(map(ribbon, input_list))

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
