#!/usr/bin/env python

from lib import crt
from functools import reduce
import re

if __name__ == '__main__':
  with open('../input.txt', 'r') as f:
    input_list = f.readlines()

  pattern = "Disc #[0-9]+ has ([0-9]+) positions; at time=0, it is at position ([0-9]+)."
  regex = [ re.search(pattern, line) for line in input_list]
  init = [(int(r.group(2)), int(r.group(1))) for r in regex]   

  def solve_cong(xs):
    congs = [(-disc[0]-i-1, disc[1]) for i, disc in enumerate(xs)]
    return reduce(crt, congs)[0]

  p1 = solve_cong(init)
  init.append((0, 11))
  p2 = solve_cong(init)

  print(f"Part 1 answer: {p1}")
  print(f"Part 2 answer: {p2}")
