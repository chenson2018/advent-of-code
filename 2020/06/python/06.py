#!/usr/bin/env python
from itertools import groupby

def any_vote(group):
   combined = ''.join(group)
   return len(set(combined))

def all_vote(group):
   #combined vote string in alpha order
   combined = sorted(''.join(group))

   #votes for each item
   lengths = [len(''.join(g)) for _, g in groupby(combined)]

   #count items with as many votes as group members
   unanimous = lengths.count(len(group))

   return unanimous

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().split("\n\n")
      input_list = [group.split() for group in input_list]

   p1 = sum(map(any_vote, input_list))
   p2 = sum(map(all_vote, input_list))

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
