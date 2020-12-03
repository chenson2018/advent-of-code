#!/usr/bin/env python
import numpy as np
import itertools

#get coordinates of a square list
def coordinates(length):
   grid = list(itertools.product(list(range(length)), repeat=2))
   return grid

#class representing a keypad
#takes layout as input, assumes square shape
#assumes that zero encodes an edge with a square grid
class keypad:
   def __init__(self, start, keys):
      self.row, self.col = start
      self.keys = keys
      #interpret instructions
      self.dir = {'U':(-1,  0), 
                  'D':( 1,  0), 
                  'L':( 0, -1), 
                  'R':( 0,  1)}
      #grid boundaries
      self.valid = coordinates(len(self.keys))
   def move(self, instruction):
      move_row, move_col = self.dir[instruction]

      new_row = self.row+move_row
      new_col = self.col+move_col

      if ( (new_row, new_col) in self.valid and
           self.keys[new_row][new_col] != 0):
         self.row=new_row
         self.col=new_col
   def passwd(self, input_list):
      code = []
      for line in input_list:
         [self.move(ins) for ins in line]
         code.append(self.keys[self.row][self.col])
      self.code = code

if __name__ == "__main__":
   with open("../input.txt", 'r') as f:
      input_list = f.readlines()
      input_list = [line.strip() for line in input_list]

   p1_keys = [[1, 2, 3],
              [4, 5, 6],
              [7, 8, 9]]
   pad = keypad(start = (0,0),
                keys  = p1_keys)
   pad.passwd(input_list)
   print(f"Part 1 answer: {pad.code}")

   p2_keys = [[0,  0 ,  1 ,  0 , 0],
              [0,  2 ,  3 ,  4 , 0],
              [5,  6 ,  7 ,  8 , 9],
              [0, 'A', 'B', 'C', 0],
              [0,  0 , 'D',  0 , 0]]
   pad = keypad(start = (0,2),
                keys  = p2_keys)
   pad.passwd(input_list)
   print(f"Part 2 answer: {pad.code}")
