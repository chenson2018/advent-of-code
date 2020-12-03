#!/usr/bin/env python
import numpy as np

class compass:
   def __init__(self):
      #coordinate position
      self.position = np.array([0, 0])
      #encoding for direction
      self.rotate    = {'L':-1, 'R':1}
      self.direction = {0:np.array([ 0,  1]), 
                        1:np.array([ 1,  0]),
                        2:np.array([ 0, -1]),
                        3:np.array([-1,  0])}
      self.orientation = 0
   def move(self, instruction):
      #get turn and distance
      rot   = instruction[0]
      dist  = int(instruction[1:])

      #mod 4 got compass directions
      self.orientation += self.rotate[rot]
      self.orientation %= 4

      #add multiple of unit array
      self.position += dist*self.direction[self.orientation]
   def hamming(self):
      dist = sum(np.abs(self.position))
      return(dist)

if __name__ == "__main__":
   #read input to integer list
   with open("../input.txt", 'r') as f:
      input_list = f.read().strip().split(', ')

   nav  = compass()
   for instruction in input_list:
      nav.move(instruction)
      
   print(f"Part 1 answer: {nav.hamming()}")
