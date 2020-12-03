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
      self.visited     = []

   def move(self, instruction):
      #get turn and distance
      rot   = instruction[0]
      dist  = int(instruction[1:])

      #mod 4 got compass directions
      self.orientation += self.rotate[rot]
      self.orientation %= 4

      #add multiple of unit array
      for i in range(1, dist+1):
         #using a list so we can use "in" finding repeats
         #record positions between instructions
         visit = list(self.position + i*self.direction[self.orientation])
         self.visited.append(visit)

      self.position += dist*self.direction[self.orientation]

   def hamming(self):
      dist = sum(np.abs(self.position))
      return(dist)

   def first_repeat(self):
      seen = self.visited
      for index, _ in enumerate(self.visited):
         current  = self.visited[index]
         previous = self.visited[:index]
         if current in previous:
            return(current)

if __name__ == "__main__":
   #read input to integer list
   with open("../input.txt", 'r') as f:
      input_list = f.read().strip().split(', ')

   #execute instructions
   nav  = compass()
   for instruction in input_list:
      nav.move(instruction)
      
   print(f"Part 1 answer: {nav.hamming()}")
   print(f"Part 2 answer: {sum(nav.first_repeat())}")
