#!/usr/bin/env python
from itertools import product
from copy import deepcopy

class intcode:
   def __init__(self, init_memory):
      self.memory  = deepcopy(init_memory)
      self.pointer = 0

   def increment(self, value):
      self.pointer+=value

   def update(self, address, value):
      self.memory[address] = value

   def get(self, address):
      return self.memory[address]    

   def ref(self, address):
       return self.memory[self.get(address)]

   def step(self):
       opcode = self.memory[self.pointer]
       if opcode == 1:
         self.update(address= self.get(self.pointer+3),
                     value  = self.ref(self.pointer+2) + \
                              self.ref(self.pointer+1))
         self.increment(4)
       if opcode == 2:
         self.update(address= self.get(self.pointer+3),
                     value  = self.ref(self.pointer+2) * \
                              self.ref(self.pointer+1))
         self.increment(4)

   def process(self):
      while self.memory[self.pointer] != 99:
         self.step()

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().split(',')
      input_list = list(map(int, input_list))

   program = intcode(input_list)
   program.update(address=1, value= 12)
   program.update(address=2, value=  2)
   program.process()
   p1 = program.memory[0]
   print(f"Part 1 answer: {p1}")

   #we can also notice a pattern but brute force is relatively quick
   grid = product(range(100), repeat = 2)
   for noun, verb in grid:
      program = intcode(input_list)
      program.update(address=1, value= noun)
      program.update(address=2, value= verb)
      program.process()
      if program.memory[0] == 19690720:
         print(f"Part 2 answer: {100 * noun + verb}")
