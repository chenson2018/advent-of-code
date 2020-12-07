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

   def opcode(self, value):
      full = str(value)
      code = int(full[-2:])

      try:
         p1 = int(full[-3])
      except IndexError:
         p1 = 0
      try:
         p2 = int(full[-4])
      except IndexError:
         p2 = 0
      try:
         p3 = int(full[-5])
      except IndexError:
         p3 = 0

      return code, p1, p2, p3

   def get_mode(self, address, mode):
      try:
         if mode == 0:
            return self.ref(address)
         elif mode == 1:
            return self.get(address)
         else:
            raise NotImplementedError
      except IndexError:
         return None

   def get(self, address):
      return self.memory[address]    

   def ref(self, address):
       return self.memory[self.get(address)]

   def step(self):
      opcode, p1, p2, p3 = self.opcode(self.get(self.pointer))

      p1_val = self.get_mode(self.pointer+1, p1)
      p2_val = self.get_mode(self.pointer+2, p2)

      if opcode == 1:
         self.update(address= self.get(self.pointer+3),                     
                     value = p1_val + p2_val)
         self.increment(4)

      elif opcode == 2:
         self.update(address= self.get(self.pointer+3),
                     value  = p1_val * p2_val)
         self.increment(4)

      elif opcode == 3:
         user_input = int(input("Enter value: "))
         self.update(self.get(self.pointer+1) , user_input)
         self.increment(2)

      elif opcode == 4:
         print(p1_val)
         self.increment(2)

      elif opcode == 5:
         if (p1_val != 0):
            self.pointer = p2_val
         else:
            self.increment(3)

      elif opcode == 6:
         if (p1_val == 0):
            self.pointer = p2_val
         else:
            self.increment(3)

      elif opcode == 7:
         if ( p1_val < p2_val ):
            self.update(address= self.get(self.pointer+3), value = 1)
         else:
            self.update(address= self.get(self.pointer+3), value = 0)
         self.increment(4)

      elif opcode == 8:
         if ( p1_val == p2_val ): 
            self.update(address= self.get(self.pointer+3), value = 1)
         else:
            self.update(address= self.get(self.pointer+3), value = 0)
         self.increment(4)

      else:
         raise NotImplementedError 

   def process(self):
      while self.memory[self.pointer] != 99:
         self.step()

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.read().split(',')
      input_list = list(map(int, input_list))

   #give inputs of 1/5 for diagnostic code
   program = intcode(input_list)
   program.process()
