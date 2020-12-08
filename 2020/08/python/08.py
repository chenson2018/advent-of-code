#!/usr/bin/env python
from copy import deepcopy

class game:
    def __init__(self, input_list):
        self.instructions = input_list
        self.acc       = 0
        self.address   = 0
        self.visited    = []

    def step(self, instruction):        
        code, value = instruction
        self.visited.append(self.address)
        
        if code == 'nop':
            self.address+=1
        elif code == 'acc':
            self.acc+=value
            self.address+=1
        elif code == 'jmp':
            self.address+=value
        else:
            raise NotImplementedError
        
        return self.address
        
    def execute(self):
        #exit with a status code
        #1 indicates success when address surpasses instructions
        #0 indicates a loop 
        while True:
            if self.address in self.visited:
                return 0
            elif self.address == len(self.instructions):
                return 1
            else:
                self.step(self.instructions[self.address])

if __name__ == '__main__':

    with open('../input.txt', 'r') as f:
        input_list = f.read().strip().split("\n")

    #extract tuples with instruction and value
    input_list = [(val[:3], int(val[3:].replace('+', '').strip())) \
                  for val in input_list]

    #Part 1
    g = game(input_list)
    status = g.execute()
    print(f"Part 1 answer: {g.acc}")


    #Part 2
    replace = {'nop':'jmp', 'jmp':'nop'}

    #try replacing each nop/jmp until status 1
    for index, instruction in enumerate(input_list):
        code, value = instruction
        if code == 'acc':
            next
        else:
            working = deepcopy(input_list)
            working[index] = (replace[code], value)
            g = game(working)
            status = g.execute()
        
            if status:
                print(f"Part 2 answer: {g.acc}")
                break
