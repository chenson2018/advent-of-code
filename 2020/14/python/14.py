#!/usr/bin/env python
import re
from itertools import product

#convert integer to 36 bit binary string
def bit_36(num):
    return '{0:036b}'.format(num)


#parse instructions as string for mask or tuple for assignment
def parse(instruction):
    if 'mask' in instruction:
        return re.sub('mask = ', '', instruction)
    else:
        regex   = re.match("^mem\[(\d+)\] = (\d+)", instruction)
        address = int(regex.group(1))
        value   = int(regex.group(2))
        return (address, value)

#mask for part 1
def mask_p1(value, mask):
    res = []
    for i in range(len(mask)):
        if mask[i] == 'X':
            res.append(value[i])
        else:
            res.append(mask[i])
    
    return int(''.join(res), 2)

#mask for part 2
def mask_p2(value, mask):
    floating = []
    for i in range(len(mask)):
        if mask[i] == '0':
            floating.append(value[i])
        elif mask[i] == '1':
            floating.append('1')
        elif mask[i] == 'X':
            floating.append('X')
            
    floating = ''.join(floating)
    
    #get all possible combinations of replacing floating X with 0/1
    replace  = product(('0', '1'), repeat = floating.count('X'))
    res = []
    
    for option in replace:
        working = floating
        for bit in option:
            working = working.replace('X', bit, 1)
        res.append(working)    
    
    res = [int(x, 2) for x in res]
    
    return res

def p1_execute(parsed):
    memory = dict()
    mask   = None

    for ins in parsed:
       if type(ins) == str:
          mask = ins
       elif type(ins) == tuple:
          address, value = ins
          memory[address] = mask_p1(bit_36(value), mask)
        
    return sum(memory.values())

def p2_execute(parsed):
    memory = dict()
    mask   = None

    for ins in parsed:
       if type(ins) == str:
          mask = ins
       elif type(ins) == tuple:
          address, value = ins
          rewrite = mask_p2(bit_36(address), mask)

          for add in rewrite:
              memory[add] = value
        
    return sum(memory.values())

if __name__ == '__main__':
    with open('../input.txt', 'r') as f:
       input_list = f.readlines()
       input_list = [x.strip() for x in input_list]
    
    parsed = list(map(parse, input_list))

    p1 = p1_execute(parsed)
    p2 = p2_execute(parsed)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
