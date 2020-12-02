#!/usr/bin/env python
from itertools import repeat, chain

def duplicate(input_list):
    seen = set()
    state = 0
    path = chain(*(repeat(input_list, 1000)))
   
    for freq in path:
        state += freq
        if state in seen:
            return state
        else:
            seen.add(state)

if __name__ == "__main__":
    with open("../input.txt", 'r') as f:
        input_list = f.read().splitlines()
        input_list = [int(num.lstrip('+')) for num in input_list]       

    p1 = sum(input_list)
    p2 = duplicate(input_list)
 
    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
