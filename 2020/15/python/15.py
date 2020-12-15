#!/usr/bin/env python

def memory_game(input_list, search):
    #indexing from 1, a mortal sin
    d = { num:index for index, num in enumerate(input_list, 1) } 

    #start with the final starting number
    current = input_list[-1]
    index   = len(input_list)

    while index <= search:
        previous   = d.get(current)
        d[current] = index
        current    = index-previous if previous else 0   
        index+=1
    
    #get the key that has the search index as a value
    return list(d.keys())[list(d.values()).index(search)]

if __name__ == '__main__':
    input_list = [14,3,1,0,9,5]

    p1 = memory_game(input_list, 2020)
    p2 = memory_game(input_list, 30000000)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
