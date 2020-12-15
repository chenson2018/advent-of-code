#!/usr/bin/env python

def memory_game(input_list, search):
    #initialize dictionary that stores (number, index)
    #indexing from 1, a mortal sin
    d = dict(zip(input_list, range(1, len(input_list)+1)))
    
    #start with the final starting number
    current = input_list[-1]
    index   = len(input_list)

    while index <= search:
        try:
            previous   = d[current]
            d[current] = index
            current    = index-previous
        #KeyError indicates a new number
        except KeyError:
            d[current] = index
            current    = 0
        
        index+=1
    
    #get the key that has the search index as a value
    return list(d.keys())[list(d.values()).index(search)]

if __name__ == '__main__':
    input_list = [14,3,1,0,9,5]

    p1 = memory_game(input_list, 2020)
    p2 = memory_game(input_list, 30000000)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
