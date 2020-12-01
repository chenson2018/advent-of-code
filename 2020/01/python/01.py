#!/usr/bin/env python 
from itertools import combinations
from functools import reduce
from operator  import mul

def summation(file, n, goal):
    #read input to integer list
    with open(file, 'r') as f:
        input_list = f.read().splitlines() 
        input_list = map(int, input_list)

    #combinations of n numbers that sum to goal
    groups     = combinations(input_list, n)
    select     = filter(lambda x: sum(x) == goal, groups)

    #multiply groups matching goal summation
    answer     = map(lambda x: reduce(mul, x), select)
    
    return list(answer)

if __name__ == "__main__":

    p1 = summation('../input.txt', 
                   n = 2, 
                   goal = 2020)

    p2 = summation('../input.txt', 
                   n = 3, 
                   goal = 2020)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
