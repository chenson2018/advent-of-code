#!/usr/bin/env python 
from itertools import combinations
from functools import reduce
from operator  import mul

def summation(input_list, n, goal):
    #combinations of n numbers that sum to goal
    groups     = combinations(input_list, n)
    select     = filter(lambda x: sum(x) == goal, groups)

    #multiply groups matching goal summation
    answer     = [reduce(mul, x) for x in select]
    
    return list(answer)

if __name__ == "__main__":

    #read input to integer list
    with open("../input.txt", 'r') as f:
        input_list = f.read().splitlines() 
        input_list = list(map(int, input_list))

    p1 = summation(input_list, 
                   n = 2, 
                   goal = 2020)

    p2 = summation(input_list, 
                   n = 3, 
                   goal = 2020)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
