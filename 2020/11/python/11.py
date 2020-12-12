#!/usr/bin/env python
import numpy as np

#get value from valid index
def arr_get(arr, row, col):
    nrow, ncol = arr.shape
    
    if (row < 0 or row >= nrow or col < 0 or col >= ncol):
        return None
    else:
        return arr[row][col]

#line of sight for part 2, return after one iteration for part 1
def line_of_sight(arr, row, col, row_inc, col_inc, mode):
    while True:
        row+=row_inc
        col+=col_inc
        ret=arr_get(arr, row, col)
        if ret in ['L', '#', None] or mode == 'p1':
            return ret

#count of occupied in line of sight for all directions    
def occupied(arr, row, col, mode):
    l = [line_of_sight(arr, row, col, -1,  0, mode),
         line_of_sight(arr, row, col,  1,  0, mode),
         line_of_sight(arr, row, col,  0,  1, mode),
         line_of_sight(arr, row, col,  0, -1, mode),
         line_of_sight(arr, row, col, -1,  1, mode),
         line_of_sight(arr, row, col, -1, -1, mode),
         line_of_sight(arr, row, col,  1,  1, mode),
         line_of_sight(arr, row, col,  1, -1, mode)]
    
    return l.count('#')

#get next state
def life(arr, row, col, mode, tolerance):
    count   = occupied(arr, row, col, mode)
    current = arr[row][col]
    
    if (current == 'L' and count == 0):
        return '#'
    elif (current == '#' and count >= tolerance):
        return 'L'
    else:
        return current

#next state for entire array    
def life_iter(arr, mode, tolerance):
    index = np.ndindex(*arr.shape)
    tmp = []
    for shape in index:
        row, col = shape
        tmp.append(life(arr, row, col, mode, tolerance))
    return np.array(tmp).reshape((arr.shape))

#apply life_iter until a repeat array
def general(arr, mode, tolerance):
    res = [arr]
    while True:
        arr = life_iter(arr, mode, tolerance)
        if np.array_equal(res[-1], arr):
            break
        else:
            res.append(arr)
        
    return dict(zip(*np.unique(res[-1], return_counts=True)))['#']

if __name__ == '__main__':
    with open('../input.txt', 'r') as f:
       input_list = f.read().split()
       input_list = np.array([list(x) for x in input_list])


    p1 = general(input_list, mode = 'p1', tolerance = 4)
    p2 = general(input_list, mode = None, tolerance = 5)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
