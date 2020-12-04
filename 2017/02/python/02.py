#!/usr/bin/env python

#multiply each number by successive integers
#continue until we find a match for exceed max in list
def find_divisors(line):
    for num in line:
        div = 2
        while (div*num <= max(line)):
            prod = num*div
            if prod in line:
                return div
            else:
                div+=1

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      #read into [[int]]
      input_list = f.read().splitlines() 
      input_list = [line.split() for line in input_list]
      input_list = [list(map(int, line)) for line in input_list]

   p1 = sum([max(x) - min(x) for x in input_list])
   p2 = sum(map(find_divisors, input_list))

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
