#!/usr/bin/env python

#convert to binary
def seat_id(code, lookup):
   binary = code.translate(lookup)
   return int(binary, 2)

if __name__ == "__main__":
   with open('../input.txt', 'r') as f:
      input_list = f.read().splitlines()

   #translate object
   lookup = str.maketrans({'F':'0', 
                           'B':'1', 
                           'L':'0', 
                           'R':'1'})
    
   ids = [seat_id(code, lookup) for code in input_list]

   print(f"Part 1 answer: {max(ids)}")

   #sort so we can find adjacent ids that aren't consecutive
   ids = sorted(ids)
   for index, seat in enumerate(ids):
      if seat != ids[index + 1] - 1:
         print(f"Part 2 answer: {seat + 1}")
         break
