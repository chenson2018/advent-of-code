#!/usr/bin/env python

#make a second string pushing the first
#digits to the end to find matches an offset distance
def match(input_str, offset):
    ahead = input_str[offset:] + input_str[:offset]
    match = [int(i) for i, j in zip(input_str, ahead) if i == j]
    return(sum(match))

if __name__ == "__main__":
    with open("../input.txt", 'r') as f:
        input_str = f.read().strip()

    p1 = match(input_str, offset = 1)
    #the opposite side is length divided in half
    p2 = match(input_str, offset = len(input_str)//2)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")

