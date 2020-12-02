#!/usr/bin/env python

#first basement entry
def enter_basement(input_str, count = 0):
    direction = {'(':1, ')':-1}
    for index, paren in enumerate(input_str):
        count += direction[paren]
        if count == -1:
            return (index + 1)
        
if __name__ == "__main__":
    with open('../input.txt', 'r') as f:
        input_str = f.read()

    p1 = input_str.count('(') - \
         input_str.count(')')

    p2 = enter_basement(input_str)

    print(f"Part 1 answer: {p1}")        
    print(f"Part 2 answer: {p2}")        
