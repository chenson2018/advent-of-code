#!/usr/bin/env python
from operator import itemgetter, mul
from functools import reduce

def wait(start, bus):
    return (bus, (start//bus+1)*bus-start)

#taken from https://bugs.python.org/issue39657
#see https://en.wikipedia.org/wiki/B%C3%A9zout%27s_theorem
def bezout(a, b):
    u1, v1, r1 = 1, 0, a
    u2, v2, r2 = 0, 1, b

    while r2:
        q = r1 // r2
        u1, u2 = u2, u1-q*u2
        v1, v2 = v2, v1-q*v2
        r1, r2 = r2, r1-q*r2

    if r1 < 0:
        u1, v1, r1 = -u1, -v1, -r1

    return (u1, v1, r1)

#see https://en.wikipedia.org/wiki/Chinese_remainder_theorem
def crt(cong1, cong2):
    a1, n1 = cong1
    a2, n2 = cong2
    c1, c2, g = bezout(n1, n2)
    assert n1*c1 + n2*c2 == g
    
    if (a1 - a2) % g != 0:
        raise ValueError(f"Incompatible congruences {cong1} and {cong2}.")
    
    lcm = n1 // g * n2
    rem = (a1*c2*n2 + a2*c1*n1)//g
    return rem % lcm, lcm

if __name__ == '__main__':
    with open('../input.txt', 'r') as f:
       input_list = f.read().split()

    start = int(input_list[0])
    availible = [int(x) if x != 'x' else x for x in input_list[1].split(',') ]

    #part 1
    specified = [x for x in availible if x != 'x']
    times = [wait(start, x) for x in specified]
    p1 = reduce(mul, min(times, key=itemgetter(1)))

    #part 2
    #translate each bus arrival condition into a modular equation
    mods = [(-i, num) for i, num in enumerate(availible) if num != 'x']
    #repeatedly apply Chinese Remainder Theorem
    p2 = reduce(crt, mods)[0]

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
