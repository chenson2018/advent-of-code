#!/usr/bin/env python

def fuel_single(mass):
    fuel = (mass//3) - 2
    return(fuel)

def fuel_for_fuel(mass):
    fuel_list = []
    while mass > 0:
        mass = fuel_single(mass)
        if mass <= 0: break
        fuel_list.append(mass)
        
    return(sum(fuel_list))

if __name__ == "__main__":
    with open('../input.txt', 'r') as f:
        input_list = map(int, f.readlines())
        input_list = list(input_list)
    
    p1 = sum(map(fuel_single, input_list))
    p2 = sum(map(fuel_for_fuel, input_list))

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")
