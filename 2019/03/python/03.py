#!/usr/bin/env python

class wire:
    def __init__(self, instructions):
        self.instructions = instructions
        self.x = 0
        self.y = 0

        #list to store locations visited
        self.path = [(self.x, self.y)]

        #dictionary for interpreting movement codes
        self.trans = {'sign':{'R':1, 'U':1, 'L':-1, 'D':-1},
                      'axis':{'R':0, 'U':1, 'L': 0, 'D': 1}}

    def step(self, code):
        direction = code[0]
        sign = self.trans['sign'][direction]
        axis = self.trans['axis'][direction]
        dist = int(code[1:])

        for _ in range(1, dist+1):
            if axis:
                self.y+=sign
            else:
                self.x+=sign
            
            self.path.append((self.x, self.y))

    #execute all codes
    def execute(self):
        [self.step(code) for code in self.instructions]

    #shared points between two wires        
    def intersect(self, other):
        return list(set(self.path) & set(other.path))

    #min Manhattan distance of intersections    
    def Manhattan(self, other):
        intersections = self.intersect(other)

        dist = [abs(x) + abs(y)               \
                   for (x, y) in intersections \
                    if not (x, y) == (0, 0)]

        return min(dist)

    #min wire length to reach an intersection    
    def signal_delay(self, other):
        intersections = self.intersect(other)
        delays = [self.path.index(point) + other.path.index(point) \
                     for point in intersections                     \
                        if not point == (0, 0)]

        return min(delays)

if __name__ == '__main__':
    with open("../input.txt") as f:
        wire1, wire2 = [wire.split(',') for wire in f.readlines()]

    wire1 = wire(wire1)
    wire1.execute()

    wire2 = wire(wire2)
    wire2.execute()

    p1 = wire1.Manhattan(wire2)
    p2 = wire1.signal_delay(wire2)

    print(f"Part 1 answer: {p1}")
    print(f"Part 2 answer: {p2}")   
