#!/usr/bin/env python

import numpy as np

class ship:
    def __init__(self):
        self.position = np.array([0, 0])
        
        #orientation of ship, only for part one
        #numeric 0-3 for NESW
        self.orientation = 1
        
        #waypoint position, only for part 2
        self.way = np.array([10, 1])
        
        #dicts for translating commands
        self.compass = {'N':0, 'E':1, 'S':2, 'W':3}       
        self.sign = {'L':-1, 'R':1, 'F':0}
        
        #unit vectors for compass directions
        self.direction = {0:np.array([ 0,  1]),
                          1:np.array([ 1,  0]),
                          2:np.array([ 0, -1]),
                          3:np.array([-1,  0])}

    def parse(self, instruction):
        return instruction[0], int(instruction[1:])
        
    def move_p1(self, instruction):
        code, value = self.parse(instruction)
        
        if code == 'F':
            self.position+=self.direction[self.orientation]*value
        elif code in self.compass.keys():
            self.position += self.direction[self.compass[code]]*value  
        elif code in self.sign.keys():
            self.orientation += self.sign[code]*(value // 90)
            self.orientation %= 4
        else:
            raise NotImplementedError
            
    def move_p2(self, instruction):
        code, value = self.parse(instruction)
        
        if code == 'F':
            self.position+= value*self.way
        elif code in self.compass.keys():
            self.way+=value*self.direction[self.compass[code]]
        elif code in self.sign.keys():
            if code=='R':
                value=360-value
            if value==90:
                self.way=np.flip(self.way)*np.array([-1, 1])
            elif value==180:
                self.way=self.way*np.array([-1, -1])          
            elif value==270:
                self.way=np.flip(self.way)*np.array([1, -1])
            else:
                raise NotImplementedError
                
    def hamming(self):
        dist = sum(np.abs(self.position))
        return(dist)


if __name__ == '__main__':
    with open('../input.txt', 'r') as f:
       input_list = f.read().split()
    
    p1 = ship()
    p2 = ship()

    for ins in input_list:
       p1.move_p1(ins)
       p2.move_p2(ins)

    print(f"Part 1 answer: {p1.hamming()}")
    print(f"Part 2 answer: {p2.hamming()}")
