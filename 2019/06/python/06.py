#!/usr/bin/env python

#orbit path from starting planet
def get_path(orbit_dict, planet):
   travel = [planet]
    
   while planet != 'COM':
      planet = orbit_dict[planet]
      travel.append(planet)
        
   return travel

#get distance to travel between planets
def dist_transfer(planet_1, planet_2, orbit_dict):
   t1 = get_path(orbit_dict, planet_1)
   t2 = get_path(orbit_dict, planet_2)
       
   for index, planet in enumerate(t1):
      if planet in t2:
         #dist to intersect plus dist from intersect to goal
         return len(t1[:index]) + len(t2[:t2.index(t1[index])])

if __name__ == '__main__':
   with open('../input.txt', 'r') as f:
      input_list = f.readlines()
      input_list = [x.rstrip().split(')') for x in input_list]

   orbit_dict = {orbiter:orbitee for (orbitee, orbiter) in input_list}
   planets    = list(set(orbit_dict.keys()))

   paths = [get_path(orbit_dict, planet) for planet in planets]

   #we want orbits, so subtract endpoints
   p1 = sum(map(len, paths)) - len(paths)

   #distance between objects that endpoints orbit
   p2 = dist_transfer(orbit_dict['YOU'], 
                      orbit_dict['SAN'], 
                      orbit_dict)

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")      
