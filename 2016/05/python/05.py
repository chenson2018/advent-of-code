#!/usr/bin/env python
from hashlib import md5

def hashword(door, index):
   in_str = door + str(index)
   in_str = bytes(in_str, 'utf-8')
   return md5(in_str).hexdigest()


def passwd_p1(door):
   passwd = []
   index = 0
   
   while len(passwd) < 8:
      h = hashword(door, index)
      if h[:5] == '00000':
         passwd.append(h[5])
      index+=1

   return ''.join(passwd)


def passwd_p2(door):
   #starting index at -1 to increment at front of loop
   index = -1
   passwd = [None]*8

   while None in passwd:
      index+=1
    
      h     = hashword(door, index)
      check = h[:5]
      pos   = h[5]
      value = h[6]
 
      if not pos.isdigit():
         next
      else:
         pos = int(pos)
    
      if check == '00000' and pos in range(8) and passwd[pos] is None:
         passwd[pos] = value
    
   return''.join(passwd)

if __name__ == '__main__':
   door = 'ffykfhsq'

   p1 = passwd_p1(door)
   p2 = passwd_p2(door)

   print(f"Part 1 answer: {p1}")
   print(f"Part 2 answer: {p2}")
