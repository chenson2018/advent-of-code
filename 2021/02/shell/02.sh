# Using gawk, the GNU implementation of awk, for some regex

gawk 'match($0, /(.*) ([0-9]+)/, regex) {

  command = regex[1]
  x       = regex[2]

  if (command == "forward") {
    horizontal += x
  } 
  else if (command == "down") {
    depth += x
  } 
  else if (command == "up") {
    depth -= x
  } else {
    exit 1
  } 
}
END {
  print "Part 1 answer: " horizontal*depth
}' ../input.txt

gawk 'match($0, /(.*) ([0-9]+)/, regex) {

  command = regex[1]
  x       = regex[2]

  if (command == "forward") {
    horizontal += x;
    depth+=(aim * x)
  } 
  else if (command == "down") {
    aim += x
  } 
  else if (command == "up") {
    aim -= x
  }
}
END {
  print "Part 2 answer: " horizontal*depth
}' ../input.txt


