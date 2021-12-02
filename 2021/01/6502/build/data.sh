# This is used to generate some assembly to load our data

awk '{printf "%04x\n", $1}' ../input.txt | 
#awk '{printf "%04x\n", $1}' test.txt | 
gawk -v DATA_ADR=24832 'match($0, /(..)(..)/, h)  { 
  printf "LDA #$%s\n",  h[1];
  printf "STA $%04x\n", DATA_ADR;
  print "";
  printf "LDA #$%s\n",  h[2];
  printf "STA $%04x\n", (DATA_ADR+1);
  print "";
} {DATA_ADR+=2} {count+=1} END {print "RTS"}' |
awk '{ print toupper($0) }' > data.asm
