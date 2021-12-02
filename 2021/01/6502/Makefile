all: data-asm assemble-data link-data

data-asm: data.sh
	./data.sh

assemble-data: data.asm
	ca65 data.asm

link-data: data.o apple2bin.cfg
	ld65 -o data#066000.bin -C apple2bin.cfg data.o
