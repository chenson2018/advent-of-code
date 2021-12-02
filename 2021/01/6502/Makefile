all: data-asm assemble-data link-data assemble-sol link-sol

data-asm: build/data.sh
	./build/data.sh

assemble-data: data.asm
	ca65 data.asm -o build/data.o

link-data: build/data.o build/apple2bin.cfg
	ld65 -o build/data#066000.bin -C build/apple2bin.cfg build/data.o

assemble-sol: p01.asm
	ca65 p01.asm -o build/p01.o

link-sol: build/p01.o build/apple2bin.cfg
	ld65 -o build/p01#066000.bin -C build/apple2bin.cfg build/p01.o
