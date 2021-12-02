all: data-asm assemble-data link-data

data-asm: data.sh
	./data.sh

assemble-data: data.asm
	ca65 data.asm -o build/data.o

link-data: build/data.o build/apple2bin.cfg
	ld65 -o build/data#066000.bin -C build/apple2bin.cfg build/data.o
