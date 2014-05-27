all : boot.bin

boot.bin : boot.s script.ld
	as boot.s -o boot.o
	ld boot.o -T script.ld --oformat binary -o boot.bin

clean:
	rm boot.o boot.bin

.PHONY: clean
