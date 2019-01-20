SRC=$(wildcard *.asm)
biro.prg: $(SRC)
	cl65 -o $@ -C link.config $^ 
test:
	xvic -memory none -ntsc biro.prg
clean:
	rm *.o biro.prg
