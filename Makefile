NAME=hours.prg
SRC=$(wildcard *.asm)
$(NAME): $(SRC)
	cl65 -o $@ -C link.config $^ 
test:
	xvic -memory none -ntsc -cartA $(NAME)
clean:
	rm *.o $(NAME)
