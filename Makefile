.PHONY: all clean brick

all: ./bufo.exe ./hello.exe ./small.exe ./machine.exe ./mod.exe

./bufo.exe: $(shell find src/ -type f) $(shell find std/ -type f)
	./bufo.exe src/bufo.bufo -o ./bufo1.exe
	mv ./bufo1.exe ./bufo.exe

./mod.exe: mod.bufo
	./bufo.exe mod.bufo -o ./mod.exe

./hello.exe: hello.bufo
	./bufo.exe hello.bufo -o ./hello.exe

./small.exe: small.bufo
	./bufo.exe small.bufo -o ./small.exe

./machine.exe: machine.bufo
	./bufo.exe machine.bufo -o ./machine.exe

brick:
	./working.exe src/bufo.bufo -o ./bufo.exe

clean:
	rm -f *.s
	rm -f *.obj
	rm -f hello.exe
	rm -f small.exe
	rm -f machine.exe
	rm -f mod.exe
