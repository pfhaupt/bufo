
all: bufo.exe hello.exe small.exe machine.exe
	bufo.exe src/bufo.bufo -o bufo1.exe
	mv bufo1.exe bufo.exe

bufo.exe: src/bufo.bufo
	bufo.exe src/bufo.bufo -o bufo1.exe
	mv bufo1.exe bufo.exe

hello.exe: hello.bufo
	bufo.exe hello.bufo -o hello.exe

small.exe: small.bufo
	bufo.exe small.bufo -o small.exe

machine.exe: machine.bufo
	bufo.exe machine.bufo -o machine.exe

brick:
	working.exe src/bufo.bufo -o bufo.exe

clean:
	rm -f *.s
	rm -f *.obj
