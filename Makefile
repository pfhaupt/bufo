
all:
	bufo.exe src/bufo.bufo -o bufo1.exe
	mv bufo1.exe bufo.exe
	bufo.exe src/bufo.bufo -o bufo1.exe
	mv bufo1.exe bufo.exe
	bufo.exe hello.bufo -o hello.exe
	bufo.exe small.bufo -o small.exe
#	 hello.exe
	rm *.obj

brick:
	working.exe src/bufo.bufo bufo.exe

