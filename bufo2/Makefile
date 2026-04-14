
EXTRA =

monitor.exe: monitor.bufo
	bufo.exe monitor.bufo -o monitor.exe $(EXTRA)
	./monitor.exe

test.exe: test.bufo bufo2.exe
	./bufo2.exe test.bufo

bufo2.exe: $(filter-out test.bufo, $(wildcard *.bufo))
	bufo.exe main.bufo -o bufo2.exe $(EXTRA)

debug:
	bufo.exe main.bufo -o bufo2.exe --debug $(EXTRA)
	lldb bufo2.exe test.bufo
