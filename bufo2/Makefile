.PHONY: debug

EXTRA =

test.exe: test.bufo bufo2.exe
	./bufo2.exe test.bufo

bufo2.exe: $(filter-out ops.bufo test.bufo, $(wildcard *.bufo))
	bufo.exe main.bufo -o bufo2.exe $(EXTRA)

debug:
	bufo.exe main.bufo -o bufo2.exe --debug $(EXTRA)
	lldb bufo2.exe test.bufo
