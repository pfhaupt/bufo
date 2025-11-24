# vim: set noet

.PHONY: brick

brick:
	./working src/bufo.bufo -o linux_bufo

compiler:
	./linux_bufo src/bufo.bufo -o bufo

compiler2: compiler
	./bufo src/bufo.bufo -o linux_bufo
