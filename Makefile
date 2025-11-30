# vim: set noet

EXDIR=./examples/
HOWTODIR=./how_to/


.PHONY: brick rock 2rocks

brick:
	./working src/bufo.bufo -o linux_bufo

rock:
	./linux_bufo src/bufo.bufo -o bufo

2rocks: rock
	./bufo src/bufo.bufo -o linux_bufo

clean: clean_example clean_howto
	rm -f *.s
	rm -f *.ll
	rm -f *.obj
	rm -f *.pdb
	rm -f *.rdi
	rm -f *.ilk
clean_example:
	rm -f $(EXDIR)/*.s
	rm -f $(EXDIR)/*.ll
	rm -f $(EXDIR)/*.obj
	rm -f $(EXDIR)/*.exe
	rm -f $(EXDIR)/*.pdb
	rm -f $(EXDIR)/*.rdi
clean_howto:
	rm -f $(HOWTODIR)/*.s
	rm -f $(HOWTODIR)/*.ll
	rm -f $(HOWTODIR)/*.obj
	rm -f $(HOWTODIR)/*.exe
	rm -f $(HOWTODIR)/*.pdb
	rm -f $(HOWTODIR)/*.rdi
