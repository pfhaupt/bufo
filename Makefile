.PHONY: all clean brick examples

RUN_EXAMPLES =
VERBOSE = @

ifeq ($(VERBOSE),)
define log
	$(info MAKE  $(1))
endef
else
define log
endef
endif

define check_exists_win
	$(if $(shell where $(1) 2>.TMP_FILE),$(call log,$(1) found in PATH.), \
	$(shell rm .TMP_FILE) \
	$(error Could not find $(1) in PATH. The Makefile is assuming a Unix environment. On Windows, you can use Git Bash or similar tools))
endef
define check_exists
	$(if $(shell which $(1) 2>/dev/null),$(call log,$(1) found in PATH.), \
	$(error Could not find $(1) in PATH. The Makefile is assuming a Unix environment.))
endef

ifeq ($(OS),Windows_NT)
  $(call log,Windows detected. Checking if all prerequisites exist.)
  # Special case because we can't use rm to delete .TMP_FILE if rm doesn't exist
  ifeq ($(shell where rm 2>.TMP_FILE),)
    $(shell del .TMP_FILE)
    $(error Could not find rm in PATH. The Makefile is assuming a Unix environment. On Windows, you can use Git Bash or similar tools)
  endif
  $(call log,rm found in PATH.)
  $(call check_exists_win,touch)
  $(call check_exists_win,mkdir)
  $(call check_exists_win,mv)
  $(call check_exists_win,printf)
  $(shell rm .TMP_FILE)
else
  $(call check_exists,rm)
  $(call check_exists,touch)
  $(call check_exists,mkdir)
  $(call check_exists,mv)
  $(call check_exists,printf)
endif

./bufo.exe: $(shell find src/ -type f) $(shell find std/ -type f)
	./bufo.exe src/bufo.bufo -o ./bufo1.exe
	mv ./bufo1.exe ./bufo.exe

all: ./bufo.exe examples

define log_run
	@printf "%-10s %s\n" "$(1)" "$(2)"
	$(VERBOSE) $(3)
endef

EXDIR := ./examples
EXSRC := $(wildcard $(EXDIR)/*.bufo)
EXOUT := $(EXSRC:.bufo=.exe)
EXRUN := $(EXSRC:.bufo=.run)

$(EXOUT): %.exe : %.bufo
	$(call log_run,BUFO,$@,./bufo.exe $^ -o $@)
$(EXRUN): %.run : %.exe | $(EXOUT)
	$(call log_run,RUN ,$<, $< --Makefile)

ifdef RUN_EXAMPLES
examples: $(EXRUN)
else
examples: $(EXOUT)
endif

brick:
	./working.exe src/bufo.bufo -o ./bufo.exe

clean:
	rm -f *.s
	rm -f *.obj
	rm -f $(EXDIR)/*.s
	rm -f $(EXDIR)/*.obj
	rm -f $(EXDIR)/*.exe
