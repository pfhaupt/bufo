.PHONY: all clean brick examples clean_example how_to clean_howto bootstrap_windows bootstrap_linux bootstrap

ifeq ($(OS),Windows_NT)
TARGET = x86_64-pc-windows-msvc
else
TARGET = x86_64-pc-linux-gnu
endif
RUN_EXAMPLES =
RUN_HOWTO =
VERBOSE = @
BUFO_FLAGS = --warn --target $(TARGET) $(EXTRA)

BUFO_CC = ./bufo.exe

ifeq ($(VERBOSE),)
  ALL_FLAGS = $(BUFO_FLAGS) --warn-extra --verbose
  define log
    $(info MAKE  $(1))
  endef
else
  ALL_FLAGS = $(BUFO_FLAGS)
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

${BUFO_CC}: $(shell find src/ -type f) $(shell find std/ -type f)
	${BUFO_CC} src/bufo.bufo -o ${BUFO_CC}.tmp $(ALL_FLAGS) --no-extern-comptime
	mv ${BUFO_CC}.tmp ${BUFO_CC}

all: ${BUFO_CC} examples how_to

define log_run
	@printf "%-10s %s\n" "$(1)" "$(2)"
	$(VERBOSE) $(3)
endef

EXDIR := ./examples
EXSRC := $(wildcard $(EXDIR)/*.bufo)
EXOUT := $(EXSRC:.bufo=.exe)
EXRUN := $(EXSRC:.bufo=.run)

$(EXOUT): %.exe : %.bufo
	$(call log_run,BUFO,$@,${BUFO_CC} $^ -o $@ $(BUFO_FLAGS))
$(EXRUN): %.run : %.exe | $(EXOUT)
	$(call log_run,RUN ,$<, $< --Makefile)

ifdef RUN_EXAMPLES
examples: ${BUFO_CC} $(EXRUN)
else
examples: ${BUFO_CC} $(EXOUT)
endif

HOWTODIR := ./how_to
HOWTOSRC := $(wildcard $(HOWTODIR)/*.bufo)
HOWTOOUT := $(HOWTOSRC:.bufo=.exe)
HOWTORUN := $(HOWTOSRC:.bufo=.run)

$(HOWTOOUT): %.exe : %.bufo
	$(call log_run,BUFO,$@,${BUFO_CC} $^ -o $@ $(BUFO_FLAGS))
$(HOWTORUN): %.run : %.exe | $(HOWTOOUT)
	$(call log_run,RUN ,$<, $< --Makefile)

ifdef RUN_HOWTO
how_to: ${BUFO_CC} $(HOWTORUN)
else
how_to: ${BUFO_CC} $(HOWTOOUT)
endif

bootstrap_windows: ${BUFO_CC}
	${BUFO_CC} src/bufo.bufo -o bootstrap/bufo_windows.c --transpile-c --target x86_64-pc-windows-msvc
	clang-format -style="{IndentWidth: 4, TabWidth: 4, ColumnLimit: 200}" -i bootstrap/bufo_windows.c
bootstrap_linux: ${BUFO_CC}
	${BUFO_CC} src/bufo.bufo -o bootstrap/bufo_linux.c --transpile-c --target x86_64-pc-linux-gnu
	clang-format -style="{IndentWidth: 4, TabWidth: 4, ColumnLimit: 200}" -i bootstrap/bufo_linux.c
bootstrap: bootstrap_windows bootstrap_linux

brick:
	./working.exe src/bufo.bufo -o $(BUFO_CC) $(EXTRA) --no-extern-comptime

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
