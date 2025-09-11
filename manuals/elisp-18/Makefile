# Makefile for the   GNU Emacs Lisp Reference Manual.
#
# 11 August 1990

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex

TEX=tex

# Where the TeX macros are kept:
texmacrodir = /usr/local/lib/tex/macros

# Where the Emacs hierarchy lives ($EMACS in the INSTALL document for Emacs.)
# For example: 
# emacslibdir = /usr/local/gnu/lib/emacs

# Directory where Emacs is installed, by default:
emacslibdir = /usr/local/emacs

# Unless you have a nonstandard Emacs installation, these shouldn't have to 
# be changed.
emacsinfodir = $(emacslibdir)/info

# The name of the manual:

manual = elisp

# Uncomment this line for permuted index.
#permuted_index = 1 

# List of all the texinfo files in the manual:

srcs = elisp.texi back.texi \
  abbrevs.texi backups.texi \
  locals.texi buffers.texi commands.texi \
  compile.texi control.texi debugging.texi display.texi \
  errors.texi eval.texi files.texi functions.texi \
  help.texi hooks.texi internals.texi \
  intro.texi keymaps.texi lists.texi \
  loading.texi macros.texi maps.texi markers.texi \
  minibuf.texi modes.texi numbers.texi objects.texi \
  os.texi positions.texi processes.texi \
  searching.texi sequences.texi streams.texi \
  strings.texi symbols.texi syntax.texi \
  text.texi tips.texi variables.texi windows.texi \
  index.unperm # index.perm has disappeared!

.PHONY: elisp.dvi clean

elisp.dvi: $(srcs) index.texi texindex
	# First shot to define xrefs:
	$(TEX) elisp.texi
	if [ a${permuted_index} != a ]; \
	then \
	  # Produces index.fns. \
	  ./permute-index; \
	  mv index.fns elisp.fns; \
	else \
	  ./texindex elisp.??; \
	fi
	$(TEX) elisp.texi

index.texi:
	if [ a${permuted_index} != a ]; \
	then \
	  ln -s index.perm index.texi; \
	else \
	  ln -s index.unperm index.texi; \
	fi

# The info file is named `elisp'.

elisp: makeinfo $(srcs) index.texi
	./makeinfo elisp.texi

install: elisp elisp.dvi
	mv elisp elisp-* $(emacsinfodir)
	@echo also add the line for elisp to $(emacsinfodir)/dir.

installall: install
	install -c texinfo.tex $(texmacrodir)

clean:
	rm -f $(manual).??? $(manual).?? make.out core 
	rm -f makeinfo.o makeinfo getopt.o getopt1.o
	rm -f texindex.o texindex

dist:
	-mkdir temp
	-mkdir temp/elisp
	ln README Makefile permute-index $(srcs) \
   texinfo.tex texindex.c makeinfo.c getopt.c getopt1.c getopt.h \
   elisp.dvi elisp.aux elisp.??s elisp elisp-* temp/elisp
	cd temp; tar chf ../elisp.tar elisp
	-rm -rf temp
	compress < elisp.tar > elisp.tar.Z
	rm -f elisp.tar

# Make two programs used in generating output from texinfo.

CFLAGS = -g

texindex: texindex.o
	$(CC) -o $@ $(LDFLAGS) $(CFLAGS) $?
texindex.o: texindex.c

MAKEINFO_MAJOR = 1
MAKEINFO_MINOR = 0
MAKEINFO_FLAGS = -DMAKEINFO_MAJOR=$(MAKEINFO_MAJOR) -DMAKEINFO_MINOR=$(MAKEINFO_MINOR)

makeinfo: makeinfo.o getopt.o getopt1.o
	$(CC) $(LDFLAGS) -o makeinfo makeinfo.o getopt.o getopt1.o

makeinfo.o: makeinfo.c
	$(CC) -c $(CFLAGS) $(MAKEINFO_FLAGS) makeinfo.c
