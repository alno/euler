MAKEFILES=$(wildcard */Makefile)
LANGS=$(MAKEFILES:%/Makefile=%)

run:
	@for LANG in ${LANGS}; do \
		cd $$LANG; make run; cd ..; \
	done

all:
	@for LANG in ${LANGS}; do \
		cd $$LANG; make all; cd ..; \
	done

clean:
	@for LANG in ${LANGS}; do \
		cd $$LANG; make clean; cd ..; \
	done
