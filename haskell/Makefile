SOURCES=$(wildcard *.hs)
BASES=$(SOURCES:%.hs=%)
PROBLEMS=$(BASES:problem%=%)

EXECUTABLES=${BASES}

run: ${EXECUTABLES}
	@for PR in ${PROBLEMS}; do echo -n "Running $$PR:\t"; ./problem$$PR; done

all: ${EXECUTABLES}

clean:
	rm -f $(EXECUTABLES)

${EXECUTABLES}: ${SOURCES}
	ghc $@.hs -o $@
