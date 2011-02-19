SOURCES=$(wildcard *.scala)
BASES=$(SOURCES:%.scala=%)
PROBLEMS=$(BASES:problem%=%)

run: ${SOURCES}
	@for PR in ${PROBLEMS}; do echo -n "Running $$PR:\t"; scala problem$$PR.scala; done

all:

clean:
	rm -f *~
