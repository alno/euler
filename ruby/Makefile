SOURCES=$(wildcard *.rb)
BASES=$(SOURCES:%.rb=%)
PROBLEMS=$(BASES:problem%=%)

run: ${SOURCES}
	@for PR in ${PROBLEMS}; do echo -n "Running $$PR:\t"; ruby problem$$PR.rb; done

all:

clean:
	rm -f *~
