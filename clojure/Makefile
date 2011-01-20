SOURCES=$(wildcard *.clj)
BASES=$(SOURCES:%.clj=%)
PROBLEMS=$(BASES:problem%=%)

run: ${SOURCES}
	@for PR in ${PROBLEMS}; do echo -n "Running $$PR:\t"; clojure -i problem$$PR.clj -e "(main)"; done

all:
clean:

