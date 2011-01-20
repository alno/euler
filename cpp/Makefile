SOURCES=$(wildcard *.cpp)
BASES=$(SOURCES:%.cpp=%)
PROBLEMS=$(BASES:problem%=%)

EXECUTABLES=${BASES}

run: ${EXECUTABLES}
	@for PR in ${PROBLEMS}; do echo -n "Running $$PR:\t"; ./problem$$PR; done

all: ${EXECUTABLES}

clean:
	rm -f $(EXECUTABLES)

${EXECUTABLES}: ${SOURCES}
	g++ $@.cpp -o $@
