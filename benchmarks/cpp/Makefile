CXX = g++
CXXFLAGS = -O2 -Wall

OBJS = \
    bench-program.o \
	16lawrencel-dynamic-graphs/fullDynamic.o \
	16lawrencel-dynamic-graphs/eulerTourTree.o \
	16lawrencel-dynamic-graphs/splayTree.o \
	dropbox-json11/json11.o \

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

bench-program: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS)

.PHONY: clean
clean:
	find . -name '*.o' -delete
