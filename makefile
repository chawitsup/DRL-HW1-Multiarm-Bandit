test:
#	Compile each module
	gfortran src/multiarm_bandit.f03 -c -o build/multiarm_bandit.o
	gfortran src/greedy_y.f03 -c -o build/greedy_y.o

# Compile and link to main file
	gfortran src/test.f03 build/multiarm_bandit.o build/greedy_y.o -o build/a.out

	./build/a.out