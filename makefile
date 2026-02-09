test:
#	Compile each module
	gfortran src/multiarm_bandit.f03 -c -o build/multiarm_bandit.o
	gfortran src/greedy_y.f03 -c -o build/greedy_y.o
	gfortran src/logger.f03 -c -o build/logger.o

# Compile and link to main file
	gfortran src/test.f03 build/multiarm_bandit.o build/greedy_y.o build/logger.o -o build/a.out

	./build/a.out