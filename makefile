test:
# Compile each fortran module
	gfortran src/multiarm_bandit.f08 -c -g -o build/multiarm_bandit.o
	gfortran src/greedy_y.f08 -c -g -o build/greedy_y.o

# Compile and link to test file
	gfortran src/test.f08 build/multiarm_bandit.o build/greedy_y.o -g -o build/a.out
	./build/a.out

greedy_y:
# Compile each fortran module
	gfortran src/multiarm_bandit.f08 -c -g -o build/multiarm_bandit.o
	gfortran src/greedy_y.f08 -c -g -o build/greedy_y.o

# Compile and link to greedy epsilon's file
	gfortran src/do_greedy_y_agent.f08 build/multiarm_bandit.o build/greedy_y.o -g -o build/a.out
	./build/a.out
