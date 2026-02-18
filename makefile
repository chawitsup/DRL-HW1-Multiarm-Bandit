test:
# Compile each fortran module
	gfortran src/multiarm_bandit.f03 -c -o build/multiarm_bandit.o
	gfortran src/greedy_y.f03 -c -o build/greedy_y.o

# Compile and link to test file
	gfortran src/test.f03 build/multiarm_bandit.o build/greedy_y.o -o build/a.out
	./build/a.out

greedy_y:
# Compile each fortran module
	gfortran src/multiarm_bandit.f03 -c -o build/multiarm_bandit.o
	gfortran src/greedy_y.f03 -c -o build/greedy_y.o

# Compile and link to greedy epsilon's file
	gfortran src/do_greedy_y_agent.f03 build/multiarm_bandit.o build/greedy_y.o -o build/a.out
	./build/a.out
