all:
	gfortran src/multiarm_bandit.f03 -c -o build/multiarm_bandit.o

	gfortran src/main.f03 build/multiarm_bandit.o -o build/a.out

	./build/a.out