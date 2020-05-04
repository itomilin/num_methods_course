FC=gfortran-8
FFLAGS=-Wall -fimplicit-none  -std=f2008ts -Wno-maybe-uninitialized
FOPT=-O0

#all: clean build run
all: clean build run

clean:
	rm -Rf ./obj/*
	rm -Rf ./bin/*

build:
	$(FC) $(FFLAGS) -c "./environment_x64.f90" -J "./obj/" -o "./obj/environment.o"

	$(FC) $(FFLAGS) -c "./modules/DECOMP.f90" -I "./obj/" -o "./obj/DECOMP.o"
	$(FC) $(FFLAGS) -c "./modules/FEHL.f90"   -I "./obj/" -o "./obj/FEHL.o"
	$(FC) $(FFLAGS) -c "./modules/QUANC8.f90" -I "./obj/" -o "./obj/QUANC8.o"
	$(FC) $(FFLAGS) -c "./modules/RKF45.f90"  -I "./obj/" -o "./obj/RKF45.o"
	$(FC) $(FFLAGS) -c "./modules/RKFS.f90"   -I "./obj/" -o "./obj/RKFS.o"
	$(FC) $(FFLAGS) -c "./modules/SOLVE.f90"  -I "./obj/" -o "./obj/SOLVE.o"
	$(FC) $(FFLAGS) -c "./modules/ZEROIN.f90" -I "./obj/" -o "./obj/ZEROIN.o"

	$(FC) $(FFLAGS) -c "./src/main.f90" -I "./src/" -o "./obj/main.o"
#	$(FC) $(FFLAGS) $(FOPT) -c "$(DIR)/src/funA.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/funA.o"
#	$(FC) $(FFLAGS) $(FOPT) -o "$(DIR)/bin/app" "$(DIR)/obj/environment.o" "$(DIR)/obj/RKF45.o" "$(DIR)/obj/RKFS.o" "$(DIR)/obj/FEHL.o" "$(DIR)/obj/QUANC8.o"  "$(DIR)/obj/ZEROIN.o" "$(DIR)/obj/funA.o"	"$(DIR)/obj/SOLVE.o" "$(DIR)/obj/DECOMP.o" 
	$(FC) $(FFLAGS) $(FOPT) -o "./bin/app" "./obj/DECOMP.o" "./obj/FEHL.o" "./obj/QUANC8.o" \
                               "./obj/RKF45.o" "./obj/RKFS.o" "./obj/SOLVE.o" "./obj/ZEROIN.o" \
                               "./obj/main.o"

run:
	./bin/app
#	cat ./bin/output.txt

