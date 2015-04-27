all : fact1 fact2 factorial hello kindfns nested root simple temp

fact1 :
	gfortran -o fact1 factorial1.f90
fact2 :
	gfortran -o fact2 factorial2.f90
factorial :
	gfortran -o factorial factorial.f90
hello :
	gfortran -o hello hello.f90
kindfns :
	gfortran -o kindfns kindfns.f90
nested :
	gfortran -o nested nested.f90
root :
	gfortran -o root.x quad_roots.f90
simple :
	gfortran -o simple simple.f90
temp :
	gfortran -o temp temp.f90
clean :
	rm -rf fact1 fact2 factorial hello kindfns nested root.x simple temp
