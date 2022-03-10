program pikovalue
implicit none
real x,y,r,countincircle,pi
integer k 
countincircle =0
open(unit=7,file="piko.dat")
do k = 1,50000
call random_number(x)
call random_number(y)
r = x*x+y*y

if (r .lt.1)then
   countincircle=countincircle+1
endif

pi =4.0*countincircle/k 
write(7,*) k, countincircle,pi
end do
print*,pi
end
