program bisection
implicit none
real,parameter::error=1e-4
real::a,b,f,c
10 print*,'Input the two interval value'
read(*,*) a,b

15 if (f(a)*f(b)<0) then
c=(a+b)/2.0
else
write(*,*)"try with another value of a & b "
goto 10
endif


if(f(a)*f(c)<0)then
b=c
else
a=c
endif


if(abs(b-a)>error) goto 15
write(*,*)"the root is=",c
end

real function f(x)
implicit none
real::x
f=cos(x)
end
