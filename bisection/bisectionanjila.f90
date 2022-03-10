program bisection
implicit none
real, parameter ::error=1e-4

real x1,x2, f,x0, R, f1, f2, f0,i
print*, 'give the initial number'
read*, x1, x2
f1 = f(x1)
f2 = f(x2)

i=1

45 if(f1*f2.gt. 0) then
print*, 'it donot contain the roots'
i=i+1
else
x0 = x1+x2/2
f0 = f(x0)
end if

if(f1*f0.le. 0) then
x2 = x0
 else
x1 = x0

end if


if (abs((x2-x1)/x1)<error) then
!if (abs((x2-x1)/x1)).lt.error then
print*, ' the value of root is', x0, 'number of iteration',i
else
go to 45
end if 


end program


real function f(x)
real::x
f = cos(x)-x
end function
