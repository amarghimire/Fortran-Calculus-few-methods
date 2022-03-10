program newton_raphson
implicit none
real, parameter::error =1e-4
integer::i

real::xo,x1,f,fd
print*, 'Enter initial Guess'
read*,xo
i=1
10 x1=xo-(f(xo)/fd(xo))

if(abs((x1-xo)/x1)<error)then
print*,"root is ",x1,"no. of iteration=",i
else
xo=x1
i=i+1
goto 10
endif

end

real function f(x)
real, intent(in) :: x
f = cos(x)
end

real function fd(x)
real::x
fd=-sin(x)
end
