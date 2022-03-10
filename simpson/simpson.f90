program simpson_rule
implicit none
integer:: i ,n;
real::a , b , dx , sum , f , x


n=100
a=0
b=3.14
dx=(b-a)/n
sum=f(a)+f(b)


do i=1,n-1 ;
x=a+(i*dx)
If (i/2*2.ne.i) then
sum=sum+4*f(x)
else ;
sum=sum+2*f(x)
endif ;
enddo


sum=sum*dx/3.0
print*, "for n =",n,"Integral =", sum
end


real function f(x)
implicit none
real::x
f=sin(x)
end
