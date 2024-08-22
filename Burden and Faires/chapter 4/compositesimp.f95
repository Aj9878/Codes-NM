program compsimp

    implicit none
    real a,b,XI0,XI1,XI2,XI,X,h,f
    integer i,n
    a=0
    b=1
    n=100

    h=(b-a)/n

    XI0=f(a)+f(b)
    XI1=0
    XI2=0

    do i=1,n-1,1
    X=a+i*h

        if(mod(i,2)==0)then
            XI2=XI2+f(X)
        else
            XI1=XI1+f(X)
        end if
    end do

    XI=h*(XI0+2*XI2+4*XI1)/3

    write(*,*)  "The integral is",XI

end program

real function f(x)
real,intent(in)::x
f=sin(x)
end function


