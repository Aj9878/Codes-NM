program cubic_spline_boundary_value_problem
    implicit none
    integer :: n, i, j
    real :: h
    real, dimension(:), allocatable :: x_values, c, b
    !phi in algorithm is A here
    real, dimension(:,:), allocatable :: A

    ! Step 1: Define the number of intervals
    n=10

    ! Step 2: Set up grid points
    h = 1.0 / real(n + 1)
    allocate(x_values(0:n+1))
    
    !create array x_values and put the values i*h,i=0 to 10
    x_values = (/ (i*h, i=0,n+1) /)

    x_values(1) = 0.0
    x_values(2) = 0.0
    x_values(n+2) =1.0 
    x_values(n+3) = 1.0
    !step 3
   

    ! Step 4: Define cubic spline basis
    allocate(A(0:n+1, 0:n+1))
allocate(b(0:n+1))
allocate(c(0:n+1))

do i = 0, n+1
    do j = i, min(i+3, n+1)
        ! Define the cubic spline basis functions based on the conditions in Step 4
        if (i == 0) then
            A(i,j) = S((x_values(j) - x_values(i)) / h) - 4.0 * S((x_values(j) - x_values(i)) / h + 1.0)
        else if (i == 1) then
            A(i,j) = S((x_values(j) - x_values(i)) / h) - S((x_values(j) - x_values(i)) / h - 1.0) - 4.0 * S((x_values(j) - x_values(i)) / h + 1.0)
        else if (i == n) then
            A(i,j) = S((x_values(j) - x_values(i)) / h) - S((x_values(j) - x_values(i)) / h + 1.0) - 4.0 * S((x_values(j) - x_values(i)) / h - 1.0)
        else if (i == n+1) then
            A(i,j) = S((x_values(j) - x_values(i)) / h) - 4.0 * S((x_values(j) - x_values(i)) / h - 1.0)
        else
            A(i,j) = S((x_values(j) - x_values(i)) / h)
        end if
        if (i /= j) A(j,i) = A(i,j) ! A is symmetric
    end do
end do

    ! Step 5-10: Compute coefficients of linear system
    do i = 0, n+1
        do j = i, min(i+3, n+1)
            l=max(x_value(j-2),0)
            u=min(x_values(i+2),1)
            ! Compute integral involving p and q
             call integrate_pq(l,u, xi,h)
             A(i,j)=xi
            if (i /= j) then A(j,i) = A(i,j) ! A is symmetric
        end do
    end do

    ! Steps 7-8: Adjust A matrix for boundary conditions
    
    do i = 4, n+1
        do j = 0, i-4
            A(i,j) = 0.0
        end do
    end do

    do i = 0, n-3
        do j = i+4, n+1
            A(i,j) = 0.0
             
        end do
    end do

    ! Step 9: Compute the right-hand side vector b
    
    l=max(x_value(j-2),0)
    u=min(x_values(i+2),1)
    do i = 0, n+1
        call integrate_f(l,u, xi,h)
        b(i) = xi
    end do
    ! Step 10: Solve linear system Ac = b
    call solve_linear_system(n+2, A, b, c)

    ! Step 11: Output coefficients
    print*, "Cubic spline coefficients:"
    do i = 0, n+1
        print*, c(i)
    end do

    deallocate(A)
    deallocate(b)
    deallocate(c)
    deallocate(x_values)

end program cubic_spline_boundary_value_problem

! Subroutine to solve linear system Ac = b
 

real function S(x)
real, intent(in) :: x
if(x .le. -2) then
    s=0
else if ((x .le. -1.0).and.(x .ge. -2.0)) then
    S = (1.0 / 4.0 )* (2.0 + x)**3
else if ((x <= 0.0).and. (x .ge. -1.0)) then
    S = (1.0 /  4.0) *( (2.0 + x)**3 - 4.0 * (1.0 + x)**3)
else if ((x <= 1.0) .and. (x .ge. 0) )then
    S = (1.0 / 4.0 )*( (2.0 - x)**3 - 4.0 * (1.0 - x)**3)
else
    S =( 1.0 / 4.0 )* ((2.0 - x)**3)
end if
end function S

subroutine integrate_pq(a,b,xi,hi)
 implicit none

    real XI0,XI1,XI2,X,h,ff,a,b,xi
    integer i,nnn,hi

    nnn=10

    h=(b-a)/nnn

    XI0=ff(a,b,a,hi)+ff(a,b,b,hi)
    XI1=0
    XI2=0

    do i=1,nnn-1,1
    X=a+i*h

        if(mod(i,2)==0)then
            XI2=XI2+ff(a,b,X,hi)
        else
            XI1=XI1+ff(a,b,X,hi)
        end if
    end do

    XI=h*(XI0+2*XI2+4*XI1)/3.0
end subroutine

real function ff(a,b,x,hi)
implicit none
real x,a,b,sq,p,f
integer hi
if(hi==1) then
ff=(b-x)*(x-a)*sq(x)
endif
if(hi==2) then
ff=((x-a)**2)*sq(x)
endif
if(hi==3) then
ff=((b-x)**2)*sq(x)
endif
if(hi==4) then
ff=p(x)
endif
if(hi==5) then
ff=(x-a)*f(x)
endif
if(hi==6) then
ff=(b-x)*f(x)
endif
end function


! Step 10: Solve the linear system Ac = b
call solve_linear_system(n+2, A, b, c)

! Subroutine to solve linear system
subroutine solve_linear_system(n, A, b, c)
    integer, intent(in) :: n
    real, dimension(n,n), intent(in) :: A
    real, dimension(n), intent(in) :: b
    real, dimension(n), intent(out) :: c
    integer :: info
    ! Declare additional variables for LAPACK routine
    integer :: ipiv(n)

    ! Call LAPACK routine for solving linear system
    call dgesv(n, 1, A, n, ipiv, b, n, info)

    ! Check if solution is successful
    if (info == 0) then
        ! Solution successful, retrieve solution
        c = b
    else
        ! Solution failed
        print*, 'Error: Linear system solver failed'
    end if
end subroutine solve_linear_system
