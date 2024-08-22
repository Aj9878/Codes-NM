program Poisson_eqnfintediff
    implicit none
    integer, parameter :: n = 6, m = 5
    integer, parameter :: Nt = 10000
    real :: a, b, c, d, tol, x(n), y(m), w(n, m), lambda, mu, z, norm, h, k
    real, external :: f, g
    integer :: l, i, j

    ! Input parameters
    tol = 1.0E-10
    a = 0.0
    b = 2.0
    c = 0.0
    d = 1.0

    ! Calculate step sizes
    h = (b - a) / n
    k = (d - c) / m

    ! Generate mesh points
    do i = 1, n-1
        x(i) = a + i * h
    end do

    do j = 1, m-1
        y(j) = c + j * k
    end do

    ! Initialize solution matrix
    do i = 1, n-1
        do j = 1, m-1
            w(i, j) = 0.0
        end do
    end do

    ! Pre-calculate coefficients
    lambda = (h**2) / (k**2)
    mu = 2 * (1 + lambda)
    l = 1

    ! Gauss-Seidel iterations
    do while (l <= Nt)
        ! Update boundary conditions (top side)
        z = (-h**2 * f(x(1), y(m-1)) + g(a, y(m-1)) + lambda * g(x(1), d) + lambda * w(1, m-2) + w(2, m-1)) / mu
        norm = abs(z - w(1, m-1))
        w(1, m-1) = z

        ! Update interior points
        do i = 2, n-2
            z = (-h**2 * f(x(i), y(m-1)) + lambda * g(x(i), d) + w(i-1, m-1) + w(i+1, m-1) + lambda * w(i, m-2)) / mu
            if (abs(w(i, m-1) - z) > norm) then
                norm = abs(w(i, m-1) - z)
                w(i, m-1) = z
            end if
        end do

        ! Update boundary conditions (bottom side)
        z = (-h**2 * f(x(n-1), y(m-1)) + g(b, y(m-1)) + lambda * g(x(n-1), d) + w(n-2, m-1) + lambda * w(n-1, m-2)) / mu
        if (abs(w(n-1, m-1) - z) > norm) then
            norm = abs(w(n-1, m-1) - z)
            w(n-1, m-1) = z
        end if

        ! Update interior points (bottom to top)
        do j = m-2, 2, -1
            z = (-h**2 * f(x(1), y(j)) + g(a, y(j)) + lambda * w(1, j+1) + lambda * w(1, j-1) + w(2, j)) / mu
            if (abs(w(1, j) - z) > norm) then
                norm = abs(w(1, j) - z)
                w(1, j) = z
            end if

            do i = 2, n-2
                z = (-h**2 * f(x(i), y(j)) + w(i-1, j) + lambda * w(i, j+1) + w(i+1, j) + lambda * w(i, j-1)) / mu
                if (abs(w(i, j) - z) > norm) then
                    norm = abs(w(i, j) - z)
                    w(i, j) = z
                end if
            end do

            z = (-h**2 * f(x(n-1), y(j)) + g(b, y(j)) + w(n-2, j) + lambda * w(n-1, j+1) + lambda * w(n-1, j-1)) / mu
            if (abs(w(n-1, j) - z) > norm) then
                norm = abs(w(n-1, j) - z)
                w(n-1, j) = z
            end if
        end do

        ! Update boundary conditions (bottom-left corner)
        z = (-h**2 * f(x(1), y(1)) + g(a, y(1)) + lambda * g(x(1), c) + lambda * w(1, 2) + w(2, 1)) / mu
        if (abs(w(1, 1) - z) > norm) then
            norm = abs(w(1, 1) - z)
            w(1, 1) = z
        end if

        ! Update interior points (left side)
        do i = 2, n-2
            z = (-h**2 * f(x(i), y(1)) + lambda * g(x(i), c) + w(i-1, 1) + lambda * w(i, 2) + w(i+1, 1)) / mu
            if (abs(w(i, 1) - z) > norm) then
                norm = abs(w(i, 1) - z)
                w(i, 1) = z
            end if
        end do

        ! Update boundary conditions (top-left corner)
        z = (-h**2 * f(x(n-1), y(1)) + g(b, y(1)) + lambda * g(x(n-1), c) + w(n-2, 1) + lambda * w(n-1, 2)) / mu
        if (abs(w(n-1, 1) - z) > norm) then
            norm = abs(w(n-1, 1) - z)
            w(n-1, 1) = z
        end if

        ! Check convergence
        if (norm <= tol) then
            ! Output results if converged
            do i = 1, n-1
                do j = 1, m-1
                    print *, "i=", i, "j=", j, "w=", w(i, j), 'x=', x(i), "y=", y(j)
                end do
            end do
            stop
        end if

        ! Increment iteration counter
        l = l + 1
    end do

    ! Maximum iteration limit reached
    print *, "Maximum number of iterations exceeded"
end program

! Define the function f
real function f(x, y)
    real :: x, y
    f = x * exp(y)
end function

! Define the function g
real function g(x, y)
    real :: x, y
    if (x == a) then
        g = a
    else if (x == b) then
        g = 2 * exp(y)
    else if (y == c) then
        g = x
    else if (y == d) then
        g = exp(1.0) * x
    end if
end function
