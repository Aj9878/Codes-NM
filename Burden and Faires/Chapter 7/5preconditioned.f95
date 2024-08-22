program cgm
    implicit none
    integer, parameter :: n = 5
    integer :: i, j, k, Q
    real, dimension(1:n) :: r, x, w, v, b, u, ax, av
    real, dimension(1:n, 1:n) :: C, a
    real :: alpha, sum0, t, s , beta, tol

    ! Setting the number of iterations and tolerance
    Q = 14
    tol = 10E-7

    ! Initialize matrix A
    a = reshape([0.2, 0.1, 1.0, 1.0, 0.0, &
                 0.1, 4.0, -1.0, 1.0, -1.0, &
                 1.0, -1.0, 60.0, 0.0, -2.0, &
                 1.0, 1.0, 0.0, 8.0, 4.0, &
                 0.0, -1.0, -2.0, 4.0, 700.0], shape=[n, n])

    ! Initialize vector b
    b = [1.0, 2.0, 3.0, 4.0, 5.0]

    ! Initial guess x(0)
    x = [0.0, 0.0, 0.0, 0.0, 0.0]

    ! Preconditioning matrix C (Identity matrix for simplicity)
    C = 0.0
    C(1,1) = 1.0; C(2,2) = 1.0; C(3,3) = 1.0; C(4,4) = 1.0; C(5,5) = 1.0

    ! Initial residual and preconditioning
    r = b - matmul(a, x)
    w = matmul(C, r)
    v = matmul(C, w)
    alpha = sum(w**2)

    k = 1
    do while (k <= Q)
        u = matmul(a, v)
        dum = dot_product(v, u)
        t = alpha / dum
        x = x + t * v
        r = r - t * u
        w = matmul(C, r)
        beta = sum(w**2)
            !sqrt(sum(r**2) will give the norm using the pytthagoras theorem)
        if (sqrt(sum(r**2)) < tol) then
            print *, "Solution vector:", x
            print *, "Residual vector:", r
            exit
        endif

        s = beta / alpha
        v = matmul(C, w) + s * v
        alpha = beta
        k = k + 1
    end do

    if (k > Q) then
        print *, "Max iteration reached"
    endif

end program cgm
