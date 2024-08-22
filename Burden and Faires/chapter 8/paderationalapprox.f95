program pra
    implicit none
    integer, parameter :: m = 2, n = 3, nn = m + n
    integer i, j, k
    real q(0:m), p(0:n), b(nn, nn+1), bcopy, xm, fact, a(0:nn)

    
    do i = 0, nn
        a(i) = ((-1)**i) / fact(i)
    end do

    q(0) = 1
    p(0) = a(0)
    do i = 1, nn
        j=1
        do j = 1, i-1
            if (j <= n) then
                b(i, j) = 0
            endif
        end do
        if (i <= n) then
            b(i, i) = 1
        endif
        do j = i+1, nn
            b(i, j) = 0
        end do
        do j = 1, i
            if (j <= m) then
                b(i, n+j) = -a(i-j)
            endif
        end do
        do j = n+i+1, nn
            b(i, j) = 0
        end do
        b(i, nn+1) = a(i)
    end do
    
    
    do i = n+1, nn-1
        k = i  ! Initialize k to i
        do j = i, nn
            if (abs(b(j, i)) == maxval(abs(b(i:nn, i)))) then
                k = j
                exit  ! Ensures we exit after finding the max value
            endif
        end do
        if (b(k, i) == 0) then
            print *, 'the system is singular'
            stop
        endif
        if (k .ne. i) then
            do j = i, nn+1
                bcopy = b(i, j)
                b(i, j) = b(k, j)
                b(k, j) = bcopy
            end do
        endif
        do j = i+1, nn
            xm = b(j, i) / b(i, i)
            do k = i+1, nn+1
                b(j, k) = b(j, k) - xm * b(i, k)
            end do
            b(j, i) = 0
        end do
    end do

    if (b(nn, nn) == 0) then
        print *, 'the system is singular'
        stop
    endif

    if (m > 0) then
        q(m) = b(nn, nn+1) / b(nn, nn)
    endif

    do i = nn-1, n+1, -1
        bcopy = 0.0
        do j = i+1, nn
            bcopy = bcopy + b(i, j) * q(j-n)
        end do
        q(i-n) = (b(i, nn+1) - bcopy) / b(i, i)   
    end do

    do i = n, 1, -1
        bcopy = 0.0
        do j = n+1, nn
            bcopy = bcopy + b(i, j) * q(j-n)
        end do
        p(i) = b(i, nn+1) - bcopy
    end do

    print *, q, p
end program pra

 
real function fact(i)
    implicit none
    integer i, j
    if (i == 0) then
        fact = 1
    else
        fact = 1
        do j = 1, i
            fact = j * fact
        end do
    endif
end function
