PROGRAM Steepest_Descent
    IMPLICIT NONE
    INTEGER :: n, N, k
    REAL :: TOL
    REAL, DIMENSION(:), ALLOCATABLE :: x, z
    REAL :: g1, g2, g3, g0, h1, h2, h3
    REAL :: alpha1, alpha2, alpha3, alpha0
    REAL :: norm_z

    ! Function prototypes
    REAL FUNCTION f1(x)
        REAL, DIMENSION(:) :: x
    END FUNCTION f1

    REAL FUNCTION f2(x)
        REAL, DIMENSION(:) :: x
    END FUNCTION f2

    REAL FUNCTION f3(x)
        REAL, DIMENSION(:) :: x
    END FUNCTION f3

    SUBROUTINE gradient(x, z)
        REAL, DIMENSION(:) :: x, z
    END SUBROUTINE gradient

    ! Input parameters
    n = 3 ! Number of variables
    ALLOCATE(x(n), z(n))

    x = (/0.0, 0.0, 0.0/) ! Initial approximation

    TOL = 1.0E-6 ! Tolerance
    N = 100 ! Maximum number of iterations

    ! Step 1: Set k = 1
    k = 1

    ! Main loop
    DO WHILE (k <= N)
        ! Step 2: Compute gradient and norm
        g1 = NORM2(f1(x), f2(x), f3(x))
        CALL gradient(x, z)
        norm_z = NORM2(z)

        ! Step 3: Check for zero gradient
        IF (norm_z == 0.0) THEN
            PRINT *, 'Zero gradient'
            PRINT *, 'x = ', x
            PRINT *, 'f1(x) = ', f1(x)
            PRINT *, 'f2(x) = ', f2(x)
            PRINT *, 'f3(x) = ', f3(x)
            STOP
        END IF

        ! Step 5: Normalize gradient
        z = z / norm_z
        alpha1 = 0.0
        alpha3 = 1.0
        g3 = NORM2(f1(x - alpha3 * z), f2(x - alpha3 * z), f3(x - alpha3 * z))

        ! Step 6: Bracket the minimum
        DO WHILE (g3 >= g1)
            alpha3 = alpha3 / 2.0
            g3 = NORM2(f1(x - alpha3 * z), f2(x - alpha3 * z), f3(x - alpha3 * z))

            ! Check for likely improvement
            IF (alpha3 < TOL / 2.0) THEN
                PRINT *, 'No likely improvement'
                PRINT *, 'x = ', x
                PRINT *, 'f1(x) = ', f1(x)
                PRINT *, 'f2(x) = ', f2(x)
                PRINT *, 'f3(x) = ', f3(x)
                STOP
            END IF
        END DO

        ! Step 9: Interpolation
        alpha2 = alpha3 / 2.0
        g2 = NORM2(f1(x - alpha2 * z), f2(x - alpha2 * z), f3(x - alpha2 * z))

        ! Step 10: Newton's divided differences
        h1 = (g2 - g1) / alpha2
        h2 = (g3 - g2) / (alpha3 - alpha2)
        h3 = (h2 - h1) / alpha3

        ! Step 11: Compute critical point
        alpha0 = 0.5 * (alpha2 - h1 / h3)
        g0 = NORM2(f1(x - alpha0 * z), f2(x - alpha0 * z), f3(x - alpha0 * z))

        ! Step 12: Determine new alpha
        alpha = MIN(alpha0, alpha3)

        ! Step 13: Update x
        x = x - alpha * z

        ! Step 14: Check for convergence
        IF (ABS(g - g1) < TOL) THEN
            PRINT *, 'Converged:'
            PRINT *, 'x = ', x
            PRINT *, 'f1(x) = ', f1(x)
            PRINT *, 'f2(x) = ', f2(x)
            PRINT *, 'f3(x) = ', f3(x)
            STOP
        END IF

        ! Step 15: Increment iteration count
        k = k + 1
    END DO

    ! Step 16: Maximum iterations exceeded
    PRINT *, 'Maximum iterations exceeded'
    STOP

END PROGRAM Steepest_Descent

! Define the functions f1, f2, and f3
REAL FUNCTION f1(x)
    REAL, DIMENSION(:) :: x
    f1 = 3 * x(1) - COS(x(2) * x(3)) - 1.2
END FUNCTION f1

REAL FUNCTION f2(x)
    REAL, DIMENSION(:) :: x
    f2 = x(1)**2 - 81.0 * (x(2) + 0.1)**2 + SIN(x(3)) + 1.06
END FUNCTION f2

REAL FUNCTION f3(x)
    REAL, DIMENSION(:) :: x
    f3 = EXP(-x(1) * x(2)) + 20 * x(3) + 10 * PI - 3.0**(1.0/3.0)
END FUNCTION f3

! Define the function to compute the gradient
SUBROUTINE gradient(x, z)
    REAL, DIMENSION(:) :: x, z
    REAL :: h = 1.0E-6
    !!!impliment gradient using partial derivative
END SUBROUTINE gradient
