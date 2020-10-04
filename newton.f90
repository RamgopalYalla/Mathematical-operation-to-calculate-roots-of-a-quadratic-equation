! #################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! - Implementation of Algorithm
! #################################################################
!---------------------------------------------------------------------------

real(8) function newton(lb,x0,h,maxitx,eps)

    implicit none
    !-----------------------------------------------------------------------
    ! Declaration of parameters
    !-----------------------------------------------------------------------
    real(8) ::x0             ! starting value
    real(8) ::x              ! variable in the function
    real(8) ::lb             ! lower bound,
    real(8) ::h              ! step width,
    real(8) ::eps            ! precision
    real(8) ::f0             ! initial value of function
    real(8) ::fs0            ! derivative of function
    integer ::nit            ! Iteration counter
    integer ::maxitx         ! max no. of iterations

    !-----------------------------------------------------------------------
    ! Declaration of external functions
    !-----------------------------------------------------------------------
    real(8), external:: F  ! function
    real(8), external:: Fs ! derivative of the function

    !-----------------------------------------------------------------------
    nit = 0                  ! iteration counter
    x=x0                     ! initialize the iteration variable

    do
        nit = nit+1

        if(nit>maxitx) then
            newton=lb-10
            return
        end if

        ! --------------------------------------------------------------------
        ! function evaluation at x
        ! --------------------------------------------------------------------
        f0=F(x)

        if (abs(f0)<eps) then              !f(x) = 0: root found
            newton = x
            return
        end if

        ! --------------------------------------------------------------------
        ! Only for function 1 which involves a vanishing denominator
        ! --------------------------------------------------------------------
        if (abs(1-x**2)<eps) then
            newton=x+h                ! checking for the zero value of the
            return                    ! denominator of function 1
        end if

        ! --------------------------------------------------------------------
        ! check the derivative for vanishing slope
        ! --------------------------------------------------------------------
        fs0=Fs(F,x,h)

        if(abs(fs0)<=2*eps) then      ! f'(x) = 0
            newton=x+h                ! jump to the next value
            return
        end if

        ! --------------------------------------------------------------------
        ! Implement newtons algorithm
        ! --------------------------------------------------------------------
        x = x-f0/fs0                  ! calculate updated x value

    end do

end function
