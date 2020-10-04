! #################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! #################################################################
!---------------------------------------------------------------------------

integer function ScanForRoots(lb,ub,eps,maxitx,maxroots,sw,h,p)

    implicit none

    ! -----------------------------------------------------------------------------
    ! Declaration of parameters
    ! -----------------------------------------------------------------------------
    real(8) :: lb          ! lower bound
    real(8) :: ub          ! upper bound
    real(8) :: sw          ! stepwidth
    real(8) :: eps         ! precision
    real(8) :: h           ! stepwidth
    integer :: maxitx      ! max no. of iterations
    integer :: maxroots    ! max number of roots to be found

    !-------------------------------------------------------------------
    ! A array to store the roots
    !-------------------------------------------------------------------
    real(8), dimension(maxroots) :: p   !array for the roots found

    !-----------------------------------------------------------------------
    ! Declaration of external functions
    !-----------------------------------------------------------------------
    real(8), external :: newton

    !-----------------------------------------------------------------------
    ! Newton method variables
    !-----------------------------------------------------------------------
    real(8) :: x0            ! initial value
    real(8) :: x_new         ! root of the function
    integer :: i

    !-----------------------------------------------------------------------
    ! Initialize the number of found roots
    !-----------------------------------------------------------------------
    ScanForRoots=0

    do i=1,maxroots
        p(i) = lb-10
    end do

    !-----------------------------------------------------------------------
    ! initial value
    !-----------------------------------------------------------------------
    x0=lb-sw

    do
        x0 = x0 + sw
        if (x0>ub) return
        !-------------------------------------------------------------------
        ! Call function 'Newton'
        !-------------------------------------------------------------------
        x_new = newton(lb,x0,h,maxitx,eps)

        !-------------------------------------------------------------------
        ! check for root redundancy
        !-------------------------------------------------------------------
        call duplicacy(x_new,eps,ub,lb,maxroots,p,ScanForRoots)

        if (ScanForRoots==maxroots) return                         !max roots found

    end do

end function
