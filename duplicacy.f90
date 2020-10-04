! #####################################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! #####################################################################################

!--------------------------------------------------------------------------------------

subroutine duplicacy(x_new,eps,ub,lb,maxroots,p,ScanForRoots)

    implicit none

    !-----------------------------------------------------------------------------------
    ! Declaration of parameters
    !-----------------------------------------------------------------------------------
    real(8)         ::x_new                  ! roots from newton function
    real(8)         ::eps                    ! precision, upper bound, lower bound
    real(8)         ::lb                     ! lower limit
    real(8)         ::ub                     ! upper limit
    integer         ::maxroots               ! max roots to find, ScanForRoots function
    integer         ::ScanForRoots           ! function to find the roots
    integer         ::i
    real(8), dimension(maxroots) :: p        ! array of roots

    !----------------------------------------------------------------------------------
    ! Given function
    !----------------------------------------------------------------------------------
    real(8), external :: F

    if (abs(F(x_new)) < eps .and. x_new<=ub .and. x_new>=lb) then

        do i=1,maxroots

            if (abs(x_new-p(i))<(0.01)) return       ! check for repetitive of roots

            if (i==maxroots) then
                ScanForRoots = ScanForRoots+1        ! add a new memory for the new root
                p(ScanForRoots)=x_new                ! add the new non-duplicate root

            if (ScanForRoots==maxroots) return       ! return if we reach the limit of number of roots

            end if

        end do

    end if

end subroutine
