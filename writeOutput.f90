! #################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! - Generating Output Data
! #################################################################e
!---------------------------------------------------------------------------------

logical function writeOutput(name,lb,ub,eps,maxitx,maxroots,sw,h,epsd,nroots,p)
    implicit none

    ! -----------------------------------------------------------------------------
    ! Declaration of Controlling parameters
    ! -----------------------------------------------------------------------------
    character(*):: name                   ! Files name
    integer     :: nroots                 ! Number of found roots
    integer     :: rc                     ! root counter
    integer     :: iwriteerr              ! memory status code
    integer     :: maxitx                 ! maximum number of iterations
    integer     :: maxroots               ! max. number of roots to find
    real(8)     :: lb                     ! lower limit
    real(8)     :: ub                     ! upper limit
    real(8)     :: eps                    ! precision
    real(8)     :: sw                     ! step width
    real(8)     :: h                      ! derivative step width
    real(8)     :: epsd                   ! precision of the derivative
    real(8),dimension(maxroots)::p        ! Array to save the roots

    ! -----------------------------------------------------------------------------
    ! open the file
    ! -----------------------------------------------------------------------------
    open (20,file = name,status = 'replace',iostat = iwriteerr)
    if (iwriteerr /= 0) then
        writeOutput = .False.
        return
    endif

    ! -----------------------------------------------------------------------------
    ! To write the roots found in ScanForRootsOutput.out
    ! -----------------------------------------------------------------------------
    if (nroots .eq. 0) then
        write (20,*) "Input parameters:           "
        write (20,*) "------------------------------------------------"
        write (20, '(a,f12.3)')   " Lower boundary               :", lb
        write (20, '(a,f12.3)')   " Upper boundary               :", ub
        write (20, '(a,f12.3)')   " Step Width                   :", sw
        write (20, '(a,f17.8)')   " Precision                    :", eps
        write (20, '(a,f12.3)')   " Derivation Step Width        :", h
        write (20, '(a,i8)')      " Maximum Number of Iteration  :", maxitx
        write (20, '(a,f17.8)')   " Precision of derivative      :", epsd
        write (20,*) "------------------------------------------------"
        write (20,*) "Output      :         "
        write (20,*) "------------------------------------------------"
        write (20,*) " Total no of roots found."
        write(20,'(a)')"------------------------------------------------"
        close(20)
    else
        write (20,*) "Input parameters:           "
        write (20,*) "------------------------------------------------"
        write (20, '(a,f12.3)')   " Lower boundary               :", lb
        write (20, '(a,f12.3)')   " Upper boundary               :", ub
        write (20, '(a,f12.3)')   " Step Width                   :", sw
        write (20, '(a,f17.8)')   " Precision                    :", eps
        write (20, '(a,f12.3)')   " Derivation Step Width        :", h
        write (20, '(a,i8)')      " Maximum Number of Iteration  :", maxitx
        write (20, '(a,i8)')      " Maximum Number of Roots      :", maxroots
        write (20, '(a,f17.8)')   " Precision of derivative      :", epsd
        write (20,*) "------------------------------------------------"
        write (20,*) "Output      :         "
        write (20,*) "------------------------------------------------"
        write(20,'(a,i4)') ' Total no of roots found             :',nroots
        write (20,*) "------------------------------------------------"
        write(20,'(a)')"       n     root of f(x)    "
        write(20,'(a)')" -----------------------------------------------"

        do rc=1,nroots
            write(20,'(i7,f11.3)') rc,p(rc)
        end do

    ! -----------------------------------------------------------------------------
    ! To write the roots found in Console window
    ! -----------------------------------------------------------------------------
        write (*,*) "---------------------------------------------------"
        write(*,*) " The Roots are: "
        do rc=1,nroots
            write(*,'(f11.3)') p(rc)
        end do
    end if

    ! -----------------------------------------------------------------------------
    ! close the output file
    ! -----------------------------------------------------------------------------
    close(20)

    ! -----------------------------------------------------------------------------
    ! return code
    ! -----------------------------------------------------------------------------
    writeOutput = .True.

end function writeOutput



