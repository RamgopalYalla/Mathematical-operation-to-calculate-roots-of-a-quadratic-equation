! #################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! - Implementation of Newton's Algorithm
! #################################################################
!---------------------------------------------------------------------------

program main

    implicit none
    !-----------------------------------------------------------------------
    ! Declaration of external functions
    !-----------------------------------------------------------------------
    real(8),external::F             ! function to read function
    real(8),external::Fs            ! function to find the derivative
    logical,external::readData      ! function to read the input parameters
    integer,external::ScanForRoots  ! function to find the roots
    logical,external::writeOutput   ! function to write the roots

    !-----------------------------------------------------------------------
    ! Declaration of parameters
    !-----------------------------------------------------------------------
    real(8)         ::lb            ! lower limit
    real(8)         ::ub            ! upper limit
    real(8)         ::eps           ! precision
    real(8)         ::sw            ! step width
    real(8)         ::h             ! derivative step width
    real(8)         ::epsd          ! precision of the derivative
    integer         ::maxitx        ! maximum number of iterations
    integer         ::maxroots      ! max. number of roots to find
    integer         ::nroots        ! root counter

    !-----------------------------------------------------------------------
    ! Assigning the read and write files
    !-----------------------------------------------------------------------
    character(256)  ::inpFile = "ScanForRootsInp.inp"  ! input file name
    character(256)  ::outFile = "ScanForRootSOut.out"  ! output file name

    !-----------------------------------------------------------------------
    ! Assigning of array to store the roots
    !-----------------------------------------------------------------------
    real(8),allocatable,dimension(:):: p    ! Array to save the roots

    !-------------------------------------------------------------------
    ! Examine the command line parameters
    !-------------------------------------------------------------------
    integer         ::numargs
    integer         ::i
    character(256)  ::arg

    numargs = iargc()
    do i=0,numargs
        call getarg(i,arg)
        !write(*,'(i2,x,a)') i,arg(1:len_trim(arg))

        if (i==1) then
            inpFile = arg
        end if
    end do
    write (*,*) "CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034"
    write (*,*) "==================================================="
    !write (*,*)   " function F(x)              :"
    ! ----------------------------------------------------------------------
    write (*,*) "          SCANNING FUNCTION FOR ITS ROOTS          "
    write (*,*) "---------------------------------------------------"
    write (*,*) " Implementation of Newton's Algorithm to Scan roots"
    write (*,*) "              For a given function f(x)            "
    write (*,*) "---------------------------------------------------"
    ! ----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    ! Read and print input data
    !-----------------------------------------------------------------------
    if (.not. readData(inpFile,lb,ub,eps,maxitx,maxroots,sw,h,epsd)) then
        write(*,*) '*** fatal error: stop'
        stop
    else
        write (*,*) "Input parameters:           "
        write (*,*) "---------------------------------------------------"
        write (*, '(a,f12.3)')   " Lower boundary               :", lb
        write (*, '(a,f12.3)')   " Upper boundary               :", ub
        write (*, '(a,f12.3)')   " Step Width                   :", sw
        write (*, '(a,f17.8)')   " Precision                    :", eps
        write (*, '(a,f12.3)')   " Derivation Step Width        :", h
        write (*, '(a,i8)')      " Maximum Number of Iteration  :", maxitx
        write (*, '(a,i8)')      " Maximum Number of Roots      :", maxroots
        write (*, '(a,f17.8)')   " Precision of derivative      :", epsd
        !write (*,*) "---------------------------------------------------"
    end if

    !-------------------------------------------------------------------
    ! Assign a array to store the roots
    !-------------------------------------------------------------------
    allocate(p(maxroots))

    !-------------------------------------------------------------------
    ! Call for ScanForRoots function to find the roots
    !-------------------------------------------------------------------
    nroots = ScanForRoots(lb,ub,eps,maxitx,maxroots,sw,h,p)

    if (nroots .ne. 0) then
        ! print total number of roots
        write (*,*) ""
        write (*,*) "==================================================="
        write(*,'(a,i4)') ' Total no of roots found      :    ',nroots
        write (*,*) "==================================================="
    else
        ! print error message
        !write (*,*) "------------------------------------------------"
        write(*,'(a,f6.2,f6.2)') ' Error: No root found for the interval: ',lb,ub
    end if

    ! ---------------------------------------------------------------------
    ! Write the root's into the Output file
    ! ---------------------------------------------------------------------
    if (.not. writeOutput(outFile,lb,ub,eps,maxitx,maxroots,sw,h,epsd,nroots,p)) then
        write (*,*) "---------------------------------------------------"
        write(*,'(a)') " *** Error listing the Results in Output file"
        write (*,*) "---------------------------------------------------"
    else
        write (*,*) "---------------------------------------------------"
        write(*,'(a)') "Output file is generated successfully"
        write (*,*) "---------------------------------------------------"
    end if

    ! --------------------------------------------------------------------
    write(*,'(a)') " "
    write (*,*) " ************ Program Terminates here ************* "

    deallocate(p)

end program
