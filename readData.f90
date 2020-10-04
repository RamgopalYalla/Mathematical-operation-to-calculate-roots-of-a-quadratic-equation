! #################################################################################
! CLFE_RAMGOPAL YALLA; Matriculation. Nr: 03040034
! Newton's Method to evaluate root of the equation
! - Parameters to be considered from input data
! #################################################################################
!----------------------------------------------------------------------------------

logical function readData(name,lb,ub,eps,maxitx,maxroots,sw,h,epsd)

    implicit none

    ! -----------------------------------------------------------------------------
    ! Declaration of parameters
    ! -----------------------------------------------------------------------------
    real(8)         ::lb                          ! starting value
    real(8)         ::ub                          ! ending value
    real(8)         ::eps                         ! precision
    real(8)         ::sw                          ! step aside
    real(8)         ::h                           ! derivative step width
    real(8)         ::epsd                        ! precision of the derivative
    integer         ::io = 10                     ! channel to read
    integer         ::ioerr                       ! return information of the open
    integer         ::maxitx                      ! maximum number of iterations
    integer         ::maxroots                    ! max. number of roots to find
    integer         ::errors = 0                  ! error counter
    integer         ::line = 0                    ! line number
    character(256)  ::key                         ! key
    character(256)  ::buffer                      ! input buffer
    character(*)    ::name                        ! 90 version

    ! -----------------------------------------------------------------------------
    ! open the input file
    ! -----------------------------------------------------------------------------
    open(io,file=name,status='old',iostat=ioerr)
    if (ioerr /= 0) then
        write (*,*) "Input File not readable"
        readData = .False.
        return
    end if

    ! -----------------------------------------------------------------------------
    ! read the parameters from the input file
    ! -----------------------------------------------------------------------------
    read(io,*,iostat=ioerr) lb,ub,eps,maxitx,maxroots,sw,h,epsd

    ! -----------------------------------------------------------------------------
    ! Input data reading and error handling
    ! -----------------------------------------------------------------------------
    do while(.True.)

        ! Read next line into a buffer
        line = line +1
        read(io,'(a256)',iostat=ioerr) buffer
        if (ioerr /= 0) then
            exit
        end if

        ! Ignore empty lines
        if (len_trim(buffer) < 1)   cycle

        ! Ignore comment lines
        if (buffer(1:1) == "#")     cycle

        ! Get the key
        read(buffer,*) key

        ! Analyze the key and read value
        if (key == "lb") then
            read(buffer,*,iostat=ioerr) key,lb
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for lower limit of search interval (lb)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "ub") then
            read(buffer,*,iostat=ioerr) key,ub
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for upper limit of search interval (ub)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "eps") then
            read(buffer,*,iostat=ioerr) key,eps
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for Precision (eps)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "maxitx") then
            read(buffer,*,iostat=ioerr) key,maxitx
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for max number of Iterations (itx)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "maxroots") then
            read(buffer,*,iostat=ioerr) key,maxroots
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for max number of roots (maxroots)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "sw") then
            read(buffer,*,iostat=ioerr) key,sw
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for step width of x (sw)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "h") then
            read(buffer,*,iostat=ioerr) key,h
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for step width of derivative (h)"
                write(*,*) "------------------------------------------------"
            end if

        elseif (key == "epsd") then
            read(buffer,*,iostat=ioerr) key,epsd
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** error in line ",line," of the Input file:"
                write(*,*) "      Data error for Precision of derivative (epsd)"
                write(*,*) "------------------------------------------------"
            end if

        end if

    end do

    ! -----------------------------------------------------------------------------
    ! close the input file
    ! -----------------------------------------------------------------------------
    close(io)

    if (errors > 0) then
        readData = .False.
    else
        readData = .True.
    endif

end function
