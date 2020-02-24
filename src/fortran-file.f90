! inspired by https://github.com/fortran-lang/stdlib/issues/14
module fortran_file
    use iso_fortran_env, only: r4 => real32, r8 => real64
    implicit none
    private
    public :: File

    integer, parameter :: MAX_LINE_WIDTH = 150

    type :: File
        integer :: unit
    contains
        procedure :: readline => read_file
        procedure :: write => write_file
        procedure :: close => close_file
        procedure :: next => next_file
        procedure :: back => back_file
        procedure :: seek => seek_file
        generic :: print_matrix => print_matrix_r4, print_matrix_r8
        procedure :: print_matrix_r4, print_matrix_r8
        final :: destroy_file
    end type File

    interface File
        module procedure :: open_file
    end interface

    interface optval
        module procedure :: optval_int
        module procedure :: optval_r8
        module procedure :: optval_char
        module procedure :: optval_logic
    end interface

contains

    function open_file(file_name, mode, stat) result(f)
        character(len=*), intent(in) :: file_name
        character(len=*), optional :: mode
            !! the mode in which file is opened
            !! 'r' -- read an existed file
            !! 'w' -- write an existed file and clean previous file
            !! 'x' -- create a new file, failing if file already exists
            !! 'a' -- append new line to the end of file
            !! '+' -- read and write
        integer, optional :: stat
        type(File) :: f
        integer :: ierr

        select case (optval(mode, 'r')) ! default mode is 'r' (read)
        case ('r')
            open(newunit=f%unit, file=file_name, action="read", status="old", &
                 form="formatted", position="rewind", iostat=ierr)
        case ('w')
            open(newunit=f%unit, file=file_name, action="write", status="replace", &
                 form="formatted", position="rewind", iostat=ierr)
        case ('x')
            open(newunit=f%unit, file=file_name, action="readwrite", status="new", &
                 form="formatted", position="rewind", iostat=ierr)
        case ('a')
            open(newunit=f%unit, file=file_name, action="write", status="unknown", &
                 form="formatted", position="append", iostat=ierr)
        case ('+')
            open(newunit=f%unit, file=file_name, action="readwrite", status="unknown", &
                 form="formatted", position="asis", iostat=ierr)
        case default
            !error stop "invalid mode: '" // mode // "'"
            ierr = 1 ! set error code
        end select

        if (present(stat)) stat = ierr

    end function open_file
    

    subroutine close_file(this, stat)
        class(File), intent(inout) :: this
        integer, optional :: stat
        integer :: ierr

        close(this%unit, iostat=ierr)

        if (present(stat)) stat = ierr

    end subroutine close_file


    subroutine delete_file(this, stat)
        class(File), intent(inout) :: this
        integer, optional :: stat
        integer :: ierr

        close(this%unit, status="delete", iostat=ierr)

        if (present(stat)) stat = ierr

    end subroutine delete_file


    subroutine destroy_file(this)
       type(File), intent(inout) :: this

       call this%close()

    end subroutine destroy_file


    function read_file(this, stat) result(line)
        class(File), intent(inout) :: this
        integer, optional :: stat
        character(len=:), allocatable :: line
        character(len=MAX_LINE_WIDTH) :: tmp_line
        integer :: ierr

        read(this%unit, "(A)", iostat=ierr) tmp_line
        if (present(stat)) stat = ierr

        line = trim(tmp_line)

    end function read_file


    subroutine write_file(this, line, stat, end)
        class(File), intent(inout) :: this
        character(len=*), intent(in) :: line
        logical, optional :: end
        integer, optional :: stat
        integer :: ierr

        if (optval(end, .true.)) then
            ! entering to the next line is default
            write(this%unit, "(A)", iostat=ierr) line
        else
            write(this%unit, "(A)", advance="no", iostat=ierr) line
        end if

        if (present(stat)) stat = ierr

    end subroutine write_file

    
    subroutine next_file(this, num_lines, stat)
        class(File), intent(in) :: this
        integer, optional :: num_lines
        integer, intent(inout), optional :: stat
        integer :: ierr
        integer :: i

        do i = 1, optval(num_lines, 1)
            read(this%unit, "(A)", iostat=ierr)
        end do
        if (present(stat)) stat = ierr

    end subroutine next_file


    subroutine back_file(this, num_lines, stat)
        class(File), intent(in) :: this
        integer, optional :: num_lines
        integer, intent(inout), optional :: stat
        integer :: ierr
        integer :: i

        do i = 1, optval(num_lines, 1)
            backspace(this%unit, iostat=ierr)
        end do
        if (present(stat)) stat = ierr

    end subroutine back_file


    subroutine seek_file(this, offset, stat)
        class(File), intent(in) :: this
        integer, intent(in) :: offset
        integer, intent(inout), optional :: stat
        integer :: ierr
        integer :: i

        rewind(this%unit)
        do i = 0, offset
            call this%next(ierr)
        end do
        if (present(stat)) stat = ierr

    end subroutine seek_file


    subroutine print_matrix_r4(this, mat, title)
        class(File), intent(in) :: this
        real(kind=r4), dimension(:,:), intent(in) :: mat
        character(len=*), optional :: title
        integer :: i
    
        if (present(title)) write(this%unit,"(A)") title
        do i = lbound(mat, dim=1), ubound(mat, dim=1)
            write(this%unit, "(*(g0,'  '))") mat(i,:)
        end do
    
    end subroutine print_matrix_r4


    subroutine print_matrix_r8(this, mat, title)
        class(File), intent(in) :: this
        real(kind=r8), dimension(:,:), intent(in) :: mat
        character(len=*), optional :: title
        integer :: i

        if (present(title)) write(this%unit,"(A)") title
        do i = lbound(mat, dim=1), ubound(mat, dim=1)
            write(this%unit, "(*(g0,'  '))") mat(i,:)
        end do

    end subroutine print_matrix_r8

!===========================================================
! from https://github.com/fortran-lang/stdlib/issues/62
! return default value if opt is not presented
    function optval_int(opt, default) result(val)
        integer, intent(inout), optional :: opt
        integer, intent(in) :: default
        integer :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_int

    function optval_r8(opt, default) result(val)
        real(kind=r8), intent(inout), optional :: opt
        real(kind=r8), intent(in) :: default
        real(kind=r8) :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if
        
    end function optval_r8

    function optval_char(opt, default) result(val)
        character(len=*), intent(inout), optional :: opt
        character(len=*), intent(in) :: default
        character(len=:), allocatable :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if

    end function optval_char

    function optval_logic(opt, default) result(val)
        logical, intent(inout), optional :: opt
        logical, intent(in) :: default
        logical :: val

        if (present(opt)) then
            val = opt
        else
            val = default
        end if
        
    end function optval_logic

end module fortran_file