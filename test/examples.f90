program main
    implicit none
    
    call test_read()
    call test_write()
    call test_new()
    call test_append()
    call test_readwrite()
    call manually_close()
    call wrong_mode()
    
end program main

subroutine test_read()
    use fortran_file
    implicit none
    type(File) :: f
    character(len=50) :: line
    integer :: stat

    write(*,*) "Opening file in 'r' mode"
    f = File("./files/read.txt", 'r', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'r' mode"
    else 
        write(*,*) "Failed to open in 'r' mode"
    end if
    
    line = f%readline(stat)
    if (stat == 0) then
        write(*,*) line
    end if
    line = f%readline(stat)
    if (stat == 0) then
        write(*,*) line
    end if

end subroutine test_read

subroutine test_write()
    use fortran_file
    implicit none
    type(File) :: f
    integer :: stat

    write(*,*) "Opening file in 'w' mode"
    f = File("./files/write.txt", 'w', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'w' mode"
    else 
        write(*,*) "Failed to open in 'w' mode"
    end if
    
    call f%write("# first", stat)
    call f%write("# second", stat)
    call f%write("# third", stat)
    call f%write("first", stat, end=.false.)
    call f%write("second", stat, end=.false.)
    call f%write("third", stat, end=.false.)

end subroutine test_write

subroutine test_new()
    use fortran_file
    implicit none
    type(File) :: f
    integer :: stat

    write(*,*) "Opening file in 'x' mode"
    f = File("./files/new.txt", 'x', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'x' mode"
    else
        write(*,*) "Failed to open in 'x' mode"
    end if

end subroutine test_new

subroutine test_append()
    use fortran_file
    implicit none
    type(File) :: f
    integer :: stat

    write(*,*) "Opening file in 'a' mode"
    f = File("./files/append.txt", 'a', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'a' mode"
    else 
        write(*,*) "Failed to open in 'a' mode"
    end if

    call f%write("# first", stat)
    call f%write("# second", stat)
    call f%write("# third", stat)

end subroutine test_append

subroutine test_readwrite()
    use fortran_file
    use iso_fortran_env, only: r4 => real32
    implicit none
    type(File) :: f
    character(len=50) :: line
    real(kind=r4), dimension(3,3) :: mat = reshape([1.,2.,3.,4.,5.,6.,7.,8.,9.], [3,3])
    integer :: stat

    write(*,*) "Opening file in '+' mode"
    f = File("./files/readwrite.txt", '+', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in '+' mode"
    else 
        write(*,*) "Failed to open in '+' mode"
    end if

    line = f%readline(stat)
    if (stat == 0) then
        write(*,*) line
    end if

    call f%seek(0)
    call f%write("# first", stat)
    call f%print_matrix(mat, title="print matrix:")

end subroutine test_readwrite

subroutine manually_close()
    use fortran_file
    implicit none
    type(File) :: f
    integer :: stat

    write(*,*) "Opening file in 'r' mode"
    f = File("./files/read.txt", 'r', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'r' mode"
    else 
        write(*,*) "Failed to open in 'r' mode"
    end if

    call f%close(stat)
    if (stat == 0) then
        write(*,*) "Successfully close file"
    else 
        write(*,*) "Failed to close file"
    end if

end subroutine manually_close

subroutine wrong_mode()
    use fortran_file
    implicit none
    type(File) :: f
    integer :: stat

    write(*,*) "Wrong mode example"
    f = File("./files/read.txt", 'q', stat)
    if (stat == 0) then
        write(*,*) "Successfully open in 'q' mode"
    else 
        write(*,*) "Failed to open in 'q' mode"
    end if

end subroutine wrong_mode