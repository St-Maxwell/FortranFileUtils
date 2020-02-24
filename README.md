# FortranFileUtils
A trivial Fortran `file` object code (Python-like) 

# Usage

## open a file
```fortran
type(File) :: f
f = File(file_name, [mode], [stat])
```
`mode`:
* `r` -- read an existed file (default mode)
* `w` -- write an existed file and clean previous lines
* `x` -- create a new file, failing if file already exists
* `a` -- append new line to the end of file
* `+` -- read and write
  
`stat` (in return):
* `stat=0`, no error occured
* `stat!=0`, found error

## close a file
Usually, the destructor (finalizer) of `file` object is called when `file` goes out of scope (e.g. a subroutine), and the file will be close automatically.

But if necessary, you can close the `file` manually.

```fortran
subroutine automatically()
type(File) :: f
f = File(file_name, [mode], [stat])
...
end subroutine
```

```fortran
subroutine manually()
type(File) :: f
f = File(file_name, [mode], [stat])
...
call f%close([stat])
end subroutine
```

## read and write
```fortran
character(len=40) :: line
line = f%readline([stat])
```

```fortran
call f%write(line, [stat], [end])
```
The default value of `end` is `.true.`, which means after writing a line to file, the cursor will point to the next new line. If `end` is set `.false`, the cursor stays at the tail of this line.

## move
```fortran
call f%seek(offset, [stat])
call f%seek(0) ! move to the begining of file
call f%seek(2) ! move to the second line

call f%next([num_lines], [stat])
call f%next() ! move to next line
call f%next(2) ! move twice

call f%back([num_lines], [stat])
call f%back() ! move back
call f%back(2) ! move back twice
```

## print matrix
```fortran
call f%print_matrix(mat, [title]) ! only available for single precision and double precision matrix
```

# Examples
See [test/examples](test/examples.f90)