module paraview
    use Structures, only : grid_object
    implicit none
    
contains
    subroutine exportcsv(grid)
        type(grid_object) :: grid
        Integer :: n, i, j, Imax, Jmax
        ! Open the file after generation using a notepad. Then, click "Ctrl+A", "Shift+Tab", and "Ctrl+S". Then, close it.
        open(UNIT = 4, FILE = 'laplace grid 90x15.csv', STATUS = 'REPLACE')
        write(4,*)'x coord, y coord, z coord, scalar'
        Imax = grid%Imax
        Jmax = grid%Jmax
        do n = 1, Imax*Jmax
            j = NINT(real(n)/real(Imax))
            i = n - Imax * (j -1)
            write(4,*) grid%nodes(i,j)%x,', ', grid%nodes(i,j)%y,', 0', ', ', n-1
        end do
    end subroutine exportcsv
    
end module paraview