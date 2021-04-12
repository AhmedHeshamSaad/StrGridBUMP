program StrGrid
    ! Import modules' variables, types or procedures
    use AlgInit, only : Imax, Jmax, algebricGrid
    use Structures, only : node

    implicit none
    integer :: i, j
    type(node), dimension(:,:), allocatable :: AlgNodes

    ! Define no of nodes in x and Y directions
    Imax = 30
    Jmax = 15

    allocate(AlgNodes(Imax,Jmax))           ! locate memory for AlgNodes
    AlgNodes = algebricGrid(Imax, Jmax)     ! Create algebric grid

    ! Print x and y coordinates to the terminal window
    print '("x Grid: "/(30f7.3))', ((AlgNodes(i,j)%x, i=1, Imax), j = 1, Jmax)
    print '("y Grid: "/(30f7.3))', ((AlgNodes(i,j)%y, i=1, Imax), j = 1, Jmax)

end program StrGrid
