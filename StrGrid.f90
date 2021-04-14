program StrGrid
    ! Import modules' variables, types or procedures
    use Structures, only : grid_object
    use AlgebricMod, only : genAlgebric
    use LaplaceMod, only : genLaplace
    use paraview, only: exportcsv

    implicit none
    ! integer :: i, j
    type(grid_object) :: AlgGrid
    type(grid_object) :: LapGrid

    ! Define no of nodes in x and Y directions
    AlgGrid%Imax = 90
    AlgGrid%Jmax = 15
    
    allocate(AlgGrid%nodes(AlgGrid%Imax,AlgGrid%Jmax))  ! locate memory for Algebric grid nodes

    ! Generate algebric grid
    call genAlgebric(AlgGrid)

    ! Generate Laplace grid
    LapGrid = AlgGrid  

    call genLaplace(LapGrid)

    ! ! Print x and y coordinates to the terminal window
    ! print '("x LGrid: "/(90f7.3))', ((LapGrid%nodes(i,j)%x, i=1, LapGrid%Imax), j = 1, LapGrid%Jmax)
    ! print '("y LGrid: "/(90f7.3))', ((LapGrid%nodes(i,j)%y, i=1, LapGrid%Imax), j = 1, LapGrid%Jmax)

    ! export grid into csv file
    call exportcsv(AlgGrid)
end program StrGrid
