program StrGrid
    ! Import modules' variables, types or procedures
    use Structures, only : grid_object
    use AlgebricMod, only : genAlgebric
    use LaplaceMod, only : genLaplace
    use paraview, only: exportcsv, exportvtk

    implicit none
    ! integer :: i, j
    type(grid_object) :: AlgGrid
    type(grid_object) :: LapGrid

    ! Define no of nodes in x and Y directions
    AlgGrid%Imax = 800
    AlgGrid%Jmax = 200
    
    allocate(AlgGrid%nodes(AlgGrid%Imax,AlgGrid%Jmax))  ! locate memory for Algebric grid nodes

    ! Generate algebric grid
    call genAlgebric(AlgGrid)

    ! Generate Laplace grid
    LapGrid = AlgGrid  
    call genLaplace(LapGrid)

    ! export grid into vtk file
    call exportvtk(grid=LapGrid, fileName='laplace grid 400x100.vtk', format="STRUCTURED_GRID")
end program StrGrid
