module AlgebricMod
    use constants, only : pi
    use Structures, only : grid_object     ! import node type from Structures module

    implicit none
    private                 ! declare that all things not declared public are private
    public :: genAlgebric   ! declare public 

    integer :: Imax, Jmax, i, j

contains
    ! define x at Left boundary at j
    function xL() result(retval)
        implicit none
        ! integer, intent(in):: j
        real :: retval
        retval = 0
    end function xL

    ! define x at Right boundary at j
    function xR() result(retval)
        implicit none
        ! integer, intent(in):: j
        real(8) :: retval
        retval = 5
    end function xR

    ! define y at Upper boundary at i
    function yU() result(retval)
        implicit none
        ! integer, intent(in):: i
        real(8) :: retval
        retval = 1
    end function yU

    ! define y at Lower boundary at i
    function yL(i) result(retval)
        implicit none
        integer, intent(in) :: i
        real(8) :: temp_x, retval
        temp_x = x_function(i)
        if ((temp_x >= 0 .and. temp_x <= 2) .or. (temp_x >= 3 .and. temp_x <= 5)) then
            retval = 0
        else if (temp_x > 2 .and. temp_x < 3) then
            retval = 0.20d0 * sin((temp_x-2)*pi)
        end if
    end function yL

    ! calculate zeta at i
    function zeta(i) result(retval)
        implicit none
        integer, intent(in) :: i
        real(8) :: retval
        retval = real(i-1)/real(Imax-1)
    end function zeta

    ! calculate eta at i
    function eta(j) result(retval)
        implicit none
        integer, intent(in) :: j
        real(8) :: retval
        retval = real(j-1,8)/real(Jmax-1,8)
    end function eta

    ! calculate x at i 
    function x_function(i) result(retval)
        implicit none
        integer, intent(in) :: i
        real(8) :: retval
        retval = xL() + zeta(i) * (xR() - xL())
    end function x_function

    ! calculate y at i & j
    function y_function(i, j) result(retval)
        implicit none
        integer, intent(in) :: i, j
        real(8) :: retval
        retval = yL(i) + eta(j) * (yU() - yL(i)) 
    end function y_function

    ! generate the algebric grid (Imax x Jmax) nodes
    subroutine genAlgebric(AlgGrid) 
        implicit none
        type(grid_object) :: AlgGrid
        real(8), dimension(:), allocatable :: temp
        integer :: MinIndex
        
        Imax = AlgGrid%Imax
        Jmax = AlgGrid%Jmax

        do i = 1, Imax
            do j = 1, Jmax
                AlgGrid%nodes(i,j)%n = i + (j-1) * Imax
                AlgGrid%nodes(i,j)%zeta = zeta(i)
                AlgGrid%nodes(i,j)%eta = eta(j)
                AlgGrid%nodes(i,j)%x = x_function(i)
                AlgGrid%nodes(i,j)%y = y_function(i,j)
            end do 
        end do

        ! fix position of LE
        allocate(temp(Imax))
        temp = abs(AlgGrid%nodes(:,1)%x - 2)
        MinIndex = MINLOC(temp, 1)
        AlgGrid%nodes(MinIndex,1)%x = 2.0d0
        AlgGrid%nodes(MinIndex,1)%y = 0.0d0

        ! fix position of TE
        temp = abs(AlgGrid%nodes(:,1)%x - 3)
        MinIndex = MINLOC(temp, 1)
        AlgGrid%nodes(MinIndex,1)%x = 3.0d0
        AlgGrid%nodes(MinIndex,1)%y = 0.0d0

    end subroutine genAlgebric
end module AlgebricMod