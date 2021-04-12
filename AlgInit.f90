module AlgInit
    use Structures, only : node     ! import node type from Structures module

    implicit none

    private     ! declare that all things not declared public are private
    public :: Imax, Jmax, algebricGrid, node   ! declare public 

    integer :: Imax, Jmax, i, j
    real, parameter :: pi = real(22)/real(7)

contains
    ! define x at Left boundary at j
    function xL(j) result(r)
        implicit none
        integer, intent(in):: j
        real :: r
        r = 0
    end function xL

    ! define x at Right boundary at j
    function xR(j) result(r)
        implicit none
        integer, intent(in):: j
        real :: r
        r = 5
    end function xR

    ! define y at Upper boundary at i
    function yU(i) result(r)
        implicit none
        integer, intent(in):: i
        real :: r
        r = 1
    end function yU

    ! define y at Lower boundary at i
    function yL(i) result(r)
        implicit none
        integer, intent(in) :: i
        real :: temp_x, r
        temp_x = x_function(i, 1)
        if ((temp_x >= 0 .and. temp_x <= 2) .or. (temp_x >= 3 .and. temp_x <= 5)) then
            r = 0
        else if (temp_x > 2 .and. temp_x < 3) then
            r = 0.2 * sin((temp_x-2)*pi)
        end if
    end function yL

    ! calculate zeta at i
    function zeta(i) result(r)
        implicit none
        integer, intent(in) :: i
        real :: r
        r = real(i-1)/real(Imax-1)
    end function zeta

    ! calculate eta at i
    function eta(j) result(r)
        implicit none
        integer, intent(in) :: j
        real :: r
        r = real(j-1)/real(Jmax-1)
    end function eta

    ! calculate x at i & j
    function x_function(i, j) result(r)
        implicit none
        integer, intent(in) :: i, j
        real :: r
        r = xL(j) + zeta(i) * (xR(j) - xL(j))
    end function x_function

    ! calculate y at i & j
    function y_function(i, j) result(r)
        implicit none
        integer, intent(in) :: i, j
        real :: r
        r = yL(i) + eta(j) * (yU(i) - yL(i)) 
    end function y_function

    ! generate the algebric grid (Imax x Jmax) nodes
    function algebricGrid(Imax, Jmax) result(AlgNodes)
        implicit none
        integer, intent(in) :: Imax, Jmax
        type(node), dimension(:,:), allocatable :: AlgNodes
        
        allocate(AlgNodes(Imax, Jmax))
        do i = 1, Imax
            do j = 1, Jmax
                AlgNodes(i,j)%n = i + (j-1) * Imax
                AlgNodes(i,j)%zeta = zeta(i)
                AlgNodes(i,j)%eta = eta(j)
                AlgNodes(i,j)%x = x_function(i,j)
                AlgNodes(i,j)%y = y_function(i,j)
            end do 
        end do
    end function algebricGrid
end module AlgInit