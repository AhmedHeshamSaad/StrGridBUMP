program StrGrid
    implicit none
    integer :: Imax, Jmax, i, j
    real, parameter :: pi = real(22)/real(7)
    
    type node
        real :: x
        real :: y
    end type node
    type(node), dimension(:), allocatable :: AlgNodes

    Imax = 30
    Jmax = 15

    allocate(AlgNodes(1:Imax*Jmax))
    AlgNodes = algebricGrid(Imax, Jmax)

    print '("x Grid: "/(30f7.3))', (AlgNodes(i)%x, i=1,Imax*Jmax)
    print '("y Grid: "/(30f7.3))', (AlgNodes(i)%y, i=1,Imax*Jmax)

    contains

    function xL(j) result(r)
        implicit none
        integer, intent(in):: j
        real :: r
        r = 0
    end function xL

    function xR(j) result(r)
        implicit none
        integer, intent(in):: j
        real :: r
        r = 5
    end function xR

    function yU(i) result(r)
        implicit none
        integer, intent(in):: i
        real :: r
        r = 1
    end function yU

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

    function zeta(i) result(r)
        implicit none
        integer, intent(in) :: i
        real :: r
        r = real(i-1)/real(Imax-1)
    end function zeta

    function eta(j) result(r)
        implicit none
        integer, intent(in) :: j
        real :: r
        r = real(j-1)/real(Jmax-1)
    end function eta

    function x_function(i, j) result(r)
        implicit none
        integer, intent(in) :: i, j
        real :: r
        r = xL(j) + zeta(i) * (xR(j) - xL(j))
    end function x_function

    function y_function(i, j) result(r)
        implicit none
        integer, intent(in) :: i, j
        real :: r
        r = yL(i) + eta(j) * (yU(i) - yL(i)) 
    end function y_function

    function algebricGrid(Imax, Jmax) result(AlgNodes)
        implicit none
        integer, intent(in) :: Imax, Jmax
        integer :: n
        type(node), dimension(:), allocatable :: AlgNodes
        n = Imax * Jmax
        allocate(AlgNodes(1:n))
        do i = 1, Imax
            do j = 1, Jmax
                AlgNodes(i + (j-1) * Imax)%x = x_function(i,j)
                AlgNodes(i + (j-1) * Imax)%y = y_function(i,j)
    
                ! print 100, i + (j-1) * Imax, i, j, AlgNodes(i + (j-1) * Imax)%x, AlgNodes(i + (j-1) * Imax)%y
                ! 100 format('Node number:', i3,' i= ', i2, ' j= ', i2, ' x= ', f7.4, ' y= ', f7.4)
            end do 
        end do
    end function algebricGrid

end program StrGrid
