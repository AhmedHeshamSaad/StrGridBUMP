module Structures
    implicit none

    type :: node_object
        integer :: n
        real(8) :: zeta
        real(8) :: eta
        real(8) :: x
        real(8) :: y
    end type node_object

    type :: grid_object
        integer :: Imax, Jmax
        type(node_object), dimension(:,:), allocatable :: nodes

    contains
        procedure :: d_zeta
        procedure :: d_eta
    end type grid_object

contains

    function d_zeta(g) result(retval)
        class(grid_object), intent(in) :: g
        real :: retval

        retval = real(1) / (g%Imax-1)
    end function d_zeta
    
    function d_eta(g) result(retval)
        class(grid_object), intent(in) :: g
        real :: retval

        retval = real(1)  / (g%Jmax-1)
    end function d_eta
end module Structures