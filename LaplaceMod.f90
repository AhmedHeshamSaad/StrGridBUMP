module LaplaceMod
    use constants, only : pi
    use Structures, only : grid_object
    implicit none
    private
    public :: genLaplace
    type(grid_object) :: grid
    ! real(8), parameter :: pi = real(22,8)/real(7,8)

contains
    function alpha(i,j) result(retval)
        implicit none
        integer, intent(in) :: i, j       
        real(8) :: retval

        retval =    ( (grid%nodes(i,j+1)%x - grid%nodes(i,j-1)%x)**2 +      &
                      (grid%nodes(i,j+1)%y - grid%nodes(i,j-1)%y)**2 ) /    &
                    (  4 * (grid%d_eta())**2                         )
    end function alpha

    function beta(i,j) result(retval)
        implicit none
        integer, intent(in) :: i, j
        real(8) :: retval

        retval =    ( (grid%nodes(i+1,j)%x - grid%nodes(i-1,j)%x) *         &
                        (grid%nodes(i,j+1)%x - grid%nodes(i,j-1)%x) +       &
                      (grid%nodes(i+1,j)%y - grid%nodes(i-1,j)%y) *         &
                        (grid%nodes(i,j+1)%y - grid%nodes(i,j-1)%y)    ) /  &
                    (  4 * (grid%d_eta()) * (grid%d_zeta())            )
    end function beta

    function jamma(i,j) result(retval)
        implicit none
        integer, intent(in) :: i, j
        real(8) :: retval

        retval =    ( (grid%nodes(i+1,j)%x - grid%nodes(i-1,j)%x)**2 +      &
                      (grid%nodes(i+1,j)%y - grid%nodes(i-1,j)%y)**2 ) /    &
                    (  real(4) * (grid%d_zeta())**2                         )
    end function jamma

    subroutine genLaplace(lg)
        implicit none
        type(grid_object) :: lg
        real(8) :: e, dzeta, deta,  den, a1, a2, &
            a3, a4, a5, a6, a7, a8
        integer :: i , j, n, Imax, Jmax, k, MinIndex
        real(8), dimension(:), allocatable :: temp

        grid = lg
        Imax = grid%Imax
        Jmax = grid%Jmax
        dzeta = grid%d_zeta()
        deta = grid%d_eta()
        allocate(temp(Imax))

        e = 1
        do k = 1, 100
            do n = 1, Imax*Jmax
                j = CEILING(real(n)/real(Imax))
                i = n - Imax * (j -1)

                den = 2 * (alpha(i,j)/dzeta**2 + jamma(i,j)/deta**2)
                a1 = -1 * beta(i,j) / (2 * dzeta * deta * den)
                a2 = jamma(i,j) / (deta**2 * den)
                a3 = -1 * a1
                a4 = alpha(i,j) / (dzeta**2 * den)
                a5 = a4
                a6 = -1 * a1
                a7 = a2
                a8 = a1

                if (j == 1) then ! for lower boundary
                    if ((lg%nodes(i,j)%x < 2) .or. (lg%nodes(i,j)%x > 3)) then
                        ! slope is zero for x and y is constant
                        lg%nodes(i,j)%x = lg%nodes(i,j+2)%x
                    else    ! at bump
                        ! x can still slides on bump surface
                        ! thus, the slop of x with change of eta equals to 
                        ! derivative of bump function 
                        lg%nodes(i,j)%x = lg%nodes(i,j+2)%x     &
                            + 2 * deta * (0.2*pi*cos((lg%nodes(i,j)%x-2)*pi))
                        ! here y coordinate must be updated because it might be changed while 
                        ! fixing the position of LE and TE
                        lg%nodes(i,j)%y = 0.2 * sin((lg%nodes(i,j)%x-2)*pi)
                    end if
                else if (j == Jmax) then    ! for upper boundary
                    lg%nodes(i,j)%x = lg%nodes(i,j-2)%x
                else if (i == 1) then       ! for left boundary
                    lg%nodes(i,j)%y = lg%nodes(i+2,j)%y
                else if (i == Imax) then    ! for right boundary
                    lg%nodes(i,j)%y = lg%nodes(i-2,j)%y
                else if (i > 1 .and. i < Imax .and. j > 1 .and. j < Jmax) then
                    lg%nodes(i,j)%x =   a1 * lg%nodes(i-1,j-1)%x + &
                                        a2 * lg%nodes(i  ,j-1)%x + &
                                        a3 * lg%nodes(i+1,j-1)%x + &
                                        a4 * lg%nodes(i-1,j  )%x + &
                                        a5 * lg%nodes(i+1,j  )%x + &
                                        a6 * lg%nodes(i-1,j+1)%x + &
                                        a7 * lg%nodes(i  ,j+1)%x + &
                                        a8 * lg%nodes(i+1,j+1)%x 

                    lg%nodes(i,j)%y =   a1 * lg%nodes(i-1,j-1)%y + &
                                        a2 * lg%nodes(i  ,j-1)%y + &
                                        a3 * lg%nodes(i+1,j-1)%y + &
                                        a4 * lg%nodes(i-1,j  )%y + &
                                        a5 * lg%nodes(i+1,j  )%y + &
                                        a6 * lg%nodes(i-1,j+1)%y + &
                                        a7 * lg%nodes(i  ,j+1)%y + &
                                        a8 * lg%nodes(i+1,j+1)%y 
                end if
            end do

            ! fix position of LE
            temp = abs(lg%nodes(:,1)%x - 2)
            MinIndex = MINLOC(temp, 1)      ! get index of closest node to the LE
            lg%nodes(MinIndex,1)%x = 2.0d0
            lg%nodes(MinIndex,1)%y = 0.0d0

            ! fix position of TE
            temp = abs(lg%nodes(:,1)%x - 3)
            MinIndex = MINLOC(temp, 1)      ! get index of closest node to the TE
            lg%nodes(MinIndex,1)%x = 3.0d0
            lg%nodes(MinIndex,1)%y = 0.0d0
            
            grid = lg
        
            if (e < 0.00001) exit
        end do

        
    end subroutine genLaplace

end module LaplaceMod