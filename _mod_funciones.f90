module modulo_funciones
use ISO

implicit none
contains

!------------------------------------------
!#########################################
!funcion f1
real(wp) function f1(x)
    real(wp), intent(in) :: x

    f1 = x**3._wp + 4._wp*x**2._wp - 10._wp

end function f1

!-----------------------------------------
!########################################
!Derivada de la funcion f1:  df1

real(wp) function df1(x)
    real(wp), intent(in) :: x
    
    df1 = 3._wp*x**2._wp + 8._wp*x 
end function df1


!----------------------------------------
!########################################
end module modulo_funciones
