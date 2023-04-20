program main_test
use ISO
use modulo_funciones
use modulo_raices


implicit none

!Declaracion de variables
real(wp) :: a, b, errx, erry, raiz
integer :: mit
character(80) :: archivo1 , archivo2, archivo3, archivo4

a = 1.0_wp
b = 2.0_wp
errx = 1.e-11_wp
erry = 1.e-11_wp
mit = 100000
!------------------------------------------------------------
archivo1 = "datos_bisec.dat"

call r_bisec(f1, a, b, mit, errx, erry,raiz, archivo1)

write(*,*) "caca1"

!------------------------------------------------------------
!Test del metodo de Newton

archivo2 = "datos_newton.dat"
call r_newton(f1, df1, b, mit, errx, erry, archivo2)
write(*,*) "caca newton"

!------------------------------------------------------------
!Test del metodo de la Secante

archivo3 = "datos_secante.dat"
call r_secante(f1, a, b, mit, errx, erry, archivo3)
write(*,*) "caca secante"


!-----------------------------------------------------------
!Test del metod Regula- Falsi

archivo4 = "datos_regula_falsi.dat"
call r_regula_falsi(f1, a, b, mit, errx, erry, archivo4)
write(*,*) "caca rfalsi"
end program main_test
