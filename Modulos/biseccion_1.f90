module biseccion

use iso_fortran_env
use iso
use funcion

! MODIFICACIONES_1:
! en las variables de la subrutina reemplacé a y b por a0 y b0
! para no redefinir todos los otros.
! a esos a0, b0 los defini como intent in.

! agregué errorx !!!!!!!! real(pr)

! agregue character datos_biseccion y abri el archivo para escribirle cosas.


contains
subroutine bisec (func, p, a0, b0, tolx, tolf, nit, nmax)
	implicit none

!	real(pr), intent(in)			::		func, a, b, tolx, tolf
	real(pr), intent(in)			::		tolx, tolf, a0, b0
	real(pr)						::		func, a, b !!PREGUNTAR :(
	real(pr), intent(out)			::		p !raiz
	integer(pi), intent(in)			::		nmax
	integer(pi)						::		i, nit, fu
	real(pr)						::		fa, fb, fp, aux, errorx, errorf, errorrel
	character(50)					::		file_datos_biseccion

	file_datos_biseccion = "datos_biseccion.txt"
	fa = func(a0)
	fb = func(b0)
	a = a0
	b = b0

!--- me fijo que ninguno sea raiz y que el intervalo sirva --------

	if (fa*fb==0) then !alguno de los dos es raiz. hay q ver cual
		if((fa==0).and.(fb/=0)) then
			print *, a, "es raiz"
		else if ((fb==0).and.(fa/=0)) then
			print *, b, "es raiz"
		else
			print *, a, "y", b, "son ambos raices"
		end if
			
	else if (fa*fb>0) then 
		print *, "Este intervalo no sirve. Elija otro, por favor."
		!stop
			
	else !fa*fb<0
	
!--- acá chequeo que a<b. si no, reasigno -----------------

		if (b<a) then
			aux = a
			a = b
			b = aux
		end if
		
!--- listos a < b. Abro archivo-------------------------------
		
		open(newunit=fu, file=file_datos_biseccion)
			write (fu,*) "	#it		raiz						f(raiz)						errorx						errorf					error relativo"
						
			! adentro del do write (fu,*) nit, p, fp, errorx, errorf, errorrel 
			
		do nit = 1, nmax, 1
			
			errorx = (b-a)*0.5_pr 
			p = a + errorx
			fp = func(p)
			errorf = abs(fp)
			errorrel = errorx/p
			
			if (fa*fp<0) then
				!a = a
				b = p
			else if (fb*fp<0) then		
				a = p
				!b = b		
			end if
			
			write (fu,*) nit, p, fp, errorx, errorf, errorrel 
			
			if ((abs(fp)<tolf).and.(errorx<tolx)) then
				write (fu,*) ":)"
				exit
				
			end if
			
		end do
		
		close(fu)
	
	end if	
	
end subroutine bisec
end module biseccion

