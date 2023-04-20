program parcial1

	use mod_prec
	use mod_funciones
	
	real (wp)		:: t, a, b!, x1t, x2t, DeltaXt
	!real (wp)		:: x1, x2, DeltaX
	integer (il)	:: i, fu
	character (80)	:: file_datos
	
	file_datos = "datos.dat"
	
	a = 1._wp
	b =	2._wp

	
	open(newunit=fu, file=file_datos)
		
		do i=0, 600, 1
			
			t = a + i*(b-a)/600._wp
			
			write (fu,'(4(X, ES20.13))') t, x1(t), x2(t), DeltaX(t) 
			
		end do	
		
	close (fu)
	
	
end program parcial1
