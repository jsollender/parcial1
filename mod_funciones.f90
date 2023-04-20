module mod_funciones

use mod_prec

contains

	function x1(t)
		real (wp), intent (in)	:: t
		real (wp)				:: x1
		
		x1 = t-10._wp	
		
	end function x1
	
	function x2(t)
		real (wp), intent (in)	:: t
		real (wp)				:: x2
		
		x2 = 100._wp*exp(-0.1_wp*t)-2._wp*t-96._wp
		
	end function x2
	
	function DeltaX(t)
		real (wp), intent (in)	:: t
		real (wp)				:: DeltaX
		
		DeltaX = 100._wp*exp(-0.1_wp*t)-3._wp*t-86._wp
	
	end function DeltaX

end module mod_funciones
