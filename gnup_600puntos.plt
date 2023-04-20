set term x11 persist

##################################################
#### 			h o l i s	 ( :			######
##################################################

	set title " T I T U L "
	set xlabel "equis"
	set ylabel "igriega"
    set grid 
    set sample 500	
##################################################
#### D E F I N I C I O N     F U N C I O N	######
##################################################

	#f (x) = x**3. + 4.*x**2. - 10.
	#f (x) = sin (x)
	# esas son las funciones con las que estuve probando jeje
	
		ax = 1
		bx = 2
		#ay =
		#by =

	#set xrange	[ax:bx]
	#set yrange	[ay:by]

##################################################
######		G R A F.    F U N C I O N		######
##################################################
# reemplazar % por tipo y grosor segun corresponda

   	#plot f(x) linewidth 2 



##################################################
######		G R A F.   D A T O S			######
##################################################
# reemplazar % por tipo y grosor segun corresponda

#	replot "datos.dat" with lines linetype % linewidth %
#	replot "datos_bisec.dat" u 2:3 dashtype '.'
	
	plot "datos.dat" u 1:2 with linespoints linetype 1 linewidth 1
	replot "datos.dat" u 1:3 with linespoints linetype 2 linewidth 2
	replot "datos.dat" u 1:4 with linespoints linetype 3 linewidth 3


#using n:m para seleccionar que columnas graficar#




####Próximamente, solo en cines:


##################################################
######			E X P O R T A R				######
##################################################
#############		P 	N 	G		 #############

	
    set terminal png size 1200,900 
    set output 'output.png'
    replot
#############		P 	D 	F		 #############

	#set terminal pdf enhanced font "Helvetica, 11"
	#set output "nombre_grafico.pdf"
exit


