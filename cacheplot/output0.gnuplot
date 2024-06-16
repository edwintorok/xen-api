set datafile columnheader separator '\t'
set logscale x 2
set xtics 16,2 logscale
set format x "%.0s%cB"
output(t) = t == 0 ? "output0.dat" : sprintf("output1_0.%03d.dat", 2**t)

set xlabel "stride" rotate parallel
set ylabel "cycles" rotate parallel

set style fill transparent solid 0.5 noborder

plot for [i=0:12] output(0) using 1:3+2*i:2+2*i with filledcurves title columnhead(2+2*i),\
	for [i=0:12] output(0) using 1:2+2*i with lines title columnhead(2+2*i) lc black
	


pause -1
