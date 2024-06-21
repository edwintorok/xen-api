set terminal svg dynamic mouse standalone
load 'parula.pal'
set datafile columnheader separator '\t'
set logscale x 2
set xtics 16,4 logscale
set format x "%.0s%cB"
output(t) = t == 0 ? "output0.dat" : sprintf("output1_0.%03d.dat", 2**t)

set xlabel "stride" rotate parallel
set ylabel "cycles" rotate parallel

set style fill transparent solid 0.5 noborder

plot for [i=0:*] output(0) using 3:4+2*i:5+2*i:4+2*i with errorlines title columnhead(4+2*i) ls i
	


