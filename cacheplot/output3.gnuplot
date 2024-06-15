set datafile columnheader separator '\t'
set logscale y 2
set logscale x 2
set ytics 16,2 logscale
set format y "%.0s%cB"
output(t) = t == 0 ? "output0.dat" : sprintf("output1_0.%03d.dat", 2**t)

set xlabel "timeslice(ms)" rotate parallel
set ylabel "stride" rotate parallel
set zlabel "cycles" rotate parallel
set pm3d depthorder base

splot for [t=1:5] for [i=1:14] output(t) using (2**t):1:1+3*i:2+3*i:3+3*i notitle with zerrorfill,\
	for [i=1:14] output(0) using (50):1:1+3*i:2+3*i:3+3*i title columnhead(i) with zerrorfill
show view
pause -1
