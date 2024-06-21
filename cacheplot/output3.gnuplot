set terminal svg dynamic mouse standalone
load 'parula.pal'
set datafile columnheader separator '\t'
set logscale y 2
set logscale x 2
set ytics 16,2 logscale
set format y "%.0s%cB"

timeslice(t) = t == 6 ? 50 : 2**t
output(t) = t == 0 ? "output0.dat" : sprintf("output1_0.%03d.dat", t)

set xlabel "timeslice(ms)" rotate parallel
set ylabel "stride" rotate parallel
set zlabel "cycles" rotate parallel
set pm3d depthorder base

set style fill transparent solid 0.75 noborder
splot for [t=0:6] for [i=0:12] output(timeslice(t)) using (timeslice(t)):1:4+2*i:5+2*i:4+2*i with lines ls i notitle,\
	for [i=0:12] output(0) using (100):1:5+2*i:4+2*i:5+2*i ls i title columnhead(4+2*i) with zerrorfill

