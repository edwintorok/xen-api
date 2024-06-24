set terminal svg dynamic mouse standalone
load 'viridis.pal'
set datafile columnheader separator '\t'
set logscale x 2
set logscale y 2
set mxtics 4
set mytics 4
set key outside
set grid xtics mxtics ytics mytics

set xlabel "timeslice(ms)" rotate parallel
set ylabel "cycles" rotate parallel
set pointsize 0.5

#plot for [i=0:*] 'auto.dat' using ($2*1000):4+3*i:5+3*i:6+3*i with errorlines title columnhead(4+3*i) ls i
# some errobars are so thin, that they're invisible, so always add a line too
plot for [i=0:*] 'auto.dat' using ($2*1000):5+3*i:6+3*i with filledcurves title columnhead(4+3*i) ls (2*i+1),\
	for [i=0:*] 'auto.dat' using ($2*1000):5+3*i:6+3*i with lines title columnhead(4+3*i) ls (2*i+1),\
	for [i=0:*] '' using ($2*1000):4+3*i:5+3*i:6+3*i with points title columnhead(4+3*i)
	


