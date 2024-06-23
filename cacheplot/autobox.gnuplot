
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
set boxwidth 0.5


#plot for [i=2:*] 'auto.dat' using ($1*1000):i with boxplot title columnhead(i) ls i,\
#	for [i=2:*] '' using ($1*1000):i with lines smooth mcs title columnhead(i) ls i
# TODO: not done
plot for [i=2:*] 'auto.dat' using ($1*1000):i with lines smooth mcs title columnhead(i) ls i,\
	for [i=2:*] 'auto.dat' using ($1*1000):i with boxplot title columnhead(i) ls i
	


