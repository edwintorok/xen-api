set datafile columnheader separator '\t'
set logscale x 2
set xtics 16,2
set format x "%.0s%cB"
set samples 1000
plot for [i=2:14] 'output.dat' using 1:i title columnhead(i) with linespoints
pause -1
