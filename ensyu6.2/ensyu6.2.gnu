set term png
set output '/dev/null'
plot 'ensyu6.2.dat' using 1:2 with linespoints title 'gauss-jordan'
set output 'ensyu6.2.png'
replot 'ensyu6.2.dat' using 1:3 with linespoints title 'gaussian elimination'
