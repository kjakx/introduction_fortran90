set term png
set output '/dev/null'
set logscale x
set logscale y
plot 'list2.14.out' with linespoints title 'matrix*vector'
set output 'ensyu2.18.png'
replot 'list2.15.out' with linespoints title 'matrix*matrix'
