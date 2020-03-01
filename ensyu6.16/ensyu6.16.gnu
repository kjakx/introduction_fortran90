set term png
set output 'ensyu6.16.png'
set hidden3d
set xlabel 'x1'
set ylabel 'x2'
splot 'output.d' title 'phi' with lines
