set term png
set output 'list6.6.png'
set pm3d
set palette rgbformulae 33, 13, 10
set nokey
set xlabel 'x1'
set ylabel 'x2'
set zlabel 'phi'
splot 'output.d' with pm3d
