set term png
set output "/dev/null"
set xlabel 'x'
set ylabel 'y(x)'
plot 'points.d' title 'f(x)=0.1x^3+0.2x^2+0.5x+1+rnd'
set output 'ensyu6.9.png'
replot 'poly.d' with lines title 'fitting curve'
