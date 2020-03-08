set term png
set xlabel "x1, x2"
set ylabel "phi"
set output "/dev/null"
plot "sor.d" title "SOR"
set output "ensyu6.18.png"
replot "theory.d" title "theory" with lines