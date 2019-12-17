set terminal png
set output 'ensyu1.21.png'
set size square
set xlabel 'n'
set ylabel '|e|'
plot 'output.d' with lines
