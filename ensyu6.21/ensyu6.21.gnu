set term png
set output '/dev/null'
set nokey
set yrange [0.0:0.5]
set xlabel "x"
set ylabel "Temperature"
plot "data/00"
replot "data/01" title 'numerical'
replot "data/02"
replot "data/03"
replot "data/04"
replot "data/05"
replot "data/06"
replot "data/07"
replot "data/08"
replot "data/09"
replot "data/10"
replot "data/theory_00" with lines title 'theoretical'
replot "data/theory_01" with lines
replot "data/theory_02" with lines
replot "data/theory_03" with lines
replot "data/theory_04" with lines
replot "data/theory_05" with lines
replot "data/theory_06" with lines
replot "data/theory_07" with lines
replot "data/theory_08" with lines
replot "data/theory_09" with lines
replot "data/theory_10" with lines
set output 'ensyu6.21.png'
replot