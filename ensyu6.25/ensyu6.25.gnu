set terminal gif animate delay 1 optimize size 640,480
set output 'ensyu6.25.gif'
set xlabel 'x1'
set ylabel 'x2'
set zlabel 'eta'
set zrange [0.9:1.1]
set cbrange [0.97:1.1]
set pm3d
set palette rgbformulae 33, 13, 10
do for [i = 0 : 100] {
  splot sprintf("data/%03d", i) with pm3d title sprintf("Δt=0.1, t=%2.1f", (i / 10.0))
}

unset output