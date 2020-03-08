set terminal gif animate delay 10 optimize size 640,480
set output 'ensyu6.19.gif'
set xlabel 'x1'
set ylabel 'x2'
set zlabel 'phi'
set zrange [0:1]
set pm3d
set palette rgbformulae 33, 13, 10
do for [i = 1 : 100] {
  splot sprintf("phi_each_step/%03d", i) with pm3d title 'Δt=5.0x10^{-4}, α=0.5
}

unset output