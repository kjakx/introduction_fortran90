touch ./ensyu6.2.dat
for i in 100 200 300 400 500 600 700 800 900 1000 ; do
	echo $i > input
	./ensyu6.2 < input >> ./ensyu6.2.dat
done
gnuplot ./ensyu6.2.gnu
