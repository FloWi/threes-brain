set datafile separator ","
#set terminal png
#set output 'plot.png'
set key right bottom
#plot for [filename in filenames] filename every ::1 using 1:4 with line title 'Average', '' using 1:2 with line title 'Best','' using 1:3 with line title 'Worst'
plot for [filename in filenames] filename every ::1 using 1:4 with line title filename
pause -1
