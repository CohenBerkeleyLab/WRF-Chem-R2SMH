#!/bin/bash

for files in wrfinput_d01_0000 
do
fbase=`echo ${files} | cut -d'_' -f 1-2`
for base in `seq 0 8 120 ` 
do
for si in `seq 0 1 7`
do
let filenumber=$si+$base;
fextension=`printf %04d $filenumber`
ncks  -h -v XLAT,XLONG,HGT,Times   -O       ${fbase}_${fextension}            crazy.tmp.`printf %04d $si`.nc
ncks  -h -O                                 crazy.tmp.`printf %04d $si`.nc    crazy.tmp.`printf %04d $si`.nc
ncpdq -h -O -a west_east,Time,south_north   crazy.tmp.`printf %04d $si`.nc    crazy.tmp.`printf %04d $si`.nc &> /dev/null
echo $fextension
done
echo "PAUSE"
ncrcat -h -O            crazy.tmp.*.nc     tmp_`printf %04d ${base}`
ncpdq  -h -a south_north,west_east,Time -O tmp_`printf %04d ${base}`    tmp_`printf %04d ${base}` &> /dev/null
done
ncrcat -h -O tmp_[0-9][0-9][0-9][0-9] ${fbase}
ncpdq  -h -O -a Time,south_north,west_east ${fbase} ${fbase} &> /dev/null
done

rm  crazy.tmp.????.nc 
