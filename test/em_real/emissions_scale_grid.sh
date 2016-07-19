#!/bin/bash
 
for files in `ls wrfchemi_*z*1`; do for i in `seq 0 7`; do for j in `seq 0 7`; do ncks -a -h -d west_east,${i},,8 -d south_north,${j},,8  ${files} ${files}.i${i}.j${j}.nc ; done ; done ; done
