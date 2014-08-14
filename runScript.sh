#!/bin/bash

if [ $# -ne 3 ]; then 
  echo "Usage: $0 <executable> <matrixSize> <blockSize>"
  exit -1
fi

for i in `seq 1 10`; do 
  scala $1 $2 $3 | grep "= [0-9]*" | cut -f2 -d'=' | grep [0-9][0-9]*; 
done
