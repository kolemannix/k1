#!/bin/sh
set -e
set -x
FPATH=$1
outpath=$(dirname $FPATH)/$(basename $FPATH .c).ll
# cc -S -emit-llvm -O0 $FPATH -o $outpath 
cc -target x86_64-unknown-linux -S -emit-llvm -O0 $FPATH -o $outpath 
cat $outpath | less
