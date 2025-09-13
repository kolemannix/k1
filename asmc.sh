#!/bin/sh
set -e
set -x
FPATH=$1
outpath=$(dirname $FPATH)/$(basename $FPATH .c).asm
cc -S -g -O0 $FPATH -o $outpath 
cat $outpath | nvim
