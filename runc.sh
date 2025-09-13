#!/bin/sh
set -e
set -x
FPATH=$1
outpath=$(dirname $FPATH)/$(basename $FPATH .c).out
cc -g -O0 $FPATH -o $outpath 
$outpath
