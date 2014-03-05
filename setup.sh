#!/bin/sh

PROGRAM=`echo $0 | sed 's%.*/%%'`
PROGDIR="$(cd `dirname $0`; echo $PWD)"

REBENCH="`command -v rebench`"
if [ -z "$REBENCH" ]; then
    pushd $PROGDIR >/dev/null 2>/dev/null
    pip install -r requirements.txt
    popd >/dev/null 2>/dev/null
fi

RACKET="`command -v racket`"
if [ -z "$RACKET" ]; then
    echo "Cannot find racket" 1>&2
    exit 1
fi

if [ ! -f "$PROGDIR/pycket/targetpycket.py" ]; then
    pushd $PROGDIR >/dev/null 2>/dev/null
    git submodule init
    git submodule update
    popd >/dev/null 2>/dev/null
fi

if ls -1 "$PROGDIR/CrossBenchmarks/dish" | grep -q rkt; then
    :
else
   pushd $PROGDIR/CrossBenchmarks/src  >/dev/null 2>/dev/null
   sh cook_files.sh 
   popd >/dev/null 2>/dev/null
fi
