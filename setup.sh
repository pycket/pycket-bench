#! /bin/sh

PROGRAM=`echo $0 | sed 's%.*/%%'`
PROGDIR="$(cd `dirname $0`; echo $PWD)"

if [ ! -z "$ZSH_VERSION" ]; then
  setopt shwordsplit
fi
ECHO="/usr/bin/printf %b\\n"
if type wget >/dev/null 2>/dev/null; then
  FETCH="wget --quiet"
elif type curl >/dev/null 2>/dev/null; then
  FETCH="curl -s -S -O"
else
  FETCH="$ECHO Please download "
fi

set +e
type pushd 2>/dev/null >/dev/null
if [ 0 -ne "$?" ]; then
    _OLD=""
    pushd() {
        _OLD=`echo $PWD`
        cd "$1"
    }
    popd() {
        cd "$_OLD"
    }
fi
set -e

REBENCH="`command -v rebench`"
if [ -z "$REBENCH" ]; then
    $ECHO "installing ReBench"
    pushd $PROGDIR >/dev/null 2>/dev/null
    pip install -r requirements.txt
    popd >/dev/null 2>/dev/null
fi

RACKET="`command -v racket`"
if [ -z "$RACKET" ]; then
    $ECHO "Cannot find racket" 1>&2
    exit 1
fi

$ECHO "ignoring  bigloo"

if [ ! -x "$PROGDIR/gambit/bin/gsc" ]; then
    $ECHO "installing Gambit"
    pushd "$PROGDIR/src" >/dev/null 2>/dev/null
    GAMBIT="gambc-v4_7_2"
    $FETCH "http://www.iro.umontreal.ca/~gambit/download/gambit/v4.7/source/$GAMBIT.tgz"
    tar -xzf "$GAMBIT.tgz"
    cd $GAMBIT
    if uname | grep -qi 'Darwin'; then
        if [ -z "$GCC" ]; then
            $ECHO "Please provide viable GCC, clang won't do here" 1>&2
            exit -1
        fi
        CC=$GCC ./configure --prefix=$PROGDIR/gambit --enable-single-host --disable-debug
    else
        ./configure --prefix=$PROGDIR/gambit --enable-single-host --disable-debug
    fi
    make -j
    make install
    popd >/dev/null 2>/dev/null
fi

if  [ ! -x "$PROGDIR/larceny/larceny" ]; then
    $ECHO "installing Gambit"
    pushd "$PROGDIR/src" >/dev/null 2>/dev/null
    if uname | grep -qi 'Darwin'; then
        LARCENY="larceny-0.97-bin-native-ia32-linux86"
    else
        LARCENY="larceny-0.97-bin-native-ia32-macosx"
    fi
    $FETCH "http://www.larcenists.org/LarcenyReleases/$LARCENY.tar.gz"
    tar -xzf "$LARCENY.tar.gz"
    ln -s $PROGDIR/src/$LARCENY $PROGDIR/larceny
    popd >/dev/null 2>/dev/null
fi

if [ ! -f "$PROGDIR/pycket/targetpycket.py" ]; then
    $ECHO "fetching Pycket"
    pushd $PROGDIR >/dev/null 2>/dev/null
    git submodule init
    git submodule update
    popd >/dev/null 2>/dev/null
fi

if ls -1 "$PROGDIR/CrossBenchmarks/dish" | grep -q rkt; then
    :
else
    $ECHO "Cooking benchmarks"
    pushd $PROGDIR/CrossBenchmarks/src  >/dev/null 2>/dev/null
    sh cook_files.sh
    popd >/dev/null 2>/dev/null
fi
