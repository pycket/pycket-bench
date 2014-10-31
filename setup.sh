#! /bin/sh

PROGRAM=`echo $0 | sed 's%.*/%%'`
PROGDIR="$(cd `dirname $0`; echo $PWD)"

if [ ! -z "$ZSH_VERSION" ]; then
  setopt shwordsplit
fi
ECHO="/usr/bin/printf %b\\n"
if command -v wget >/dev/null 2>/dev/null; then
  FETCH="wget --quiet"
elif command -v curl >/dev/null 2>/dev/null; then
  FETCH="curl -L -s -S -O"
else
  FETCH="$ECHO Please download "
fi

_go() {
    _OLD=`echo $PWD`
    cd "$1"
}
_gone() {
    cd "$_OLD"
}

REBENCH="`command -v rebench`"
if [ -z "$REBENCH" ]; then
    $ECHO "installing ReBench"
    _go $PROGDIR
    pip install --user -r requirements.txt
    _gone
fi

RACKET="`command -v racket`"
if [ -z "$RACKET" ]; then
    $ECHO "Cannot find racket" 1>&2
    exit 1
fi

if [ ! -x "$PROGDIR/bigloo/bin/bigloo" ]; then
    $ECHO "installing Bigloo"
    # ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo4.2a-alpha13Oct14.tar.gz
    _go "$PROGDIR/src"
    BIGLOO="bigloo4.2a"
    BIGLOO_GET="$BIGLOO-alpha13Oct14"
    $FETCH "ftp://ftp-sop.inria.fr/indes/fp/Bigloo/$BIGLOO_GET.tar.gz"
    tar -xzf "$BIGLOO_GET.tar.gz"
    cd $BIGLOO
    mkdir -p $PROGDIR/bigloo
    if uname | grep -qi 'Darwin'; then
        mkdir -p $PROGDIR/bigloo/lib
        ln -s $PROGDIR/bigloo/lib $PROGDIR/bigloo/Frameworks
        ./configure --prefix=$PROGDIR/bigloo --clang --disable-flac
        make
    else
      ./configure --prefix=$PROGDIR/bigloo --benchmark=yes
      make
    fi
    make install
    _gone
fi

if [ ! -x "$PROGDIR/gambit/bin/gsc" ]; then
    $ECHO "installing Gambit"
    _go "$PROGDIR/src"
    GAMBIT="gambc-v4_7_2"
    $FETCH "http://www.iro.umontreal.ca/~gambit/download/gambit/v4.7/source/$GAMBIT.tgz"
    tar -xzf "$GAMBIT.tgz"
    cd $GAMBIT
    if uname | grep -qi 'Darwin'; then
        if [ -z "$GCC" ]; then
            $ECHO "Please provide viable GCC, clang won't do here" 1>&2
            exit 1
        fi
        CC=$GCC ./configure --prefix=$PROGDIR/gambit --enable-single-host --disable-debug
    else
        ./configure --prefix=$PROGDIR/gambit --enable-single-host --disable-debug
    fi
    make -j
    make install
    _gone
fi

if  [ ! -x "$PROGDIR/larceny/larceny" ]; then
    $ECHO "installing Larceny"
    _go "$PROGDIR/src"
    if uname | grep -qi 'Darwin'; then
        LARCENY="larceny-0.97-bin-native-ia32-macosx"
    else
        LARCENY="larceny-0.97-bin-native-ia32-linux86"
    fi
    $FETCH "http://www.larcenists.org/LarcenyReleases/$LARCENY.tar.gz"
    tar -xzf "$LARCENY.tar.gz"
    ln -fs $PROGDIR/src/$LARCENY $PROGDIR/larceny
    _gone
fi

if [ ! -f "$PROGDIR/pycket/targetpycket.py" ]; then
    $ECHO "fetching Pycket"
    _go $PROGDIR
    git submodule init
    git submodule update
    _gone
fi

if [ ! -f "$PROGDIR/ShootoutBenchmarks/fasta-1000000" ]; then
    $ECHO "generating input files for some Racket benchmarks"
    _go "$PROGDIR/ShootoutBenchmarks"
    racket gen-inputs.rkt
    _gone
fi
