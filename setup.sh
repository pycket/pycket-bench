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

RACKET="`command -v python`"
if [ -z "$RACKET" ]; then
    $ECHO "Cannot find pyhton. Really?" 1>&2
    exit 1
fi

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

RACKET="`command -v pypy`"
if [ -z "$RACKET" ]; then
    $ECHO "Cannot find pypy" 1>&2
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

if [ ! -x "$PROGDIR/larceny/larceny" ]; then
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


if [ ! -x "$PROGDIR/v8/v8" ]; then
    $ECHO "installing v8"
    _go "$PROGDIR/src"
    V8V=3.25.30
    $FETCH https://github.com/v8/v8-git-mirror/archive/$V8V.tar.gz
    tar -xzf "$V8V.tar.gz"
    cd v8-git-mirror-$V8V
    svn checkout \
        http://gyp.googlecode.com/svn/trunk \
        build/gyp --revision 1831
    svn checkout --force --non-interactive --trust-server-cert \
        https://src.chromium.org/chrome/trunk/deps/third_party/icu52 \
        third_party/icu --revision 277999
    if uname | grep -qi 'Darwin'; then
        export CXX=clang++
        export CC=clang
        export CPP="clang -E"
        export LINK=clang++
        export CXX_host=clang++
        export CC_host=clang
        export CPP_host="clang -E"
        export LINK_host=clang++
        export GYP_DEFINES="clang=1"
    fi
    make x64.release -j4
    ln -fs shell out/x64.release/v8
    ln -fs $PROGDIR/src/v8-git-mirror-$V8V/out/x64.release $PROGDIR/v8
    _gone
fi

if [ ! -x "$PROGDIR/spidermonkery/bin/js24" ]; then
    if uname | grep -qi 'Darwin' && type -fp brew >/dev/null; then
      AUTOCONF=autoconf213
      if [ ! \( -f /usr/local/bin/autoconf213 -a \
                -f /usr/local/include/nspr/nspr.h \) ]; then
          $ECHO "Please brew autoconf213 and nspr"
          exit 1
      fi
    else
      AUTOCONF=autoconf2.13
      if [ ! \( -f /usr/bin/autoconf2.13 -a \
                -f /usr/include/nspr/nspr.h \) ]; then
          $ECHO "Please install nspr/libsnpr4-dev and autoconf2.13"
          exit 1
      fi
    fi
    _go "$PROGDIR/src"
    MJS=mozjs-24.2.0
    $FETCH https://ftp.mozilla.org/pub/mozilla.org/js/$MJS.tar.bz2
    tar -xjf $MJS.tar.bz2
    cd $MJS/js/src
    $AUTOCONF
    mkdir build-release
    cd build-release
    mkdir -p $PROGDIR/spidermonkey
    ../configure --prefix=$PROGDIR/spidermonkey
    make
    make install
    _gone
fi

if [ ! \( -f "pycket/targetpycket.py" -a -f "ChaperoneBenchmarks/README" \) ]; then
    $ECHO "fetching deps (Pycket, Chaperone-benches)"
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
