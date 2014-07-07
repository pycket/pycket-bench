pycket-bench: Benchmarking `pycket` against some Schemes
========================================================

Check out via

     git clone --recursive https://github.com/krono/pycket-bench.git

Preparation
-----------

**pycket-bench** depends on

 * [pycket][pycket], obviously, and [Racket][racket]
 * a [PyPy][pypy] checkout for `rpython`
 * [ReBench][rebench] for carrying out benchmarks
 * [Gambit][gambit], [Larceny][larceny], and [Bigloo][bigloo] for comparison
 * Optionally: [R][R] to analyze the results
 
and their transitive dependencies, notably

 * `git`, `hg`, and `pip`
 * PyYAML, SciPy
 * A C compiler environment (`cc`, `make` and friends)

### Installation

You can use the provided `setup.sh` to install ReBench, get Pycket and compile Gambit, Larceny, and Bigloo:

    sh ./setup.sh
    
You have to make sure that `git`, `hg`, and Racket are already installed.

Then you should proceed to translate `pycket`:

    # may take a while
    hg clone https://bitbucket.org/pypy/pypy
    export PYPYDIR=$PWD/pypy

    cd $PYCKET_BENCH_DIR 
    cd pycket
    $PYPYDIR/rpython/bin/rpython -Ojit targetpycket.py

You are now set to go benchmarking.

### ReBench
    
Should the ReBench installation via `pip` fail, try this:

* On Debian / Ubuntu / other Linux
 
  Use `apt` or `aptitude` to install ReBench perquisites (this seems to fail when relying on `pip` alone)

      aptitude install python-pip python-yaml python-scipy

  or your package manager equivalent.

 * On OS X

   Try `pip install ReBench` first or your package manager of choice ([homebrew][homebrew], [MacPorts][macports], [Fink][fink], …)


Running Benchmarks
------------------

Running any of this commands produces a file `output/pycket.data`, which is a [TSV file][tsv] containing the benchmark results.


### Run the benchmarks statistically rigorously

    sudo rebench -d -v rebench.conf

_Note:_ Uses `sudo` because `rebench` without `-N` uses `nice`

### Run the benchmarks normally but not re-niced

Use this when using `sudo` is not an option, but beware that increased context switches may distord the results.

    rebench -N -d -v rebench.conf

### Run the benchmarks quickly

These results should only serve as a rough estimation and are not statistically rigorous.


    rebench -N -q -d -v rebench.conf


### Run the benchmarks very quickly 

This runs only Pycket and Racket and writes to `output/fast.data` (also a TSV file). These results should only serve as a rough estimation and are not statistically rigorous.

    rebench -N -d -v rebench.conf FastBenchmark
   


### Run a single benchmark

 * **Racket**

        cd CrossBenchmarks
        ../bin/run-racket -nothing ctak

 * **Pycket binary**

        cd CrossBenchmarks
        ../bin/run-pycket -nothing ctak

 * **Pycket hosted**

        cd CrossBenchmarks
        PYTHONPATH=$PYPY pypy ../../pycket/targetpycket.py dish/ctak-nothing.rkt

 * **Gambit**

        cd CrossBenchmarks
        ../bin/run-gambit -nothing ctak

 * **Larceny**

        cd CrossBenchmarks
        ../bin/run-larceny -nothing ctak
        
 * **Bigloo**
 
        cd CrossBenchmarks
        ../bin/run-bigloo -nothing ctak

Analyze benchmarks
------------------

    ./analyze_benchmarks.R [output/your.tsv]

or 

    Rscript ./analyze_benchmarks.R [output/your.tsv]

If no argument is given, it defaults to `output/current.tsv`, which could be a convenient symlink.

[pycket]: https://github.com/samth/pycket
[pypy]: https://bitbucket.org/pypy/pypy
[rebench]: https://github.com/smarr/ReBench
[gambit]: http://gambitscheme.org/
[larceny]: https://github.com/larcenists/larceny/
[bigloo]: http://www-sop.inria.fr/indes/fp/Bigloo/
[racket]: http://racket-lang.org/
[R]: http://www.r-project.org/
[homebrew]: http://brew.sh/
[macports]: https://www.macports.org/
[fink]: http://www.finkproject.org/
[tsv]: https://en.wikipedia.org/wiki/Tab-separated_values