pycket-bench
============

Run the benchmarks quickly
--------------------------

    rebench -N -q -d -v rebench.conf

Run the benchmarks normally but not re-niced
--------------------------------------------

    rebench -N -d -v rebench.conf

Run the benchmarks statistically rigorously
-------------------------------------------

    sudo rebench -d -v rebench.conf

_Note:_ Uses `sudo` because `rebench` without `-N` uses `nice`

Run a single benchmark
----------------------

 * **Racket**

        cd CrossBenchmarks
        ../bin/run-racket -nothing ctak

 * **Pycket binary**

        cd CrossBenchmarks
        ../bin/run-pycket -nothin ctak

 * **Pycket hosted**

        cd CrossBenchmarks
        PYTHONPATH=$PYPY pypy ../../pycket/targetpycket.py dish/ctak-nothing.rkt

 * **Gambit**

        cd CrossBenchmarks
        ../bin/run-gambit -nothin ctak


