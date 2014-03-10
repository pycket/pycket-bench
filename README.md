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

        cd CrossBenchmarks/dish
        racket -e '(require racket/fixnum racket/flonum racket/unsafe/ops)' -r ctak-nothing.rkt

 * **Pycket binary**

        cd CrossBenchmarks/dish
        ../../pycket/pycket-c ctak-nothing.rkt

 * **Pycket hosted**

        cd CrossBenchmarks/dish
        PYTHONPATH=$PYPY pypy ../../pycket/targetpycket.py ctak-nothing.rkt


