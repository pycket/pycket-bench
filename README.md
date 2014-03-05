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
