#! /bin/sh

for spec_file in ../misc/specialize-*.rkt; do
  spec="`echo "$spec_file" | sed -e 's/.*-\(..*\)\.rkt$/\1/'`"
  for bench_file in *.scm; do
    bench="`echo "$bench_file" | sed -e 's/.scm//'`"
    res_file="../dish/$bench-$spec.rkt"
    echo "$res_file"
    printf "`cat ../bench.tmpl`\n" "$spec" "$bench_file" > "$res_file"
  done
done
