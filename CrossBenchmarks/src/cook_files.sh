#! /bin/sh

for spec_file in ../misc/specialize-*.rkt; do
  spec="`echo "$spec_file" | sed -e 's/.*-\(..*\)\.rkt$/\1/'`"
  for bench_file in *.scm; do
    bench="`echo "$bench_file" | sed -e 's/.scm//'`"
    res_file="../dish/$bench-$spec.rkt"
    echo "\tCooking $res_file"
    if egrep -q 'set-c[ad]r!' "$bench_file" ; then
        printf "`cat ../misc/pycket-mcons.tmpl`\n" "$spec" "$bench_file" \
             > "$res_file"
    else
      printf "`cat ../misc/pycket.tmpl`\n" "$spec" "$bench_file" \
             > "$res_file"
    fi
  done
done
