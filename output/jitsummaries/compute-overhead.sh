#!/bin/zsh -f
DIR=${DIR:-.}

# OVERALL:
# benchid, jit, total, ratio
(
  find "${DIR}" -name \*-out.txt | \
      while read FILE;
      do
        name=$(echo $FILE | sed -E 's/.*pycket-c-(.+)-out.txt/\1/');
        printf "%s," "$name"
        awk '
/^Tracing/ { tracing=$3; }
/^Backend/ { backend=$3; }
/^TOTAL/ { total=$2;}
END {
  print tracing + backend, ",", total, ",", (tracing + backend) / total
}' $FILE
      done
) > "${DIR}/jit-overhead.csv"



# Crossbenches
# specialize,benchid, jit, total, ratio
egrep '(fixflo|nothing)' "${DIR}/jit-overhead.csv" | sed -e 's/-/,/' > "${DIR}/jit-overhead-cross.csv"


# Shootout
# benchid, jit, total, ratio
egrep '(^[^-.,]+,|-generic|nbody-vec|fannkuch-redux)' "${DIR}/jit-overhead.csv" > "${DIR}/jit-overhead-shootout.csv"


# Chaperones
# variant, benchid, jit, total, ratio
egrep -v '(^[^-.,]+,|-generic|nbody-vec|fannkuch-redux|fixflo|nothing)' "${DIR}/jit-overhead.csv" | \
    sed -e 's/^no-callgraph/no-callgraph,/' -e 's/^\([^n][^o][^-]\)/callgraph,\1/' > "${DIR}/jit-overhead-chap.csv"
