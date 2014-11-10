#!/bin/bash

which=$1
which="${which%.tsv}"
pdflatex -jobname="${which}-table" "\def\FILE{${which}}\input{output/Table}"
