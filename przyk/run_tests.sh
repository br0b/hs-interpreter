#!/bin/bash

trap '' INT

d=../examples

cabal build

for i in $d/*.uhs
do
    b=$(basename $i .uhs)
    o=$(echo $d/$b.output)
    printf "%.s=" {1..124}
    echo ""
    cat $i
    echo -e "\n"input: $i           output: $o
    echo "------------"
    cabal exec zadanie3 -- $i | tail -n 100 | diff -N -y -s -Bb - $o
done

trap - INT
