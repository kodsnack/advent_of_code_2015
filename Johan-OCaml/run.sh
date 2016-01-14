#!/bin/sh
for d in day*.ml; do
	echo -e "\n----( ${d%%.ml} )--------"
	ocaml $d
done
