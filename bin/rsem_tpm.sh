#!/usr/bin/env bash

set -euo pipefail

DATADIR=$(realpath $(dirname $0)/../data)
URL=https://zenodo.org/records/13970886/files/rsem.merged.gene_tpm.tsv
OUTFILE=$(basename ${URL})

wget -O - https://zenodo.org/records/13970886/files/rsem.merged.gene_tpm.tsv \
   | gzip > ${DATADIR}/${OUTFILE}.gz

exit 0
