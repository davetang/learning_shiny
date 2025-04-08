#!/usr/bin/env bash

set -euo pipefail

VER=4.4.3
IMAGE=davetang/rstudio:${VER}

DATADIR=$(realpath $(dirname $0)/../data)

docker run \
   --rm \
   -u $(id -u):$(id -g) \
   -v ${DATADIR}:/data \
   ${IMAGE} Rscript -e "write.csv(mtcars, '/data/mtcars.csv', row.names = FALSE)"

exit 0
