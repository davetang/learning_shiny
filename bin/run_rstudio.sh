#!/usr/bin/env bash

set -euo pipefail

VER=4.4.3
IMAGE=davetang/rstudio:${VER}
NAME=rstudio_server_shiny_${VER}
PORT=7779
PACKAGE_DIR=$(pwd)/r_packages_${VER}

if [[ ! -d ${PACKAGE_DIR} ]]; then
   mkdir ${PACKAGE_DIR}
fi

docker run \
   --name ${NAME} \
   -d \
   -p ${PORT}:8787 \
   -v ${PACKAGE_DIR}:/packages \
   -v $(pwd):/home/rstudio/work \
   -e PASSWORD=password \
   -e USERID=$(id -u) \
   -e GROUPID=$(id -g) \
   ${IMAGE}

>&2 echo ${NAME} listening on port ${PORT}

exit 0
