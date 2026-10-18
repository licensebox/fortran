#!/usr/bin/env bash
set -euo pipefail
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j2
ctest --test-dir build --output-on-failure
cmake --install build --prefix "$PWD/build/install"
gfortran -Ibuild/install/include examples/status.f90 -Lbuild/install/lib -llicensebox_fortran -L"$LICENSEBOX_LIB_DIR" -llicensebox -lpthread -o build/consumer
