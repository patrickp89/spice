#!/bin/sh

dune build bin/spice.exe && \
  dune runtest
