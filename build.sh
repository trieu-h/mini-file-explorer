#!/bin/bash

set -xe

ocamlc unix.cma main.ml -o main -I +unix && ./main
