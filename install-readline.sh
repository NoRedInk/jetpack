#!/bin/bash

LDFLAGS=-L/usr/local/opt/readline/lib \
CFLAGS=-I/usr/local/opt/readline/include \
stack install readline --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib

