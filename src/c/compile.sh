#!/bin/sh
#gcc -I ../../include/ -l js  -L ../../lib/ example.c -o example
gcc -I ../../include/ -l js  -L ../../lib/ grovel.c -o cl-spidermonkey-grovel
