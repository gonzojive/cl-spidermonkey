#!/bin/sh

# ensure no errors
set -e

# download and untar
wget http://ftp.mozilla.org/pub/mozilla.org/js/js-1.8.0-rc1.tar.gz
tar -xzf js-1.8.0-rc1.tar.gz 
rm js-1.8.0-rc1.tar.gz 

# make
cd js
cd src/
make -f Makefile.ref 

# install
cp *.{h,tbl} ../../../include/
cd Linux_All_DBG.OBJ/
cp *.h ../../../../include/
cp js ../../../../bin/
