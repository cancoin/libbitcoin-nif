#!/bin/sh

set -e

test `basename $PWD` != "c_src" && cd c_src

dir=`pwd`

case "$1" in
  clean)
    rm -rf install.sh \
           $dir/.libs \
           $dir/.deps \
           $dir/../priv/*
    ;;

  *)
    test -f ../priv/libbitcoin.la && exit 0
    (test -f  install.sh || wget https://raw.githubusercontent.com/libbitcoin/libbitcoin/master/install.sh)
    bash install.sh --disable-static --with-tests=no --with-examples=no \
                    --prefix=$dir/.libs --build-dir=$dir/.deps

    cp -r $dir/.libs/lib/* $dir/../priv/
    ;;
esac
