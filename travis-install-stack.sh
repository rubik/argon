#!/bin/bash

set -e

STACK_HOME=$HOME/stack
echo 'Installing Stack'
if [[ ! -d $STACK_HOME ]]; then
    git clone --depth 300 https://github.com/commercialhaskell/stack $STACK_HOME
    git reset --hard 24a461c950c5c8719ff427ab5a3b1494530b62e5
fi

cd $STACK_HOME
echo 'Compiling Stack'
stack install
