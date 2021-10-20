#!/bin/sh
# Originally from:
# https://github.com/blankpage/e5UNIXBuilder/blob/master/build-akili.sh

if [[ "$OSTYPE" =~ "linux-gnu" ]]
then CPUS=$(nproc)
elif [[ "$OSTYPE" =~ "darwin" ]]
then CPUS=2
else CPUS=$(sysctl -n hw.ncpu)
fi

export CPUS
