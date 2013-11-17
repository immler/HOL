#!/bin/bash

set -e

cd

wget -O polyml5.5.1.tar.gz "http://downloads.sourceforge.net/project/polyml/polyml/5.5.1/polyml.5.5.1.tar.gz?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fpolyml%2Ffiles%2Fpolyml%2F5.5.1%2Fpolyml.5.5.1.tar.gz%2Fdownload&ts=1384728510&use_mirror=softlayer-dal"

tar xvzf polyml.5.5.1.tar.gz
cd polyml.5.5.1
./configure --prefix=$HOME
make
make install