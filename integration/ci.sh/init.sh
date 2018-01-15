#!/bin/bash

#
# Copyright (C) 2017-2018 Johann Lee <me@qinka.pro>
#
# This file is part of Yu.
#
# Yu is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Yu is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Yu.  If not, see <http://www.gnu.org/licenses/>.
#

set -e
echo pre-install

########
echo update apt
sudo apt update

######
echo fetch the system\' name
export OS_CORENAME=$(lsb_release -c | awk '{print $2}')
export OS_DISTRIBUTOR=$(lsb_release -i | awk '{print $3}')
echo using $OS_DISTRIBUTOR  $OS_CORENAME

#####
if [ -n "$STACKSOLVER" ]; then
    export STACKFILE=" --stack-yaml $TRAVIS_BUILD_DIR/$STACKSOLVER"
fi
echo STACKFILE $STACKFILE

######
if [ -n "$LLVMx" ]; then
  echo install llvm
  wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
  echo deb http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb-src http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  echo deb-src http://apt.llvm.org/$OS_CORENAME/ llvm-toolchain-$OS_CORENAME-$LLVM main | sudo tee -a /etc/apt/sources.list.d/llvm.list
  sudo apt update
  sudo apt install -y libllvm$LLVM libllvm$LLVM-dbg liblldb-$LLVM lldb-$LLVM llvm-$LLVM llvm-$LLVM-dev llvm-$LLVM-runtime
else
  echo without llvm
fi

######
echo login docker
docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"

######
echo setting up ghc-$GHC_VER
export PATH=/opt/ghc/$GHC_VER/bin:$PATH
ghc -V

######
echo install haskell-stack
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

######
echo config haskell-stack
stack config set system-ghc --global true
stack path --programs $STACKFILE

######
echo setup database
sudo apt install -y mongodb mongodb-clients mongodb-server mongodb-dev
nohup mongod & > /dev/null
