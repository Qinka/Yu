#!/bin/bash

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

if [ -n "$RUN_BUILD" ]; then
    ghc -V	
    O2FLAG=" --ghc-options -O3 "
    if [ -n "$LLVM" ]; then
	    export LLVMFLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
    fi
    if [ -n "$THREADED" ]; then
	    export THREADFLAG=" --ghc-options -threaded "
    fi
    if [ -n "$DEBUG" ]; then
	    export XDEBUGFLAG=" --flag glob-core:debug-info "
	    export XDEBUGFLAG=$DEBUGFLAG" --ghc-options -rtsopts "
    fi
    stack build $STACKFILE $O2FLAG $THREADFLAG $LLVMFLAG $DEBUGFLAG
fi
