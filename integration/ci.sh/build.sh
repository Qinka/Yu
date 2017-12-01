#!/bin/bash

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
