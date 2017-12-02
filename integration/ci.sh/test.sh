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

if [ -n "$RUN_TEST" ]; then
    # for testing
    echo testing
    # display the version of ghc
    ghc -V
    stack test $STACKFILE
fi
