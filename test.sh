#!/bin/sh

./build.sh || exit 1

falderal tests/Emmental.markdown
