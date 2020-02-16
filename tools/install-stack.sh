#!/bin/bash

set -Eeuxo pipefail

if test -f "$HOME/.local/bin/stack"
then
  echo 'Stack is already installed.'
else
  echo "Installing Stack for $TRAVIS_OS_NAME..."
  curl -sSL https://get.haskellstack.org/ | sh
fi

stack --version