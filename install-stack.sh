#!/usr/bin/env bash

_stack_version=$1

if [ "$_stack_version" = "" ]; then
  echo "Must provide stack version"
  exit 1
fi

if [ ! -e ~/stack-${_stack_version}-linux-x86_64/stack ]; then
  wget https://github.com/commercialhaskell/stack/releases/download/v${_stack_version}/stack-${_stack_version}-linux-x86_64.tar.gz -O ~/stack-${_stack_version}-linux-x86_64.tar.gz
  tar -C ~/ -xf ~/stack-${_stack_version}-linux-x86_64.tar.gz && chmod +x ~/stack-${_stack_version}-linux-x86_64/stack
fi

mkdir -p ~/.local/bin
ln -sf ~/stack-${_stack_version}-linux-x86_64/stack ~/.local/bin/stack
