#!/usr/bin/env bash

set -euo pipefail

CI_OS=$(uname -s)

install_deps_linux() {
  echo "Setting up the environment for linux"
  sudo apt-get update
}

install_deps_darwin() {
  echo "Setting up the environment for macOS"
  brew update
}

case $CI_OS in
  Linux) install_deps_linux;;
  Darwin) install_deps_darwin;;
esac

