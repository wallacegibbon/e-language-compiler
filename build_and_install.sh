#! /bin/sh
set -e

OS_TYPE=$(uname -s)

case $OS_TYPE in
MINGW*|CYGWIN*) INSTALL_PATH=$HOME/AppData/Local/programs/e-language-compiler;;
*             ) INSTALL_PATH=/usr/local/bin;;
esac

case $OS_TYPE in
MINGW*|CYGWIN*) SUDO="";;
*             ) SUDO=sudo;;
esac

echo Building the escript file...
rebar3 escriptize

mkdir -p $INSTALL_PATH
echo Copying the \"ec\" command to $INSTALL_PATH...
$SUDO cp _build/default/bin/ec $INSTALL_PATH

echo Done.
