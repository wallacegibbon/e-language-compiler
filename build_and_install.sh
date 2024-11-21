#! /bin/sh
set -e

case $(uname -s) in
MINGW*|CYGWIN*) INSTALL_PATH=$HOME/AppData/Local/programs/e-language-compiler;;
*             ) INSTALL_PATH=/usr/local/bin;;
esac

case $(uname -s) in
MINGW*|CYGWIN*) SUDO="";;
*             ) SUDO=sudo;;
esac

echo Building the escript file...
rebar3 escriptize

mkdir -p $INSTALL_PATH
echo Copying the \"ec\" command to $INSTALL_PATH...
$SUDO cp _build/default/bin/ec $INSTALL_PATH

echo Done.
