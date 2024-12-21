#! /bin/sh
set -e

UNAME_S=$(uname -s)

case $UNAME_S in
MINGW*|CYGWIN*) OS_TYPE=windows;;
*             ) OS_TYPE=unix;;
esac

if test $OS_TYPE = windows
then INSTALL_PATH=$HOME/AppData/Local/programs/e-language-compiler
else INSTALL_PATH=/usr/local/bin
fi

if test $OS_TYPE = windows
then SUDO=""
else SUDO=sudo
fi

echo Building the escript file...
rebar3 escriptize

mkdir -p $INSTALL_PATH
echo Copying the \"ec\" command to $INSTALL_PATH...
$SUDO cp _build/default/bin/ec $INSTALL_PATH

echo Done.
