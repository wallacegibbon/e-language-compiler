#! /bin/sh
set -e

case $(uname -o) in
GNU/Linux|FreeBSD|Darwin) INSTALL_PATH=/usr/local/bin;;
Msys                    ) INSTALL_PATH=$HOME/AppData/Local/programs/e-language-compiler;;
*                       ) echo Unsupported platform >&2; exit 1;;
esac

case $(uname -o) in
GNU/Linux|FreeBSD|Darwin) SUDO=sudo;;
*                       ) SUDO="";;
esac

echo Building the escript file...
rebar3 escriptize

mkdir -p $INSTALL_PATH
echo Copying the \"ec\" command to $INSTALL_PATH...
$SUDO cp _build/default/bin/ec $INSTALL_PATH

echo Done.

