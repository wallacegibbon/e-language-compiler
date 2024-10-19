#! /bin/sh
set -e

echo Building the escript file...
rebar3 escriptize

echo Copying the \"ec\" command to /usr/local/bin...
sudo cp _build/default/bin/ec /usr/local/bin/

echo Done.

