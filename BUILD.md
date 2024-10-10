## Build

The program is released as an escript file. We build it with rebar:

```sh
rebar3 escriptize
```

You can find the generated file at `_build/default/bin/ec`.

To install it on Unix, you can simply copy it to some `bin` directory in your PATH.

e.g.
```sh
sudo cp _build/default/bin/ec /usr/local/bin/
```

