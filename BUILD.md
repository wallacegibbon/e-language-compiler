## Build

The program is released as an [escript](https://www.erlang.org/doc/apps/erts/escript_cmd) file.
We build it with [rebar3](http://rebar3.org/docs/getting-started/).

```sh
rebar3 escriptize
```

You can find the generated file at `_build/default/bin/ec`.

To install it on Unix, you can simply copy it to some `bin` directory in your `PATH`.

e.g.
```sh
sudo cp _build/default/bin/ec /usr/local/bin/
```
