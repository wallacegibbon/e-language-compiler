## 构建编译器

本编译器是作为[escript](https://www.erlang.org/doc/apps/erts/escript_cmd)发布的，使用[rebar3](http://rebar3.org/docs/getting-started/)构建它：

```sh
rebar3 escriptize
```

运行上面的命令之后，会生成目标程序 `_build/default/bin/ec` 。

如果要在Unix上安装它，直接拷贝到一个在 `PATH` 中的`bin`目录即可。

举例：
```sh
sudo cp _build/default/bin/ec /usr/local/bin/
```
