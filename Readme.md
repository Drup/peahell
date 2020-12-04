# Peahell

Bunch of utilities for PL experiments. Originally based on boilerplates contained in the [plzoo](http://andrej.com/plzoo/), expanded to satisfy my use-cases. Examples will come eventually.

The package `peahell` contains the following libraries
- `peahell` a backend-agnostic API to create toy language implementations. You should create your basic library using this.
- `peahell.native` an implementation of this API on native. You should link your native binary with this.

The package `peahell-jsoo` package also contains a web implementation. You should link your web implementation with this. You can see an example in action here: [Affe](https://drup.github.io/pl-experiments/affe/).

To install :

```
opam install https://github.com/Drup/peahell
```
