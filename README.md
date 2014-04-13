# Extensible Clojure

An extensible, symbolic, functionally-pure Clojure interpreter.

EClojure aims to completely virtualize the Clojure language, such that
portable Clojure code can be run within a precisely controlled environment.
Proper tail calls are supported via delimited continuations, which enables all
effects to be modeled as pure data. Effects, including bidirectional interop,
are processed at the top-level of the interpreter loop, so they too are fully
programmable.

This project's ambitious goals are based on a vast academic foundation.  Oleg
Kiselyov's [Extensible Effects & Interpreters][1] page provides an excellent
entry point.

I'm also interested in building an evaluator that includes an extensible
abstract interpreter and extensible JIT compiler. See [Lancet][2].


## Status

The interpreter is self-applicable, but still highly experimental!

Most of the special forms are implemented, but forms related to types cheat a
bit. Specifically, `deftype`, `defprotocol`, and `defrecord` delegate to the
Clojure compiler. This enables interop, but prevents extended functionality in
method bodies; that restriction will be lifted eventually. The only planned
special form not yet functioning is `reify`. That leaves just `monitor-enter`
and `monitor-exit`, which are unlikely to be implemented soon.


## Usage

Currently, the public API is only `eclj.core` which contains the same publics
as `clojure.core` with essentially identical functionality.

You can evaluate individual forms with `eclj.core/eval` and load `.eclj` files
with `eclj.core/require` or related namespace & code loading functions. EClj
can interop with normal Clojure code seamlessly, but `clojure.core/ns` can't
load EClj. Of course, `eclj.core/ns` can require either file type.

Various caveats apply, but the list is too volatile to justify enumerating now.



## License

Portions of this project derived from Clojure:  
Copyright © 2006-2014 Rich Hickey

Original code and Clojure modifications:  
Copyright © 2014 Brandon Bloom

Both are distributed under the Eclipse Public License either version 1.0 or
(at your option) any later version.


[1]: http://okmij.org/ftp/Haskell/extensible/
[2]: https://github.com/TiarkRompf/lancet
