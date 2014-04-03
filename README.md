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

Woefully incomplete and highly experimental!

The majority of special forms are implemented.

Notably, forms related to classes delegate to clojure.core/eval:
`deftype`, `reify`, `defprotocol`, etc.


## Usage

The only public API is `eclj.core/eval`. It is used just like Clojure's eval.


## License

Copyright © 2014 Brandon Bloom

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


[1]: http://okmij.org/ftp/Haskell/extensible/
[2]: https://github.com/TiarkRompf/lancet
