(ns eclj.core
  (:refer-clojure :only [])
  (:require [eclj.interpret.cps :as cps]
            [eclj.env :as env]
            [eclj.common :refer (map->Syntax)]))

(clojure.core/defn eval
  ([form]
   (eval form (env/ns-env)))
  ([form env]
   (cps/interpret-result form env)))

(clojure.core/intern 'eclj.core 'eval eval)
(clojure.core/require '[eclj.ns :as ns])

(ns/publish-vars 'clojure.core :exclude '#{
  eval case ns deftype defrecord defprotocol refer-clojure apply
})

(defn load
  "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  [& paths]
  (doseq [path paths]
    (ns/load path)))

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded. Use :require in the ns macro
  in preference to calling this directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace or the :all
    keyword to bring in all public vars.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the libraries clojure.zip and clojure.set
  abbreviated as 's'.

  (require '(clojure zip [set :as s]))"
  [& args]
  (clojure.core/apply ns/load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  eclj.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for eclj.core/refer."
  [& args]
  (clojure.core/apply ns/load-libs :require :use args))

(eclj.ns/load-eclj "eclj/core/base.eclj")
(eclj.ns/load-eclj "eclj/core/case.eclj")
(eclj.ns/load-eclj "eclj/core/deftype.eclj")
(eclj.ns/load-eclj "eclj/core/defprotocol.eclj")
(eclj.ns/load-eclj "eclj/core/protocols.eclj")
(eclj.ns/load-eclj "eclj/core/jvm.eclj")
(eclj.ns/load-eclj "eclj/core/bools.eclj")
(eclj.ns/load-eclj "eclj/core/metadata.eclj")
(eclj.ns/load-eclj "eclj/core/seqs.eclj")
(eclj.ns/load-eclj "eclj/core/colls.eclj")
(eclj.ns/load-eclj "eclj/core/maps.eclj")
(eclj.ns/load-eclj "eclj/core/sets.eclj")
(eclj.ns/load-eclj "eclj/core/fns.eclj")
(eclj.ns/load-eclj "eclj/core/strs.eclj")
(eclj.ns/load-eclj "eclj/core/names.eclj")
(eclj.ns/load-eclj "eclj/core/flow.eclj")
