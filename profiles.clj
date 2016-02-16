{:user
 {:plugins
  [[speclj "3.3.1"]
   [lein-ring "0.9.7"]
   [lein-ancient "0.6.8"]
   [cider/cider-nrepl "0.10.2"]]
  :dependencies [[spyscope "0.1.5"]
                 [im.chit/vinyasa "0.4.2"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [im.chit/iroh "0.1.11"]
                 [io.aviso/pretty "0.1.23"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [leiningen "2.6.1"]]
  :injections [(require 'spyscope.core)
               (require '[vinyasa.inject :as inject])
               (require 'io.aviso.repl)
               (inject/in
                 [vinyasa.lein :exclude  [*project*]]
                 clojure.core
                 [iroh.core .> .? .* .% .%>]
                 clojure.core >
                 [clojure.repl doc pst]
                 [clojure.pprint pprint pp]
                 [clojure.tools.namespace.repl refresh])]}}
