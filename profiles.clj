{:user
 {:plugins
  [[speclj "3.1.0"]
   [lein-ring "0.9.2"]
   [lein-ancient "0.6.3"]
   [cider/cider-nrepl "0.8.2"]]
  :dependencies [[spyscope "0.1.5"]
                 [im.chit/vinyasa "0.3.3"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [im.chit/iroh "0.1.11"]
                 [io.aviso/pretty "0.1.17"]
                 [leiningen "2.5.1"]]
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
                 [clojure.tools.namespace.repl refresh])]
  :repl-options
  {:nrepl-middleware [io.aviso.nrepl/pretty-middleware]}}}
