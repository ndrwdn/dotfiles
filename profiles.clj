{:user
 {:plugins
  [[speclj "3.3.1"]
   [lein-ring "0.9.6"]
   [lein-ancient "0.6.7"]
   [cider/cider-nrepl "0.9.0"]]
  :dependencies [[spyscope "0.1.5"]
                 [im.chit/vinyasa "0.3.4"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [im.chit/iroh "0.1.11"]
                 [io.aviso/pretty "0.1.18"]
                 [org.clojure/tools.nrepl "0.2.10"]
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
                 [clojure.tools.namespace.repl refresh])]}}
