(defproject core.rdf "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "_"]
                 ]

  ;;:sub ["core.rdf" "sesame"]


  :plugins [[lein-modules "0.3.11"]]

  :modules {:versions {org.clojure/clojure "1.8.0"
                       grafter/core.rdf :version}
            }
  ;;:plugins [[lein-sub "0.3.0"]]
  )
