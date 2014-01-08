(defproject colorcrunch "0.1.0-SNAPSHOT"
  :description "Color Crunch Chronicles: A game where you smash color shapes by matching three of a kind."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
  :plugins [[lein-cljsbuild "1.0.1"]
            [com.cemerick/clojurescript.test "0.2.1"]
            ]
  :hooks [leiningen.cljsbuild]
  
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "main.js"
                                   :output-dir "out"
                                   :optimizations :none
                                   :source-map true}}
                       
                       {:id "prod"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "colorcrunch.js"
                                   :output-dir "prod"
                                   :optimizations :advanced
                                   }}
                       
                       ]}
              
              

  
)
