(defproject sporkle "0.1.0-SNAPSHOT"
  :description "Pure-Clojure Java classfile assembler and disassembler"
  :url "http://github.com/CodeFarmer/sporkle"
  :plugins [[lein-cloverage "1.0.0"] [jonase/eastwood "0.1.4"]]
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.14.0"]]}})
