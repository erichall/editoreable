(defproject editoreable "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.9.1"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/core.async "0.4.500"]
                 [reagent "0.10.0" :exclusions [cljsjs/react cljsjs/react-dom cljsjs/react-dom-server]]]

  :plugins [[lein-figwheel "0.5.20"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]
  :test-paths ["test"]

  :cljsbuild {:builds
              [{:id           "dev"
                :source-paths ["src"]

                :compiler     {:main                 editoreable.core
                               :target               :bundle
                               :asset-path           "js/compiled/out"
                               :output-to            "resources/public/js/compiled/out/index.js"
                               :output-dir           "resources/public/js/compiled/out"
                               :bundle-cmd           {:none    ["npx" "webpack" "--mode=development"]
                                                      :default ["npx" "webpack"]}
                               :source-map-timestamp true
                               ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
                               ;; https://github.com/binaryage/cljs-devtools
                               :preloads             [devtools.preload]}}
               ;; This next build is a compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once min
               {:id           "min"
                :source-paths ["src"]
                :compiler     {:output-to     "resources/public/js/compiled/editoreable.js"
                               :main          editoreable.core
                               :optimizations :advanced
                               :pretty-print  false}}]}

  :profiles {:dev {:dependencies  [[binaryage/devtools "1.0.0"]
                                   [figwheel-sidecar "0.5.20"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths  ["src" "dev"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     :target-path]}})
