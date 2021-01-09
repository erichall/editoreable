# Web Text Editor ![jenkins-build-status](https://byggarn.erkanp.dev/job/editoreable/badge/icon "")

A basic text editor for the web, built with [Clojurescript](https://clojurescript.org/).

Live demo at [editoreable.erkanp.dev](https://editoreable.erkanp.dev)

# Motivation

An attempt to build a text editor from bare divs for fun and learning!

# Usage
In your shadow-cljs.edn
```clojure
:dependencies [
                ...
                [editoreable "0.1.0-SNAPSHOT"]
                ]
```

```clojure
(ns rock-and-roll-ns.main
  (:require [reagent.dom :as rd]
            [editoreable.core :refer [editor]]))

(defn render
  []
  (rd/render
    [:div {:style {:display         "flex"
                   :flex-direction  "column"
                   :align-items     "center"
                   :justify-content "center"}}
     [editor {:on-text-change (fn [txt] (println txt))}]]))

(defn init! []
  (render))
```

# Demo
![](demo.gif)

# Q^A
### Does it work?
Kinda.

### On mobile?
Not yet! 

### IE? 
NO!

### Should you use it?
For your own safety, probably not. 



