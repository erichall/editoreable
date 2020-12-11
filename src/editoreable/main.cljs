(ns editoreable.main
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [editoreable.interop :as interop]
            [editoreable.core :refer [editor]]
            ))

(defn render
  []
  (rd/render [editor] (interop/get-element-by-id "app")))

(defn init! []
  (render))

(defn reload! []
  (render))

