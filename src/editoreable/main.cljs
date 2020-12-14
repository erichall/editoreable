(ns editoreable.main
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [editoreable.interop :as interop]
            [editoreable.core :refer [editor]]
            ))

(defn render
  []
  (rd/render
    [:div {:style {
                   :display         "flex"
                   :margin-top      "200px"
                   :flex-direction  "column"
                   :align-items     "center"
                   :justify-content "center"}}
     [editor {:style          {:min-width "500px"}
              :on-text-change (fn [txt] (println txt))}]]

    (interop/get-element-by-id "app")))

(defn init! []
  (render))

(defn reload! []
  (render))

