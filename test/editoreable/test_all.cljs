(ns editoreable.test-all
  (:require [cljs.test :refer [is successful? run-tests deftest]]
            [editoreable.core]))


(deftest test-core (is (successful? (run-tests 'editoreable.core))))

(defn init
  []
  (test-core))

