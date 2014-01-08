(ns colorcrunch.dom)

(defn elt [tag attrs & content]
  (let [e (.createElement js/document tag)]
    (doseq [[attr val] (seq attrs)]
      (.setAttribute e (name attr) val))
    (doseq [c content]
      (if (string? c)
        (.appendChild e (.createTextNode js/document c))
        (.appendChild e c)))
    e))

(defn listen [elt type handler]
  (.addEventListener elt (if (string? type)
                           type
                           (name type))
    handler))

(defn set-text! [elt text]
  (set! (.-innerText elt) (str text)))
