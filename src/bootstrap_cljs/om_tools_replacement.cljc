(ns bootstrap-cljs.om-tools-replacement)

#+clj
(defn literal?
  "Returns true if form is a literal value (number, string, map, etc),
  otherwise false."
  [form]
  (not (or (symbol? form)
           (list? form))))

(defn- opt-key-alias
  "Converts aliased attributes"
  [opt]
  (case opt
    :class :className
    :for :htmlFor
    opt))


(defn format-opt-key
  "Returns potentially formatted name for DOM element attribute.
   Converts kebab-case to camelCase."
  [opt-key]
  (-> opt-key
      opt-key-alias
      name
      opt-key-case
      keyword))

(declare format-opts)

(defn format-opt-val
  "Returns potentially modified value for DOM element attribute.
   Recursively formats map values (ie :style attribute)"
  [opt-val]
  (cond
   (map? opt-val)
   (format-opts opt-val)

   #+clj
   (not (literal? opt-val))
   #+clj
   `(format-opts ~opt-val)

   :else
   opt-val))

(defn format-opts
  "Returns JavaScript object for React DOM attributes from opts map"
  [opts]
  (if (map? opts)
    (->> opts
         (clojure.core/map
          (fn [[k v]] [(format-opt-key k) (format-opt-val v)]))
         (into {})
         clj->js)
    opts))

(defn element-args
  "Returns a vector of [opts children] for from first and second
  argument given to DOM function"
  [opts children]
  (cond
   (nil? opts) [nil children]
   (map? opts) [(format-opts opts) children]
   (js-opts? opts) [opts children]
   :else [nil (cons opts children)]))

(defn ^boolean possible-coll? [form]
  (or (coll? form)
      (symbol? form)
      (list? form)))

(defn element [ctor opts children]
  (let [[opts children] (element-args opts children)]
    (apply ctor (flatten (cons opts children)))))
