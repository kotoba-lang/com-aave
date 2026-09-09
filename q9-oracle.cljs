(ns q9-oracle
  "The Clojure side of the whole-component acceptance: run `aave.main` -- the
  oracle this port is a port OF -- under nbb, and print the same numbers the
  aarch64 artifact prints.

    nbb --classpath src q9-oracle.cljs

  Two kinds of answer are printed, and they are not equally strong.

  * The i64 answers (as-int, as-bool, page-limit, filter counts, pagination
    counts, handler status codes, store counts) call `aave.main` and count.
    Nothing is rendered on the way, so a disagreement is a disagreement about
    the component's behaviour.
  * The hash answers go through a rendering. `str-hash` is the same rolling
    hash the .kotoba computes (acc*131 + code-point, mod 1000000007, seeded
    7; every intermediate stays under 2^53 so a JS double is exact). The
    STRING being hashed is built here from `aave.main`'s own values, but the
    JSON shape around them is this port's convention, so a disagreement means
    either a behaviour difference or a rendering difference and has to be read
    before it is believed."
  (:require [aave.main :as m]
            [clojure.string :as s]))

(defn str-hash [x]
  (reduce (fn [acc ch] (mod (+ (* acc 131) (.charCodeAt ch 0)) 1000000007)) 7 (seq (str x))))

(defn jstr [v] (str "\"" v "\""))
(defn jkv [k v] (str (jstr k) ":" (jstr v)))
(defn jkv-raw [k raw] (str (jstr k) ":" raw))

;; --- the entity table -----------------------------------------------------

(defn coerce-csv [spec]
  (s/join "," (map (fn [[k v]] (str (name k) ":" (name v))) (:coerce spec))))

(defn table-line [spec]
  (s/join "|" [(:entity spec) (:plural spec) (:id-prefix spec)
               (s/join "," (map name (:fields spec)))
               (s/join "," (map name (:required spec)))
               (coerce-csv spec)]))

(defn route-line [r]
  (s/join " " [(:method r) (:path r) (str (:op r) "|" (:entity r))]))

;; --- validation, rendered the way the .kotoba renders it ------------------

(defn error-json [err]
  (if (nil? err)
    ""
    (str "{" (jkv-raw "error"
                      (str "{" (jkv "message" (get-in err [:error :message])) ","
                           (jkv "type" (get-in err [:error :type])) "}"))
         "}")))

;; --- fixtures, the same four documents the .kotoba carries ----------------

(def fixture-data
  {0 {:liquidityIndex "12" :currentLiquidityRate 7}
   1 {:liquidityIndex "12"}
   2 {:liquidityIndex 1 :currentLiquidityRate 2 :bogus "x"}
   3 {:ltv " 80 " :liquidationThreshold 85 :isolated "YES" :label "stable"}
   4 {:data "7"}
   5 {}})

(def fixture-text
  {0 "12" 1 " 42 " 2 "-7" 3 "12abc" 4 "" 5 "true" 6 "TRUE" 7 "Yes" 8 "on"
   9 "1" 10 "0" 11 "false" 12 "null" 13 "nope"})

(defn seeded-row [i]
  {:id (str "aave_res_" i)
   :liquidityIndex i
   :currentLiquidityRate (if (even? i) "even" "odd")})

(defn seeded-store [n]
  (let [st (m/fresh-store)]
    (doseq [i (range n)] (m/persist! st "ReserveData" (seeded-row i)))
    st))

(defn spec-for [e] (first (filter #(= (:entity %) e) m/entity-specs)))

;; --- print ----------------------------------------------------------------

(defn row [label v] (println (str label "\t" v)))

(println "; oracle = aave.main under nbb")
(row "main(route-count)" (count m/routes))
(row "entities" (s/join "," m/entities))

(println "; as-int")
(doseq [i [0 1 2 3 4 5 12]] (row (str "as-int " i) (m/as-int (fixture-text i))))
(println "; as-bool")
(doseq [i [5 6 7 8 9 10 11 4 13]] (row (str "as-bool " i) (if (m/as-bool (fixture-text i)) 1 0)))
(println "; page-limit")
(doseq [r [-5 0 1 20 99 100 101 250]] (row (str "page-limit " r) (m/page-limit (m/as-int r))))

(println "; table (hash)")
(doseq [i (range 4)] (row (str "table " i) (str-hash (table-line (nth m/entity-specs i)))))
(println "; route-shape (hash)")
(doseq [i (range (count m/routes))] (row (str "route-shape " i) (str-hash (route-line (nth m/routes i)))))

(println "; validation (hash)")
(row "validation 0" (str-hash (error-json (m/require-fields (fixture-data 0) (:required (spec-for "ReserveData"))))))
(row "validation 1" (str-hash (error-json (m/require-fields (fixture-data 1) (:required (spec-for "ReserveData"))))))
(row "validation 2" (str-hash (error-json (m/reject-unknown (fixture-data 0) (:fields (spec-for "ReserveData"))))))
(row "validation 3" (str-hash (error-json (m/reject-unknown (fixture-data 2) (:fields (spec-for "ReserveData"))))))
(row "validation 4" (str-hash (error-json (m/require-fields (fixture-data 5) (:required (spec-for "ReserveData"))))))
(row "validation 5" (str-hash (error-json (m/reject-unknown (fixture-data 3) (:fields (spec-for "EModeCategory"))))))

(println "; store")
(row "store 0" (count (m/query (seeded-store 3) "ReserveData")))
(row "store 3" (let [st (seeded-store 3)] (m/retract! st "ReserveData" "aave_res_1")
                    (count (m/query st "ReserveData"))))
(row "store 4" (let [st (seeded-store 3)] (m/retract! st "ReserveData" "nope")
                    (count (m/query st "ReserveData"))))
(row "store 5" (let [st (seeded-store 3)] (m/persist! st "ReserveData" (seeded-row 1))
                    (count (m/query st "ReserveData"))))
(row "store 7" (count (m/query (seeded-store 3) "EModeCategory")))

(println "; filters")
(let [rows (m/query (seeded-store 5) "ReserveData")
      fields (:fields (spec-for "ReserveData"))]
  (row "filters 0" (count (m/apply-filters rows {} fields)))
  (row "filters 1" (count (m/apply-filters rows {:currentLiquidityRate "even"} fields)))
  (row "filters 2" (count (m/apply-filters rows {:currentLiquidityRate ""} fields)))
  (row "filters 3" (count (m/apply-filters rows {:liquidityIndex "3"} fields)))
  (row "filters 4" (count (m/apply-filters rows {:liquidityIndex "99"} fields)))
  (row "filters 5" (count (m/apply-filters rows {:notAField "x"} fields))))

(println "; paginate  page*10 + has_more")
(doseq [[n lim] [[5 2] [5 5] [5 0] [5 200] [3 2] [0 2]]]
  (let [rows (m/query (seeded-store n) "ReserveData")
        [page more] (m/paginate rows {:limit lim})]
    (row (str "paginate " n " " lim) (+ (* 10 (count page)) (if more 1 0)))))

(println "; handlers (status)")
(let [mk (fn [] (seeded-store 2))]
  (row "handlers 0" (second (m/handle-create (mk) "ReserveData" (fixture-data 0))))
  (row "handlers 1" (second (m/handle-create (mk) "ReserveData" (fixture-data 1))))
  (row "handlers 2" (second (m/handle-create (mk) "ReserveData" (fixture-data 2))))
  (row "handlers 3" (second (m/handle-list (mk) "ReserveData" {})))
  (row "handlers 4" (second (m/handle-get (mk) "ReserveData" "aave_res_0" {})))
  (row "handlers 5" (second (m/handle-get (mk) "ReserveData" "nope" {})))
  (row "handlers 6" (second (m/handle-update (mk) "ReserveData" "aave_res_0" {:liquidityIndex "99"})))
  (row "handlers 7" (second (m/handle-update (mk) "ReserveData" "nope" {})))
  (row "handlers 8" (second (m/handle-delete (mk) "ReserveData" "aave_res_0")))
  (row "handlers 9" (second (m/handle-delete (mk) "ReserveData" "nope")))
  (row "handlers 10" (let [st (mk)] (m/handle-delete st "ReserveData" "aave_res_0")
                          (count (m/query st "ReserveData"))))
  (row "handlers 11" (let [st (mk)] (m/handle-create st "ReserveData" (fixture-data 0))
                          (count (m/query st "ReserveData"))))
  (row "handlers 12" (get (first (m/handle-list (mk) "ReserveData" {})) :total))
  (row "handlers 13" (:liquidityIndex (first (m/handle-update (mk) "ReserveData" "aave_res_0"
                                                              {:liquidityIndex "99"})))))

(println "; rfc3339 (hash) -- oracle is (str (Instant/ofEpochSecond s)) shape")
(doseq [secs [0 1 1000000000 1757000000]]
  (row (str "rfc3339 " secs)
       (str-hash (s/replace (.toISOString (js/Date. (* 1000 secs))) ".000Z" "Z"))))

(println "; expand -- refs is {} on all four specs, so selector 1 hands the fold a")
(println ";           synthetic ref table so the branch actually runs")
(let [st (seeded-store 3)
      rec (seeded-row 1)
      json (fn [r] (str "{" (s/join "," (map (fn [[k v]]
                                               (jkv-raw (name k)
                                                        (cond (nil? v) "null"
                                                              (string? v) (jstr v)
                                                              (map? v) "OBJ"
                                                              :else (str v))))
                                             r)) "}"))]
  (row "expand 0 changed?" (if (= rec (m/expand st rec {:expand "liquidityIndex"} {})) 0 1))
  (row "expand 1 changed?" (if (= rec (m/expand st rec {:expand "liquidityIndex"}
                                                {:liquidityIndex "ReserveData"})) 0 1))
  (row "expand 2 changed?" (if (= rec (m/expand st rec {:expand "other"}
                                                {:liquidityIndex "ReserveData"})) 0 1))
  (row "expand 1 obj-is-nil?" (if (nil? (:liquidityIndex_obj
                                         (m/expand st rec {:expand "liquidityIndex"}
                                                   {:liquidityIndex "ReserveData"}))) 1 0)))

(println "; emit-facts (hash) and healthz (hash), rendered in this port's JSON shape")
(row "facts 1" (str-hash (str "{" (s/join "," (map (fn [[k v]]
                                                     (jkv-raw k (if (string? v) (jstr v) (str v))))
                                                   (m/emit-facts "ReserveData" (seeded-row 1)))) "}")))
(let [[body status] (m/healthz)]
  (row "healthz" (str-hash (str "{" (jkv-raw "status" (str status)) ","
                                (jkv-raw "body"
                                         (str "{" (jkv "status" (:status body)) ","
                                              (jkv "actor" (:actor body)) ","
                                              (jkv "tier" (:tier body)) ","
                                              (jkv-raw "entities"
                                                       (str "[" (s/join "," (map jstr (:entities body))) "]"))
                                              "}"))
                                "," (jkv-raw "store" "{}") "}"))))
