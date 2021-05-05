(ns demo.core
  (:use tupelo.core)
  (:require
    [clojure.data.csv :as csv]
    [cambium.core :as log]
    [schema.core :as s])
  (:gen-class))

(def Date s/Str)
(def AdjClosePrice s/Num)

(s/defn read-yahoo-finance-csv :- {Date AdjClosePrice}
  "Returns a map of the adjusted close price where the key is the date
  See tst.demo.core for an example of how this function works"
  [path :- s/Str]
  (->> path
       slurp
       csv/read-csv
       rest
       (mapv (juxt first
                   #(Double. (nth % 5))))
       (into (sorted-map))))

(defn calculate-daily-weights
  "For each ticker and date, calculate the % allocated to each ticker.
  See https://www.investopedia.com/terms/s/strategicassetallocation.asp for context"
  [target-weights ticker-prices rebalance-frequency]
  (let [ticker-prices-weighted (into {}
                                 (map
                                   (fn [[ticker price-map]]
                                     (let [target-weight (get target-weights ticker)]
                                       [ticker (into {}
                                                 (map (fn [[date price]]
                                                        [date (* price target-weight)])
                                                      price-map))]))
                                   ticker-prices))
        weighted-sum (apply
                       merge-with
                       +
                       (vals ticker-prices-weighted))
        ticker-weights (into {}
                         (map
                           (fn [[ticker price-map]]
                             [ticker (merge-with / price-map weighted-sum)])
                           ticker-prices-weighted))]
    ticker-weights))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (log/info "-main - enter")

  (println "Hello, World! Again!")

  (log/info "-main - leave"))

