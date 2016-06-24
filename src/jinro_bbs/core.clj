(ns jinro-bbs.core
  (:require [net.cgrand.enlive-html :as en]))

;; there seems to be two types of message.
;; :announce, :message
(defrecord Message
  [type text name time])

(def bbs-url "http://www.wolfg.x0.com/")

(defn get-all-page-of [n]
  (let [first-page (en/html-resource (java.net.URL. (str bbs-url "index.rb?vid=" n)))
        all-links (map #(-> % :attrs :href) (en/select first-page [:body :div :p :a]))]
    (map #(let [url (java.net.URL. (str bbs-url %))]
            (en/html-resource url))
         all-links)))

(defn get-main [res]
  (en/select res [:div.main :div.message]))

(defn type-of [main-content]
  (if (-> main-content :content first :attrs :class)
    :announce :message))

(defn parse-announce [announce]
  (let [as (-> announce :content first :content)]
    (Message.
      :announce
      (clojure.string/join " " (filter string? as))
      nil nil)))

(defn parse-message [announce]
  (Message.
    :message
    (clojure.string/join " " (filter string? (-> announce :content (nth 7) :content (nth 1) :content (nth 5) :content first :content first :content)))
    (-> announce :content (nth 3) :content first)
    (-> announce :content (nth 5) :content first)
    ))

(defn parse-thought [thought]
  (Message.
    :thought
    (clojure.string/join " " (filter string? (-> thought :content (nth 5) :content (nth 1) :content (nth 5) :content first :content first :content)))
    (-> thought :content (nth 1) :content first)
    (-> thought :content (nth 3) :content first)
))

(defn parse-content [main-content]
  (condp = (type-of main-content)
    :announce (parse-announce main-content)
    :message  (try (parse-message main-content) (catch Exception e (parse-thought main-content)))))

(defn format-output
  [parsed-message]
  (clojure.string/join " ||| "
                       (map #(% parsed-message)
                            [:type :text :name :time])))

(defn -main [& args]
  (loop [n 1]
    (when (< n 1610)
      (println n)
      (loop [day 1 all-parsed-dates (map #(map parse-content (get-main %)) (get-all-page-of n))]
        (when (seq all-parsed-dates)
          (let [filename (str "log/" n "/" day)]
            (clojure.java.io/make-parents filename)
            (spit filename (clojure.string/join "\n" (map format-output (first all-parsed-dates))))
            (recur (inc day) (rest all-parsed-dates)))))
      (recur (inc n)))))
