(ns planetwit
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.contrib.zip-filter :as zf])
  (:use clojure.contrib.zip-filter.xml
        clojure.contrib.duck-streams))

(def +state-file+ "h:/clojure/planetwit.dat")
(def +twitter-url+ "http://twitter.com/statuses/update.xml")
(def +twitter-auth-file+ "h:/clojure/planetwit-auth.dat")

(defn read-file [file-name & defaults]
  (try
   (with-in-str (slurp file-name)
     (read))
   (catch java.io.FileNotFoundException e
     (first defaults))))

(defn write-file [data file-name]
  (spit file-name (with-out-str (pr data))))

(defn load-data []
  (read-file +state-file+ #{}))

(defn save-data [data]
  (write-file data +state-file+))

(defn feed-to-zip [url]
  (zip/xml-zip (xml/parse url)))

(defn update-twitter-status [auth-file status]
  (http-client/url-do +twitter-url+
                      "POST"
                      {:set-header { "Authorization" (format "Basic %s" (read-file auth-file)) } }
                      (format "status=%s&source=planetlisp" (java.net.URLEncoder/encode status))))

(defn maybe-post-twit [items]
  (let [twitter-status (cond
                        (< 1 (count items))
                        (format "%d new items posted" (count items))
                        (= 1 (count items))
                        (format "new: %s" (first items)))]
    (when twitter-status
      (update-twitter-status +twitter-auth-file+ twitter-status))))

(defn poll
  "Poll planet lisp, check for new postings, update Twitter status when new postings have appeared"
  []
  (save-data
   (let [old-data (load-data)
         process
         (fn [items new-data new-items]
           (if items
             (let [item (first items)
                   guid (first (xml-> item :guid text)) ]
               (recur (rest items)
                      (conj new-data guid)
                      (if (old-data guid)
                        new-items
                        (conj new-items (first (xml-> item :title text))))))
             (do
               (maybe-post-twit new-items)
               new-data)))]
     (process (xml-> (feed-to-zip "http://planet.lisp.org/rss20.xml") :channel :item)
              #{} []))))