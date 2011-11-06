(ns uploadr.uploadr
  (:use     [clojure-http.client] 
            [clojure.contrib.io :only (to-byte-array)] 
            [clojure.java.io :only (as-file)])
  (:require [clojure-http.resourcefully :as res] 
            [clojure.xml :as xml]) 
  (:import (java.security NoSuchAlgorithmException MessageDigest)
           (org.xml.sax InputSource)
           (java.io ByteArrayInputStream OutputStreamWriter BufferedReader InputStreamReader FileReader)
           (java.net URL))
)

(def api-key "XXXXXXXXXXXXXXXXXXXXXXXXXX")
(def api-secret "XXXXXXXXXXXXXXXXXXX")
(def upload-boundary "---------------------------deadbeef")

(defn md5code "Returns the MD5 code of a string" [#^String inp] 
  (let [alg (doto (MessageDigest/getInstance "MD5")
              (.reset)
              (.update (.getBytes inp)))]
    (try
      (.toString (new BigInteger 1 (.digest alg)) 16)
      (catch NoSuchAlgorithmException e
        (throw (new RuntimeException e))))))

(defn get-signature "Returns the signature for a call" [secret param-map]
  (let [sorted-param-map (into (sorted-map) param-map)]
    (loop [items [secret]
           sorted-keys (seq (keys sorted-param-map))
           sorted-vals (seq (vals sorted-param-map))]
      (if (and sorted-keys sorted-vals)
        (recur (into items [(first sorted-keys) (first sorted-vals)] )
               (next sorted-keys)
               (next sorted-vals))
        (md5code (apply str items))))))

(defn create-signed-url "Adds signature to a call" [url params secret]
  (loop [final-url (str url "?")
         pkeys (seq (keys params))
         pvals (seq (vals params))]
    (if (and pkeys pvals)
      (recur (str final-url (first pkeys) "=" (first pvals) "&" ) (next pkeys) (next pvals))
      (str final-url "api_sig=" (get-signature secret params ) )))) 

(defn call-flickr-getfrob "getFrob call" []
  (let [response (res/get (create-signed-url 
                            "http://flickr.com/services/rest/" 
                            {"method" "flickr.auth.getFrob" "api_key" api-key} 
                            api-secret))]
    (response :body-seq)))

(defn call-flickr-gettoken "getToken call" [frob]
  (let [response (res/get (create-signed-url
                            "http://flickr.com/services/rest/"
                            {"method" "flickr.auth.getToken" "api_key" api-key "frob" frob}
                            api-secret))]
    (response :body-seq)))

(defn get-xml-element-content "returns the text content of elements found in document" [document elmnt] 
  (first (first 
    (let [xmlSource (ByteArrayInputStream. (.getBytes (apply str document) "UTF-8")) ] 
      (for [x (xml-seq (xml/parse xmlSource)) 
           :when (= elmnt (:tag x))]
       (:content x))))))

(defn authenticate "Authenticates with flickr, returns the token" []
(do (print "Getting FROB:")
  (let [frob (get-xml-element-content (call-flickr-getfrob) :frob) ]
    (do 
      (println frob)
      (println (str "Open this URL: " (create-signed-url "http://flickr.com/services/auth/" 
                              {"api_key" api-key "perms" "write" "frob" frob} api-secret) "\n, and press ENTER"))
      (flush)
      (let [inp (read-line) 
            resp (call-flickr-gettoken frob) 
            token (get-xml-element-content resp :token) ] 
        (do 
          (println (str "Got token:" token))
          token))))))

(defn get-token "Gets the token by authenticating or reading the local store" []
  (if (:file (bean (as-file "token-store")))
    (slurp "token-store")
    (let [token (authenticate)]
      (do
        (spit "token-store" token)
        token))))

(defn call-upload-file "Photo upload call" [photo auth-token]
  (let [body1 (str "--" upload-boundary "\r\nContent-Disposition: form-data; name=\"api_key\"\r\n\r\n" api-key "\r\n"
                   "--" upload-boundary "\r\nContent-Disposition: form-data; name=\"auth_token\"\r\n\r\n" auth-token "\r\n"
                   "--" upload-boundary "\r\nContent-Disposition: form-data; name=\"api_sig\"\r\n\r\n" (get-signature api-secret {"api_key" api-key "auth_token" auth-token "is_public" "0"}) "\r\n"
                   
                   ;"--" upload-boundary "\r\nContent-Disposition: form-data; name=\"title\"\r\n\r\n"        "." "\r\n"
                   ;"--" upload-boundary "\r\nContent-Disposition: form-data; name=\"description\"\r\n\r\n"  "" "\r\n"
                   "--" upload-boundary "\r\nContent-Disposition: form-data; name=\"is_public\"\r\n\r\n"    "0" "\r\n"
                   ;"--" upload-boundary "\r\nContent-Disposition: form-data; name=\"tags\"\r\n\r\n"   ""    "" "\r\n"
                   
                   "--" upload-boundary "\r\nContent-Disposition: form-data; name=\"photo\"; filename=\"" (:name (bean photo)) "\"\r\n" 
                   "Content-Type: application/octet-stream\r\n\r\n" )
        body2 (str "\r\n" "--" upload-boundary "--\r\n")
        conn (doto (.openConnection (URL. "http://api.flickr.com/services/upload/")) 
                   (.setDoOutput true) 
                   (.setRequestProperty "Content-Type" (str "multipart/form-data;boundary=" upload-boundary) ) )
        out (OutputStreamWriter. (.getOutputStream conn))
        ] 
    (do
      (.write out body1)
      (.flush out)
      (doto (.getOutputStream conn) (.write (to-byte-array photo)) (.flush) )
      (.write out body2)
      (.flush out)
      (with-open [in (BufferedReader. (InputStreamReader. (.getInputStream conn)))] 
        (doseq [line (line-seq in)] (println line)))
      (.close out))))

(defn recursively-list-directory [^java.io.File directory file-list]
  (let [contents (seq (.listFiles directory))]
    (loop [remaining-contents (rest contents) current-file (first contents) return-list file-list]
      (if (not (nil? current-file))
	       (recur 
          (rest remaining-contents) 
          (first remaining-contents)
          (if (.isDirectory current-file)
            (recursively-list-directory current-file return-list)
	          (if (some #(.endsWith (.toUpperCase (.getName current-file)) % ) [".JPG" ".AVI" ".MTS"])
	            (do 
	              (println (str "Found media file " (.getName current-file)))
	              (conj return-list current-file))
	            return-list)))
          return-list))))

(defn return-creation-date [^java.io.File file]
  (java.util.Date. (.lastModified file))
  )

(defn return-after [l after-element]
  (if (nil? after-element) l
  (loop [ret-list l]
    (if (or (nil? (first ret-list)) (.equalsIgnoreCase after-element (.getName (first ret-list)))) 
      (rest ret-list)
      (do (println (str "Skipping " (.getName (first ret-list)))) (recur (rest ret-list)))))))


(let [token (get-token)]
  (doseq [arg-file-name *command-line-args*]
    (let [arg-file (as-file arg-file-name)
          arg-file-data (bean arg-file)
          filter-for-file-name (if (= (first *command-line-args*) "--from-file") (nth *command-line-args* 1) nil )]
      (if (:directory arg-file-data)
        (do 
          (println (str "searching " (:canonicalPath arg-file-data) (:name arg-file-data)))
          (println  (map #(do (println (.getName %)) (call-upload-file % token)) 
            (return-after (sort-by return-creation-date (recursively-list-directory arg-file [] )) filter-for-file-name)))
          (do 
            (println "Uploading single file") 
            (call-upload-file arg-file token)))
        (println (str "skipping" arg-file))))))


(comment
(let [flickr-api-token (get-token)
      command-line-switches (take-while #(.startsWith % "--" ) *command-line-args* ) 
      file-arguments (vector (drop-while #(.startsWith % "--" ) *command-line-args* )) ]
      (println command-line-switches "\n" file-arguments))
)


