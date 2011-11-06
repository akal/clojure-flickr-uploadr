;;  Copyright (c) Dirk Vleugels. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;  which can be found in the file CPL.TXT at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.
;;
;;  http-client.clj 0.1
;;
;;  Usage:
;;
;;  (http-client/url-do "https://gmail.com" "GET" { :read-timeout 5000
;;                                                  :set-header { "User-Agent" "Clojure-HttpClient/0.1 
;;                                                                "X-Header-xxx" "This is a test" }
;;                                                  :connect-timeout 1000
;;                                                  :http-proxy "http://proxy.org:8080"
;;                                                  :follow-redirect false
;;                                                  :use-caches false } ))
;;  (http-client/url-do "https://gmail.com" "POST" {:read-timeout 5000
;;                                                  :connect-timeout 1000
;;                                                  :use-caches false }
;;                                                  "this is a post test"))
;;
;; Returns a map or (currently) throws a exception
;;
;; {:return-message "Found", :return-code 302, :header {nil [HTTP/1.1 302 Found], "Server" [GFE/1.3], "Content-Type" [text/html; charset=UTF-8], "Date" [Thu, 10 Jul 2008 19:31:52 GMT], "Location" [http://www.google.com], "Content-Length" [218]}, :body [B@bed1e7}
;;
;;  A clojure interface for the jdk http client (requires jdk 1.5)
;;  - ignores SSL cert or host verification problems (thats what i want 99% of the time)
;;
;; Todo:
;;  - exception handling + resource cleanup, logging?
;;
;;  dirk.vleugels (gmail)
;;  8 Juli 2008

(load-file "/Users/dvl/work/clojure/svn/clojure/trunk/src/genclass.clj")
(clojure/in-ns 'http-client)
(clojure/refer 'clojure)


(import '(java.net URL URLConnection InetSocketAddress Proxy Proxy$Type)
        '(java.io Reader InputStream InputStreamReader FileReader
      BufferedReader File PrintWriter OutputStream ByteArrayOutputStream
      OutputStreamWriter BufferedWriter Writer FileWriter)
        '(javax.net.ssl X509TrustManager HostnameVerifier SSLContext HttpsURLConnection)
        '(java.security.cert X509Certificate))

(def http-defaults {:read-timeout 10000
                    :connect-timeout 3000
                    :http-proxy ""
                    :set-header { "User-Agent" "Clojure-HttpClient/0.1" }
                    :use-caches false
                    :follow-redirects false})

;; provide dummy trust manager
(let [ clazz "httpclient.CljTrustManager" ]
  (try (. Class (forName (str clazz)))
       (catch java.lang.ClassNotFoundException e
         (gen-and-load-class (str clazz) :implements [javax.net.ssl.X509TrustManager]))))
(clojure/in-ns 'httpclient.CljTrustManager)
(clojure/refer 'clojure)
(defn checkClientTrusted [this chain auth-type])
(defn checkServerTrusted [this chain auth-type])
(defn getAcceptedIssuers [this ] nil)
(clojure/in-ns 'http-client)
;; end

;; provide hostname verifier
(let [ clazz "httpclient.CljHostnameVerifier" ]
  (try (. Class (forName (str clazz)))
       (catch java.lang.ClassNotFoundException e
         (gen-and-load-class (str clazz) :implements [javax.net.ssl.HostnameVerifier]))))
(clojure/in-ns 'httpclient.CljHostnameVerifier)
(clojure/refer 'clojure)
(defn verify [this hostname ssl-session] true)
(clojure/in-ns 'http-client)
;; end

(defn- create-proxy [proxy]
  (if (= proxy "")
    (. Proxy NO_PROXY)
    (let [u (new URL proxy)
          host (. u getHost)
          port (if (neg? (. u getPort)) 8080 (. u getPort))
          sa (new InetSocketAddress host port)
          prx (new Proxy (. Proxy$Type HTTP) sa)]
      prx)))

(defn url-do [url method defaults & body]
  "Connect to given url, execute given HTTP method. defauls may be empty {}, body is only
   used for POST requests"
  (binding [ http-defaults (merge http-defaults defaults) ]
    (let [p (create-proxy (http-defaults :http-proxy))
          u (new URL url)]
      (when (. (. url toLowerCase) startsWith "https")
        (let [sc (. SSLContext getInstance "SSLv3")
              tma (new httpclient.CljTrustManager)
              ar (make-array httpclient.CljTrustManager 1)]
          (aset ar 0 tma)
          (. sc (init nil ar nil))
          (. HttpsURLConnection setDefaultSSLSocketFactory (. sc getSocketFactory))
          (. HttpsURLConnection setDefaultHostnameVerifier (new httpclient.CljHostnameVerifier))))

      (let [ conn (. u openConnection p) ]
        (doto conn
          (setRequestMethod method)
          (setUseCaches (http-defaults :use-caches))
          (setReadTimeout (http-defaults :read-timeout))
          (setInstanceFollowRedirects (http-defaults :follow-redirects))
          (setConnectTimeout (http-defaults :connect-timeout)))
        (doseq kw (keys (http-defaults :set-header))
          (. conn (setRequestProperty kw (get (http-defaults :set-header) kw))))
        (when (= method "POST")
          (when (nil? body) (throw (new Exception "need body for post request")))
          (do
            (. conn setDoOutput true)
            (doto (new OutputStreamWriter (. conn getOutputStream ))
                  (write (nth body 0) 0 (count (nth body 0)))   (flush) (close))))
        (let [rcode (. conn getResponseCode)
              rmsg (. conn getResponseMessage)
              is (. conn getInputStream)
              buf-len 8192
              buf (make-array (. Byte TYPE) buf-len)
              #^ByteArrayOutputStream bos (new ByteArrayOutputStream)
              headers (into {} (. conn getHeaderFields))]
          (loop []
            (let [ nread (. is (read buf 0 buf-len)) ]
              (when (> nread -1)
                (. bos (write buf 0 nread))
                (recur))))
          (let [ bar (. bos toByteArray) ]
            (. is close)
            (. bos close)
            (. conn disconnect)
            {:return-code rcode :return-message rmsg :body bar :header headers}))))))

(comment
(pr (new String ((url-do "https://google.com" "GET" { :read-timeout 5000  }) :body) "UTF-8"))
(pr (url-do "http://google.com/" "GET" { :read-timeout 5000 :connect-timeout 1000 :http-proxy "http://127.0.0.1:3128/" }))
(pr (url-do "https://google.com/" "GET" { :read-timeout 5000 :connect-timeout 1000 :http-proxy "http://127.0.0.1:3128/" }))
)