(ns metadata.metadata
  (:use  [clojure.java.io :only (as-file)])
  (:import 
    (com.drew.metadata Metadata)
    (com.drew.imaging.jpeg JpegMetadataReader)))

(defn parseDate [inp]
    (.parse (java.text.SimpleDateFormat. "yyyy:MM:dd HH:mm:ss") inp))

(defn set-metadata-time [destFile]
   (let [metaReader (JpegMetadataReader/readMetadata  destFile)
      directories (iterator-seq (.getDirectoryIterator metaReader))]
	  (doseq [directory directories]
	    (doseq [tag (iterator-seq (.getTagIterator directory))]
	      (if (=  (.getTagName tag) "Date/Time Original")
          (.setLastModified destFile (.getTime (parseDate (.getDescription tag)))))))))
	        
(let [destDir (as-file (nth *command-line-args* 0))]
  (doseq [f (.listFiles destDir)]
    (if (.endsWith (.toUpperCase (.getName f)) ".JPG")
      (set-metadata-time f))))
