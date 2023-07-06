(ns my_namespace.menu
  (:use[clojure.repl])
  (:require [compress])
  )

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(defn main-menu []

  (println "*** Compression Menu ***")
  (println "1. Display list of files")
  (println "2. Display file contents")
  (println "3. Compress a file")
  (println "4. Uncompress a file")
  (println "5. Exit")
  )

;;Option1 Display Files
(defn displayFiles[]
  (println "File List: ")
  (doseq [file (file-seq (java.io.File. "."))]
    (when (.isFile file)
      (println (.getName file))))
  )

;;Option2 Display File contents
(defn displayFileContents[filename]
  (println  "Exists!")
  (let [content (slurp filename)]
    (println content)) 
  )


;;helper to convert file to string
(defn convert-to-string[file]
  (slurp file)
)
;;helper to convert the string to lowercase
(defn convert-to-lowercase[file-content]
  (let[lower-case-content (str/lower-case file-content)]
    lower-case-content))

;;helper to covert wordFile to list
(defn convert-to-list [file]
  ;;(println "FIle: " file)
  (let [list (str/split file #"\s+")]
        list))

;;helper to remove redundant words from the frequency list
(defn remove-redundant[list]
  (distinct list)
)

;;helper to generate a new file after compression
(defn generate-file[list file]
  (let[content(str/join " " list)
    filename(str file ".ct")]
    (spit filename content)
  )
)


;;helper to convert list to string.
(defn final-result-to-string[list-file]
  (str/join " " list-file)
)


(defn main-Menu[] (
                (loop []
                  (main-menu)
                  (println "Enter an option") 
                  (let [userChoice (read-line)]
                    (cond
                      (= userChoice "1") (do (displayFiles) (recur))
                      (= userChoice "2") (do (println "Enter Filename") 
                                             (let [filename (read-line)]
                                                   (if (.exists (clojure.java.io/file filename))
                                                     (displayFileContents filename)
                                                     (println "file does not exits")))
                                                 (recur))
                      
                      (= userChoice "3") (
                                          do (println "Enter Filename") 
                                          (let [filename (read-line)] 
                                                (if (.exists (clojure.java.io/file filename))
                                                  ;;compression logic in this file only.
                                                       (let [matching-list (convert-to-list (convert-to-lowercase (convert-to-string "frequency.txt")))]
                                                          (let[current-list (convert-to-list (convert-to-string filename))]
                                                            (let [final-matching-list (remove-redundant matching-list)]
                                                              (let [compressed-list(compress/compressFile current-list final-matching-list)]
                                                                (generate-file compressed-list filename)
                                                              )    
                                                            )
                                                          )
                                                       )
                                                      (println "file does not exits")
                                                )
                                          ) 
                                          (recur)) 
                      
                      (= userChoice "4") (do (println "Enter Filename") 
                                            (let [filename (read-line)] 
                                                (if (.exists (clojure.java.io/file filename))
                                                  (let [matching-list (convert-to-list (convert-to-lowercase(convert-to-string "frequency.txt")))]
                                                          (let[current-list (convert-to-list (convert-to-string filename))]
                                                            (let [final-matching-list (remove-redundant matching-list)]
                                                              (let [final-result (compress/uncompressFile current-list final-matching-list 0)]
                                                                  (println (final-result-to-string final-result))    
                                                                ;;(compress-file compressed-list filename)
                                                              )    
                                                            )
                                                          )
                                                  )
                                                  (println "File does not exist")
                                                )
                                            )
                                          (recur))
                      (= userChoice "5") (println "Program Closed" nil)
                      :else (do (println "Enter a valid choice") (recur))
                    )))))
(main-Menu)  
;;(if(.exists (clojure.java.io/file (str (System/getProperty "user.dir") "/" filename)))
;;                                                (displayFileContents filename)
;;                                                (println (str (System/getProperty "user.dir") "/" filename))