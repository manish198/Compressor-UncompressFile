(ns compress
(:use[clojure.repl]))
(require '[clojure.string :as str])
(def non-alpha-numeric-regex  #"\W")
(def numeric-regex #"^-?\d+(\.\d+)?$")
(def alpha-regex #"\A[A-Za-z]")

(defn matching-function [current-string list index]
  (loop [lst list idx index]
    (cond
      (empty? lst) current-string
      (= (str/lower-case current-string) (first lst)) idx
      
      ;;checking for the numeric expression
      (let[result (re-find numeric-regex current-string)]result) (str "@" current-string "@")  
    
      ;;check for brackets and punctuations at last.
      (let [result (re-find non-alpha-numeric-regex
                         (subs current-string (-(count current-string) 1)))]
          result
      )
          ;;nested check weather first letter is apphabet or not.
          (if (<= (count current-string) 1)
              current-string
              (if (re-find alpha-regex
                  (subs current-string 0 1)
                  ) 
                  ;;only punctuation or bracket at the last
                (str (matching-function (subs current-string 0 (dec (count  current-string))) list 0) 
                " " 
                (subs current-string (dec (count current-string)))
                )
                ;;also non alpha-regex in the front
                (str (subs current-string 0 1) 
                " " 
                (matching-function (subs current-string 1 (dec (count  current-string))) list 0) 
                " " 
                (subs current-string (dec (count current-string)))
                )
              )
            )
          
      (let [result (re-find non-alpha-numeric-regex
            (subs  current-string  0 1))]
            result
      )
      (if (<= (count current-string) 1)
              current-string
               (
                str (subs current-string 0 1) 
                  " " 
                  (matching-function (subs current-string 1) list 0)
               )
      )
            
      
      
      :else (recur(rest lst) (inc idx))))
  )


(defn matching-function-uncompress [current-string list index]
  (loop [lst list idx index]
    (cond
      (empty? lst) current-string
      (= current-string (str idx)) (first lst)
      (let [result (re-find non-alpha-numeric-regex
            (subs current-string (-(count current-string) 1)))]
          result
      )
      (if (<= (count current-string) 1)
            current-string
            (if (re-find non-alpha-numeric-regex (subs current-string 0 1))
                (subs current-string 1 (dec (count  current-string))) 
                (subs current-string 0 (dec (count current-string)))
            )
      )
      :else (recur(rest lst) (inc idx))
    )
  )
)

;;compression logic here. option 3.
(defn compressFile [current-file-list list]
  (let [converted-list (map #(matching-function % list 0) current-file-list)]
    converted-list
    )
)

(defn uncompressFile[current-file-list list index]
   (let [converted-list (map #(matching-function-uncompress % list 0) current-file-list)]
    converted-list)
)
