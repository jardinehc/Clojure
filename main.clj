 ;; Programming Assignment #1
	;; Adds two numbers via a cmd line function call
	;; Authors: Matthew Schofield, Jardine Chapman, Muntaha Tamanna 
	;; Version: 9/19/2019
	 (defn add [x y]
	   (+ x y)
	 )
   
   
   
   - ;; Programming Assignment #2
 ;; Summation of a list of numbers via a cmd line function call
 ;; Authors: Matthew Schofield, Jardine Chapman, Muntaha Tamanna 
 ;; Version: 10/10/2019

 ;; Summation of a list of numbers
 (defn sumList [L]
   (cond 
    ;; if list is empty return 0
     (= (count L) 0) 0 
     ;; Else add the first element and recursively call this function
     ;; With the rest of the list
     :else (+ (first L) (sumList (rest L)))
   )
 )
 )
 
 ;; Homework Assignment #3
 ;; Alternating Harmoic Sum 
 ;; Authors: Matthew Schofield, Jardine Chapman, Muntaha Tamanna 
 ;; Version: 10/26/2019

 ;; Recursive funciton to calculate the sum 
 ;; of an alternating harmonic sequence
 ;; 
 ;; curIteration - is either 1 and causes a base case return
 ;;  or is the current value to divide 1 by, then re-call function
 ;;  with curIteration-1 
 (defn harmonic [curIteration] 
   (cond
    ;; Base case return 1
    (= curIteration 1) 1 
    ;; curIteration is even, re-cursively call harmonic with curIteration-1
    (= (mod curIteration 2) 0) (- (harmonic (- curIteration 1)) (/ 1 curIteration))
    ;; curIteration is odd, re-cursively call harmonic with curIteration-1
    (= (mod curIteration 2) 1) (+ (harmonic (- curIteration 1)) (/ 1 curIteration))
   )
 )
	-
 ;; Main function
 ;; Checks that the input is valid (>0) 
 ;; and does the final float conversion
 ;;
 ;; start - value to start alternating harmonic sequence
 (defn main [start]
   (cond
     (> start 0) (float (harmonic start)) 
     :else -1
   )
 )
 
 
 
 
 ;; Homework Assignment #4
(def x 5)
 ;; Text file format conversion
 ;; Authors: Matthew Schofield, Jardine Chapman, Muntaha Tamanna 
 ;; Version: 11/14/2019
 (require '[clojure.string :as str])

 ;; Starting point of the program,
 ;; Given a file with 1 or more lines with records of the format:
 ;; Last_name:First_name:Number_of_cats:Number_of_dogs:Number_of_fish:Number_of_other_pets
 ;; Create a file with the records converted to the format:
 ;; First_name Last_name:Total_number_of_pets
 ;; 
 ;; Ex:
 ;; Chawla:Kalpana:0:0:3:0
;; Davis:Jo Ann:0:0:5:2
 ;; de Vries:Gustav:3:1:0:1
 ;; Goldberg:Adele:2:1:1:0
 ;; Noriega:Carlos:2:2:0:0
 ;; Ride:Sally:0:1:0:2
 ;; Turing:Alan:0:0:0:0
 ;; =>
 ;; Kalpana Chawla:3
 ;; Jo Ann Davis:7
 ;; Gustav de Vries:5
 ;; Adele Goldberg:4
 ;; Carlos Noriega:4
 ;; Sally Ride:3
 ;; Alan Turing:0
 (defn main [file output]
   (spit output (matrixToFinalString (delimFileToMatrix file #":")))
 )

 ;; Splits a string on a given regex into a vector
 ;; Regexs in clojure are denoted with a #
 ;; Ex: (split "a b c" #" ") => ["a" "b" "c"]
 (defn split [stringToSplit splitChar]
   (str/split stringToSplit splitChar)
 )

 ;; Joins a vector into string with the given character
 ;; Ex: (join " " ["a" "b" "c"]) => "a b c"
 ;; Note: the position of the arguments is switched compared
 ;; to regular clojure, this is for consistency with the split 
 ;; function
 (defn join [stringToJoin joinChar]
   (str/join joinChar stringToJoin)
 )

 ;; Sums a given vector of strings to an integer
 ;; Ex: ["1" "2" "3"] => 6
 (defn strVecToInt [vec]
   (reduce + 
     (map 
       #(Integer/parseInt %) vec
     )
   )
 )

 ;; Reads in a delimited file and outputs it as a 2D vector
 ;; split on newlines and the given delimiter in the form
 ;; [
 ;;   [Line1 Record1, Line1 Record2,... ,Line1 RecordN]  
 ;;   .
 ;;   .
 ;;   .
 ;;   [LineM Record1, ...., LineM RecordN]
 ;; ]
 (defn delimFileToMatrix [file delim]
   (for [line (split (slurp file) #"\n")]
     (split line delim)
   )  
 )

 ;; Converts the given matrix into the desired output string
 ;; which can then be written to a file to complete the task
 ;; Ex:
 ;;  [
 ;;    ["Chawla" "Kalpana" "0" "0" "3" "0"] 
 ;;    ["Davis" "Jo Ann" "0" "0" "5" "2"]
 ;;    ["de Vries" "Gustav" "3" "1" "0" "1"]
 ;;    ["Goldberg" "Adele" "2" "1" "1" "0"] 
 ;;    ["Noriega" "Carlos" "2" "2" "0" "0"] 
 ;;    ["Ride" "Sally" "0" "1" "0" "2"] 
 ;;    ["Turing" "Alan" "0" "0" "0" "0"]
 ;;  ]
 ;; =>
 ;; "Kalpana Chawla:3
 ;; Jo Ann Davis:7
 ;; Gustav de Vries:5
 ;; Adele Goldberg:4
 ;; Carlos Noriega:4
 ;; Sally Ride:3
 ;; Alan Turing:0"
 (defn matrixToFinalString [fileMatrix]
   (join 
     (for [line fileMatrix]
       (join
         [
           (join [(nth line 1) (nth line 0)] " ")
           (strVecToInt (subvec line 2 6))
         ]
         ":"
       )         
     )
     "\n"
   )
 )
