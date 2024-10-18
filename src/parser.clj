(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Parser de expresiones aritméticas"
  [exp]
  (cond
    ;; Si es un número
    (number? exp) 
    (wae/numG exp)
    
    ;; Si es una suma
    (and (list? exp) (= '+ (first exp)))
    (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
    
    ;; Si es una resta
    (and (list? exp) (= '- (first exp)))
    (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))

    ;; Cualquier otra cosa
    :else 
    (throw (IllegalArgumentException. "Expresión AE no válida"))))


(defn parser-WAE
  "Parser de expresiones WAE"
  [exp]
  (cond
    ;; Si es un número
    (number? exp) 
    (wae/numG exp)
    
    ;; Si es un identificador
    (symbol? exp)
    (wae/idG exp)
    
    ;; Si es una expresión `with`
    (and (list? exp) (= 'with (first exp)))
    (let [bindings-list (second exp)
          var (first bindings-list)
          val (second bindings-list)
          body (nth exp 2)]
      (wae/withG (wae/bindings var (parser-WAE val)) (parser-WAE body)))
    
    ;; Si es una suma
    (and (list? exp) (= '+ (first exp)))
    (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
    
    ;; Si es una resta
    (and (list? exp) (= '- (first exp)))
    (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))

    ;;  Cualquier otra cosa 
    :else
    (throw (IllegalArgumentException. "Expresión WAE no válida"))))
