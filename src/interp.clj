(ns interp
  (:require [grammars :as wae]   ;; Importa el namespace 'grammars' como 'wae' para usar las estructuras definidas ahí
            [parser :as psr]))   ;; Importa el namespace 'parser' para usar las funciones de parseo

;; Interpreta expresiones aritméticas AE
(defn interp-AE
  "Interpreta expresiones aritméticas AE"
  [exp]
  (cond
    ;; Si es un número (NumG)
    (instance? wae.NumG exp)
    (:n exp)

    ;; Si es una suma (AddG)
    (instance? wae.AddG exp)
    (+ (interp-AE (:izq exp)) (interp-AE (:der exp)))

    ;; Si es una resta (SubG)
    (instance? wae.SubG exp)
    (- (interp-AE (:izq exp)) (interp-AE (:der exp)))

    ;; Cualquier otra cosa
    :else
    (throw (IllegalArgumentException. "Expresión AE no válida para interpretación"))))

;; Interpreta expresiones WAE
(defn interp-WAE
  "Interpreta expresiones WAE "
  [exp env]
  (cond
    ;; Si es un número (NumG)
    (instance? wae.NumG exp)
    (:n exp)

    ;; Si es un identificador (IdG)
    (instance? wae.IdG exp)
    (if (contains? env (:i exp))
      (env (:i exp))
      (throw (IllegalArgumentException. (str "Identificador no encontrado: " (:i exp)))))

    ;; Si es una suma (AddG)
    (instance? wae.AddG exp)
    (+ (interp-WAE (:izq exp) env) (interp-WAE (:der exp) env))

    ;; Si es una resta (SubG)
    (instance? wae.SubG exp)
    (- (interp-WAE (:izq exp) env) (interp-WAE (:der exp) env))

    ;; Si es una asignación (WithG)
    (instance? wae.WithG exp)
    (let [binding (:assign exp)
          var (:id binding)
          val-exp (:value binding)
          body (:body exp)
          val (interp-WAE val-exp env)]
      (interp-WAE body (assoc env var val)))

    ;; Cualquier otra cosa
    :else
    (throw (IllegalArgumentException. "Expresión WAE no válida para interpretación"))))

;; Función principal para interpretar AE o WAE
(defn interp
  "Interpreta una expresión AE o WAE. Detecta si es AE o WAE dependiendo del tipo de expresión"
  [exp]
  (cond
    ;; Si la expresión es AE
    (or (instance? wae.NumG exp)
        (instance? wae.AddG exp)
        (instance? wae.SubG exp))
    (interp-AE exp)

    ;; Si la expresión es WAE
    (or (instance? wae.IdG exp)
        (instance? wae.WithG exp))
    (interp-WAE exp {})

    ;; Cualquier otra cosa
    :else
    (throw (IllegalArgumentException. "Expresión no válida"))))
