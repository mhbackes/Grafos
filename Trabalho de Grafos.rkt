;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname leitura-de-grafos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; IMPORTANTE: para ter acesso a leitura de arquivos
;; é necessário configurar o DrRacket para a linguagem
;; "Advanced Student"

;; Nome do arquivo a ser lido
(define src "grafo.txt")

;; Lê o arquivo de entrada e armazena em r, 
;; separando a informação lida em dois valores
;;   n : número de nodos
;;   g : grafo
;(define r (with-input-from-file src read))
;(define n (first r))
;(define g (rest r))

;; DEFINIÇÃO DE DADOS PARA O GRAFO

; Uma adjacência é uma lista
;   (list s1 s2 ... sn)
; onde
;   s1 : símbolo 
;        representa um nodo no grafo
;   s2 ... sn : símbolo 
;        representam os nodos adjacentes a s1
;
; Um grafo é uma lista de adjacências
;   (list a1 a2 ... an)
; onde todo símbolo referenciado nas adjacências a1 ... an
; ocorre como primeiro elemento em uma única adjacência ak

(define (conta-n lista n)
  (cond
    [(empty? lista) 0]
    [(= n (first lista))
     (+ 1 (conta-n (rest lista) n))]
    [else (conta-n (rest lista) n)]))


(define (aresta-direcionada? lista1 lista2)
  (cond
    [(= (conta-n (rest lista1) (first lista2)) (conta-n (rest lista1) (first lista2))) false]
    [else true]))

(define (compara-2 lista1 lista2 grafo1 grafo2)
  (cond
    [(= 2 (length grafo1)) (aresta-direcionada? (first grafo1) (first grafo2))]
    [(empty? grafo2) 
     (compara-2 (first (rest grafo1)) (second (rest grafo1)) (rest grafo1) (rest (rest grafo1)))]
    [else (or 
           (aresta-direcionada? lista1 lista2) 
           (compara-2 lista1 (first grafo2) grafo1 (rest grafo2)))]))

(define (é-dígrafo? grafo)
  (cond
    [(empty? grafo) false]
    [(empty? (rest grafo)) false]
    [else (compara-2 (first grafo) (second grafo) (rest grafo) (rest(rest grafo)))]))
(compara-2 (list 0 1) (list 1 0 3 4) (list (list 1 0 3 4) (list 2)(list 3 1 4)(list 4 1 3 5)(list 5 4)) (list (list 2)(list 3 1 4)(list 4 1 3 5)(list 5 4)))