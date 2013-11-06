;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Trabalho de Grafos|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; IMPORTANTE: para ter acesso a leitura de arquivos
;; é necessário configurar o DrRacket para a linguagem
;; "Advanced Student"

;; Nome do arquivo a ser lido
(define src "grafo.txt")
;; Lê o arquivo de entrada e armazena em r, 
;; separando a informação lida em dois valores
;;   n : número de nodos
;;   g : grafo
(define r (with-input-from-file src read))
(define n (first r))
(define g (rest r))

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

; conta-n: Lista-de-números Número -> Número
; obj: Dados uma lista-de-números (lista) e um número(n), retorna o numero de
; ocorrências do número na lista.
(define (conta-n lista n)
  (length (filter (lambda (x) (= x n)) lista)))

; aresta-direcionada?: Lista-de-números Lista-de-números -> Booleano
; obj: Dadas duas lista, retorna false se a aresta não é direcionada, caso o contrario,
; retorna true.
(define (aresta-direcionada? lista1 lista2)
  (cond
    [(= (conta-n (rest lista1)(first lista2)) (conta-n (rest lista2)(first lista1))) false]
    [else true]))
    
; é-dígrafo?: Grafo Número -> Booleano
; obj: Dados um grafo e seu tamanho, retorna true se o grafo é um dígrafo, caso o contrário,
; retorna false.
(define (é-dígrafo? grafo tamanho)
  (cond
    [(< tamanho 2) false]
    [(= tamanho 2)
     (aresta-direcionada? (first grafo) (second grafo))]
    [else (or (ormap (lambda (x) (aresta-direcionada? (first grafo) x)) (rest grafo))
              (é-dígrafo? (rest grafo) (- tamanho 1)))]))

(define (imprime-é-dígrafo? grafo tamanho)
    (cond
      [(é-dígrafo? grafo tamanho) (printf "O grafo é dígrafo.\n")]
      [else (printf "O grafo não é dígrafo.\n")]))
      
(imprime-é-dígrafo? g n)
; vizinhos : Número, Grafo -> Lista-de-números
; (vizinhos n G) retorna todos os nodos no grafo G
; que recebem arestas de n
(define (vizinhos n G)
  (cond
    ; erro, não achou nodo
    [( empty? G) empty]
    ; achou nodo, retorna nodos adjacentes
    [( equal? n (first (first G))) (rest (first G))]
    ; não é o nodo atual, continua busca no resto da lista
    [else (vizinhos n (rest G))]))


; conectados-larg? : Número Número Grafo -> Booleano
(define (conectados-larg? a b G)
  (busca-larg (list a) (list ) b G))

; busca-larg : Lista-de-números,  Lista-de-números, Número, Grafo -> Booleano
(define (busca-larg la p b G)
  (cond
    ; lista vazia?
    [( empty? la) false]
    ; destino pertence à lista de nodos atual?
    [( member b la) true]
    ; repete busca nos vizinhos dos vizinhos
    [else (busca-larg (todos-vizinhos la p G) (append la p) b G)]))

; todos-vizinhos: Lista-de-números, Lista-de-números, Grafo -> Lista-de-Números
(define (todos-vizinhos la p G)
  (cond
    [( empty? la) empty]
    [else (append (filter (lambda (x) (not (member x p)))
                          (vizinhos (first la) G))
                  (todos-vizinhos (rest la) p G))]))
                  
(define (é-conexo? grafo tamanho)
  (cond
    [(<= tamanho 1) true]
    [else (teste-conectividade grafo (map first (cons (first (reverse grafo)) grafo)))]))
    
(define (teste-conectividade grafo ldn)
  (cond
    [(empty? (rest ldn)) true]
    [else (and (conectados-larg? (first ldn)(second ldn) grafo)
               (teste-conectividade grafo (rest ldn)))]))

(é-conexo? g n)

(require 2htdp/batch-io)
(require racket/format)
(define (k-grafo range1 range2)
  (cond
    [(empty? range1) empty]
    [else (cons (cons (first range1) (filter (lambda (x) (not(= x (first range1)))) range2))(k-grafo (rest range1) range2))]))
;(write-file "grafo.txt" (~a (cons 1000 (k-grafo (range 0 3000 1) (range 0 3000 1)))))