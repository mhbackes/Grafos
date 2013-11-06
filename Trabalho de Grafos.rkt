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
    [(= (conta-n (rest lista1) (first lista2)) (conta-n (rest lista2) (first lista1))) false]
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
              (é-dígrafo? (rest grafo)))]))

(define (imprime-é-dígrafo? grafo tamanho)
    (cond
      [(é-dígrafo? grafo tamanho) (printf "O grafo é dígrafo.")]
      [else (printf "O grafo não é dígrafo.")]))
      
(imprime-é-dígrafo? g n)