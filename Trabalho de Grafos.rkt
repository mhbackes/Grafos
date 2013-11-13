;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Trabalho de Grafos|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Nome do arquivo a ser lido
(define src "grafo.txt")
;; Lê o arquivo de entrada e armazena em r, 
;; separando a informação lida em dois valores
;;   n : número de nodos
;;   g : grafo
(define r (with-input-from-file src read))
(define n (first r))
(define g (rest r))

; um elemento nodo de Nodo é um número
; e identifica um nodo.

; um elemento lista-de-nodos de Lista-de-nodos é:
; - ou empty;
; - ou (cons nodo lista-de-nodos);
;   onde:
;   - nodo é um elemento de Nodo;
;   - lista-de-nodos é  um elemento de Lista-de-nodos.

; um elemento adjacência de Adjacência é:
; - ou empty;
; - ou (cons nodo (cons lista-de-nodos empty));
;   onde:
;   - nodo é um elemento de Nodo;
;   - lista-de-nodos é um elemento de Lista-de-nodos e
;     representa a lista de nodos adjacentes a nodo.

; um elemento grafo de Grafo é:
; - ou (cons adjacência empty);
; - ou (cons adjacência grafo);
;   onde:
;   - adjacência é um elemento de Adjacência e
;     representa um nodo e seus nodos adjacentes,
;     o primeiro nodo de adjacência só pode ocorrer
;     uma vez como primeiro nodo de uma adjacência
;     em um grafo;
;   - grafo é um elemento de Grafo.

; um elemento caminho de Caminho é:
; - ou empty;
; - ou (append (cons origem empty) lista-de-nodos (cons destino empty));
;   onde:
;   - origem é um elemento de Nodo e representa a
;   origem do caminho em um grafo;
;   - lista-de-nodos é um elemento de Lista-de-nodos e representa 
;   os passos intermediários da origem até o destino em um caminho em um grafo;
;   - destino é um elemento de Nodo e representa o
;   destino do caminho em um grafo;

; um elemento lista-de-caminhos de Lista-de-caminhos é:
; - ou empty;
; - ou (cons caminho lista-de-caminhos);
;   onde:
;   - caminho é um elemento de Caminho;
;   - lista-de-caminhos é um elemento de Lista-de-caminhos.
; conta-n: Lista-de-nodos Nodo -> Número
; obj: Dados uma lista-de-números (lista) e um número(n), retorna o numero de
; ocorrências do número na lista.
(define (conta-n lista n)
  (length (filter (lambda (x) (= x n)) lista)))

; aresta-direcionada?: Adjacência Adjacência -> Booleano
; obj: Dadas duas adjacências, retorna false se a aresta não é direcionada, caso o contrario,
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

; vizinhos: Nodo Grafo -> Lista-de-números
; obj: Dados um grafo e um nodo retorna todos os nodos no grafo G
; que recebem arestas de n
(define (vizinhos n G)
  (cond
    [( empty? G) empty]
    [( equal? n (first (first G))) (rest (first G))]
    [else (vizinhos n (rest G))]))


; conectados-larg? : Nodo Nodo Grafo -> Booleano
; obj: Dados dois nodos e um grafo, retorna true se existir um caminho
; entre os dois nodos, caso o contrário retorna false.
(define (conectados-larg? a b G)
  (busca-larg (list a) (list ) b G))


; busca-larg : Lista-de-nodos Lista-de-nodos Nodo Grafo -> Booleano
; obj: Dados uma lista de nodos a percorrer (la), uma lista de nodos já percorridos (p)
; um destino (b) e um grafo (G), retorna true se houver um caminho entre entre la e b,
; caso o contrário retorna false.
(define (busca-larg la p b G)
  (cond
    [(empty? la) false]
    [(member b la) true]
    [else (busca-larg (todos-vizinhos la p G) (append la p) b G)]))

; todos-vizinhos: Lista-de-nodos Lista-de-nodos Grafo -> Lista-de-Nodos
; obj: Dados uma lista de nodos (la) uma lista de nodos percorridos (p)
; e um grafo, retorna uma lista com todos os nodos adjacentes a la que não
; estão em p.
(define (todos-vizinhos la p G)
  (cond
    [( empty? la) empty]
    [else (append (filter (lambda (x) (not (member x p)))
                          (vizinhos (first la) G))
                  (todos-vizinhos (rest la) p G))]))
; é-conexo?: Grafo Número -> Booleano
; obj: Dados um grafo e seu tamanho, retorna true se o grafo for conexo, caso o
; contrário retorna false.
(define (é-conexo? grafo tamanho)
  (cond
    [(<= tamanho 1) true]
    [else (teste-conectividade grafo (map first (cons (first (reverse grafo)) grafo)))]))

; teste-conectividade: Grafo Lista-de-nodos -> Booleano
; obj: Dados um grafo e uma lista de nodos a serem testados, retorna true se
; todos os nodos nodos estiverem contectados entre si, caso o contrário,
; retorna false.
(define (teste-conectividade grafo ldn)
  (cond
    [(empty? (rest ldn)) true]
    [else (and (conectados-larg? (first ldn)(second ldn) grafo)
               (teste-conectividade grafo (rest ldn)))]))

; tem-loop?: Grafo -> Booleano
; obj: Dado um grafo, retorna true se um de seus nodos
; tem um loop , caso o contrário, retorna false
(define (tem-loop? grafo)
  (cond
    [(empty? grafo) false]
    [else (or (member (first (first grafo))(rest (first grafo)))
              (tem-loop? (rest grafo)))]))
; testa-caminho: Lista-de-Nodos Nodo Caminho Grafo -> Lista-de-Caminhos
; obj: Dados uma lista de nodos adjacentes (la), um destino (destino),
; um caminho (caminho) e um grafo (grafo), retorna uma lista-de-caminhos
; possíveis entre os nodos da lista nodos adjacentes e o destino no grafo
; levando em conta o caminho já percorrido até chegar à lista de nodos 
; adjacentes.
(define (testa-caminho la destino caminho grafo)
  (cond
    [(empty? la) empty]
    [(= (first la) destino) (cons (append caminho (list destino)) (testa-caminho (rest la) destino caminho grafo))]
    [(member (first la) caminho) (testa-caminho (rest la) destino caminho grafo)]
    [else (append (testa-caminho (vizinhos (first la) grafo) destino (append caminho (list (first la))) grafo)
                  (testa-caminho (rest la) destino caminho grafo))]))

; acha-caminho: Nodo Nodo Grafo -> Lista-de-Caminhos
; obj: Dados um nodo de origem e outro de destino, retorna
; uma lista com todos os possíveis caminhos entre eles
(define (acha-caminho origem destino grafo tamanho)
  (cond
    [(= origem destino) (cond
                          [(é-dígrafo? grafo tamanho)(map (lambda (x) (cons origem x))(testa-caminho (vizinhos origem grafo) destino empty grafo))]
                          [else (append (caminho-loop origem (vizinhos origem grafo))
                                        (orig-orig-pseudo origem grafo))])]   
    [else (testa-caminho (list origem) destino empty grafo)]))

; remove-todos: Nodo Lista-de-nodos ->Lista-de-nodos
; obj: Dados um nodo e uma lista-de-nodos, retira da lista
; os nodos iguais ao nodo.
(define (remove-todos n lista)
  (cond
    [(member n lista) (remove-todos n (remove n lista))]
    [else lista]))

(define (caminho-loop origem vizinhos)
  (cond
    [(empty? vizinhos) empty]
    [(= origem (first vizinhos)) (cons (list origem origem) (caminho-loop origem (rest vizinhos)))]
    [else (caminho-loop origem (rest vizinhos))]))

(define (orig-orig-pseudo origem grafo)
  (map (lambda (x) (cons origem x))
       (foldr (lambda (x y)(append (testa-caminho (remove-todos origem (vizinhos x grafo)) origem (list x) grafo) y))
              empty 
              (remove-todos origem (vizinhos origem grafo)))))

; distância: Nodo Nodo -> Número
; obj: Dados um nodo de origem e outro de destino,
; calcula sua distância
(define (distância origem destino grafo)
    (distância-larg (list origem) empty destino grafo))

(define (distância-larg la p b G)
  (cond
    [(empty? la) +inf.0]
    [(member b la) 0]
    [else (+ 1 (distância-larg (todos-vizinhos la p G) (append la p) b G))]))

(require 2htdp/batch-io)
(require racket/format)
(define (k-grafo range1 range2)
  (cond
    [(empty? range1) empty]
    [else (cons (cons (first range1) (filter (lambda (x) (not(= x (first range1)))) range2))(k-grafo (rest range1) range2))]))
(define x 20)
(write-file "grafos.txt" (~a (cons x (k-grafo (range 0 x 1) (range 0 x 1)))))

