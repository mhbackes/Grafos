;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname trabalhografos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
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

;-----------------------------------------EXEMPLOS DE DADOS------------------------------------------

; Listas de adjacência:
(define la11 (list 0 1))
(define la12 (list 1 0))
(define la13 (list 2))

(define la21 (list 0 1 1))
(define la22 (list 1 0 2))
(define la23 (list 2 1))

; Grafos:
(define g1 (list la11 la12 la13))
(define g2 (list la21 la22 la23))

;-----------------------------------------FUNÇÕES PRINCIPAIS-----------------------------------------

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
; Exemplos:
; (é-dígrafo? g1 3)
; deve retornar false
; (é-dígrafo? g2 3)
; deve retornar true

; é-conexo?: Grafo Número -> Booleano
; obj: Dados um grafo e seu tamanho, retorna true se o grafo for conexo, caso o
; contrário retorna false.
(define (é-conexo? grafo tamanho)
  (cond
    [(<= tamanho 1) true]
    [else (teste-conectividade grafo (map first (cons (first (reverse grafo)) grafo)))]))

; Exemplos:
; (é-conexo? g1 3)
; deve retornar false
; (é-conexo? g2 3)
; deve retornar true

; ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO
; DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PA
; RA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLO
; S DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUN
; ÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁ
; UDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE D
; ETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - ESPAÇO PARA A FUNÇÃO DE DETECÇÃO DE CICLOS DO CLÁUDIO - E

; acha-caminho: Nodo Nodo Grafo -> Lista-de-Caminhos
; obj: Dados um nodo de origem e outro de destino, retorna
; uma lista com todos os possíveis caminhos entre eles
(define (acha-caminho origem destino grafo tamanho)
  (cond
    [(= origem destino) (cond
                          [(é-dígrafo? grafo tamanho)
                           (map (lambda (x) (cons origem x))
                                (testa-caminho (vizinhos origem grafo) destino empty grafo))]
                          [else (append (caminho-loop origem (vizinhos origem grafo))
                                        (orig-orig-pseudo origem grafo))])]   
    [else (testa-caminho (list origem) destino empty grafo)]))

; distância: Nodo Nodo Grafo-> Número
; obj: Dados um nodo de origem e outro de destino,
; calcula sua distância
(define (distância origem destino grafo)
    (distância-larg (list origem) empty destino grafo))

;-----------------------------------------FUNÇÕES AUXILIARES-----------------------------------------

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

; vizinhos: Nodo Grafo -> Lista-de-números
; obj: Dados um grafo e um nodo retorna todos os nodos no grafo G
; que recebem arestas de n
(define (vizinhos n G)
  (cond
    [( empty? G) empty]
    [( equal? n (first (first G))) (rest (first G))]
    [else (vizinhos n (rest G))]))

; teste-conectividade: Grafo Lista-de-nodos -> Booleano
; obj: Dados um grafo e uma lista de nodos a serem testados, retorna true se
; todos os nodos nodos estiverem contectados entre si, caso o contrário,
; retorna false.
(define (teste-conectividade grafo ldn)
  (cond
    [(empty? (rest ldn)) true]
    [else (and (conectados-larg? (first ldn)(second ldn) grafo)
               (teste-conectividade grafo (rest ldn)))]))

; conectados-larg? : Nodo Nodo Grafo -> Booleano
; obj: Dados dois nodos e um grafo, retorna true se existir um caminho
; entre os dois nodos, caso o contrário retorna false.
(define (conectados-larg? a b G)
  (busca-larg (list a) (list ) b G))

; busca-larg : Lista-de-nodos Lista-de-nodos Nodo Grafo -> Booleano
; obj: Dados uma lista de nodos a percorrer (la), uma lista de nodos já percorridos (p)
; um destino (b) e um grafo (G), retorna true se houver um caminho entre entre la e b
; que não passe por p, caso o contrário retorna false.
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

; caminho-loop: Nodo Lista-de-nodos -> Lista-de-caminhos
; obj: Dados um nodo e sua lista de vizinhos, retorna a lista de caminhos
; com os loops desse nodo.
(define (caminho-loop origem vizinhos)
  (cond
    [(empty? vizinhos) empty]
    [(= origem (first vizinhos)) (cons (list origem origem) (caminho-loop origem (rest vizinhos)))]
    [else (caminho-loop origem (rest vizinhos))]))

; orig-orig-pseudo: Nodo Grafo -> Lista-de-caminhos
; obj: Dados um nodo e um pseudografo, retorna todos os caminhos que tem como
; origem e destino esse nodo, exceto loops.
(define (orig-orig-pseudo origem grafo)
  (map (lambda (x) (cons origem x))
       (foldr (lambda (x y)(append (testa-caminho (remove-todos origem (vizinhos x grafo)) origem (list x) grafo) y))
              empty 
              (remove-todos origem (vizinhos origem grafo)))))

; distância-larg: Lista-de-nodos Lista-de-nodos Nodo Grafo -> Número
; obj: Uma variação da função busca-larg que soma 1 a cada recursão.
; Se houver um caminho entre la e b sem passar por p, retorna a menor
; distância entre la e b, caso o contrário retorna infinito.
(define (distância-larg la p b G)
  (cond
    [(empty? la) +inf.0]
    [(member b la) 0]
    [else (+ 1 (distância-larg (todos-vizinhos la p G) (append la p) b G))]))
;----------------------------------------FUNÇÕES DE IMPRESSÃO----------------------------------------

; imprime-é-dígrafo: Grafo Número -> Void
; obj: Dados um grafo e seu tamanho, imprime
; se ele é dígrafo ou não.
(define (imprime-é-dígrafo? grafo tamanho)
  (cond
    [(é-dígrafo? grafo tamanho) (printf "O grafo é dígrafo.\n")]
    [else (printf "O grafo não é dígrafo.\n")]))

; imprime-é-dígrafo: Grafo Número -> Void
; obj: Dados um grafo e seu tamanho, imprime
; se ele é conexo ou não.
(define (imprime-é-conexo? grafo tamanho)
  (cond
   [(é-conexo? grafo tamanho) (printf "O grafo é conexo.\n")]
   [else (printf "O grafo não é conexo")]))

; Chamada das funções de impressão:
(imprime-é-dígrafo? g n)
(imprime-é-conexo? g n)    

















; remove-todos: Nodo Lista-de-nodos -> Lista-de-nodos
; obj: Dados um nodo e uma lista-de-nodos, retira da lista
; os nodos iguais ao nodo.
(define (remove-todos n lista)
  (cond
    [(member n lista) (remove-todos n (remove n lista))]
    [else lista]))








;;-----------------------------Coisas que o claudio tem que fazer--------------------

; tem-loop?: Grafo -> Booleano
; obj: Dado um grafo, retorna true se um de seus nodos
; tem um loop , caso o contrário, retorna false
(define (tem-loop? grafo)
  (cond
    [(empty? grafo) false]
    [else (or (member (first (first grafo))(rest (first grafo)))
              (tem-loop? (rest grafo)))]))

(define (dígrafo-tem-cíclo? grafo)
     (ormap (lambda (x) (busca-larg (rest x) empty (first x) grafo)) grafo))  ;(apagar esse comentario) ficou muito mais simples assim

(define (pseudo-ciclo la a grafo)
  (ormap (lambda (x) (busca-larg (remove a (todos-vizinhos (list x) empty grafo)) (list x) a grafo)) la))

(define (pseudografo-tem-cíclo? grafo)
    (cond
      [(tem-loop? grafo) true] 
      [else (ormap (lambda (x) (pseudo-ciclo (rest x) (first x) grafo)) grafo)]))


(define (tem-ciclo? grafo tamanho)         ; (apagar esse comentario) eu queria que essa função retornasse true ou false e nao void
  (cond
    [(é-dígrafo? grafo tamanho) (dígrafo-tem-cíclo? grafo)]
    [else (pseudografo-tem-cíclo? grafo)]))

(define (imprime-tem-ciclo? grafo tamanho)    ; (apagar esse comentario) colocar essa junto com as funções de impressão
    (cond
      [(tem-ciclo? grafo tamanho) (printf "O grafo é cíclico.\n")]
      [else (printf "O grafo é acíclico.\n")]))



(imprime-tem-ciclo? g n)
      
      

;;--------------------------------------------------------------------------   

