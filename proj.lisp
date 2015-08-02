;;;;MOEDAS E FIOS
;;; Ivan Freire
;;; Marcelo Dias
;;; David Fernandes

(defstruct pos linha coluna)
(defstruct fiozinho id origem destino)
(defstruct tabuleirozinho nlinhas ncolunas fios moedas idfio)
(defstruct moedinha valor posicao)
(defstruct joguinho estado pontosjogador1 pontosjogador2 proxjogador fiosremovidos)
(defstruct problema estado-inicial jogador accoes resultado teste-corte-p funcao-avaliacao historico-accoes chave-equivalencia)

;;;POSICAO
(defun cria-posicao(linha coluna)
	(make-pos :linha linha
			  :coluna coluna))
	

	
(defun posicao-linha(pos)
	(pos-linha pos))
	
	
(defun posicao-coluna(pos)
	(pos-coluna pos)) 

(defun posicao-p(x)
	(pos-p x))
	
(defun posicoes-iguais-p(p1 p2)
	(and (equal (posicao-linha p1) (posicao-linha p2))
		 (equal (posicao-coluna p1) (posicao-coluna p2))))


;;;FIOS		 
(defun cria-fio(id origem destino)
	(make-fiozinho :id id
				   :origem origem
				   :destino destino))
	
	
(defun fio-id(fio)
	(fiozinho-id fio))
	
(defun fio-origem(fio)
	(fiozinho-origem fio))
	
(defun fio-destino(fio)
	(fiozinho-destino fio))
	

(defun fio-p(x)
	(fiozinho-p x))
	
;;;TABULEIRO
(defun cria-tabuleiro(nlinhas ncolunas)
	(make-tabuleirozinho :nlinhas nlinhas
						 :ncolunas ncolunas
						 :fios nil
						 :moedas nil
						 :idfio 0))
	
(defun copia-tabuleiro(tabuleiro)
	(make-tabuleirozinho :nlinhas (tabuleirozinho-nlinhas tabuleiro)
						 :ncolunas (tabuleirozinho-ncolunas tabuleiro)
						 :fios (tabuleirozinho-fios tabuleiro)
						 :moedas (tabuleirozinho-moedas tabuleiro)
						 :idfio (tabuleirozinho-idfio tabuleiro)))
	
(defun tabuleiro-linhas(tabuleiro)
	(tabuleirozinho-nlinhas tabuleiro))
	
(defun tabuleiro-colunas(tabuleiro)
	(tabuleirozinho-ncolunas tabuleiro))
	
(defun tabuleiro-fios(tabuleiro)
	(tabuleirozinho-fios tabuleiro))
	
(defun tabuleiro-fio-com-id(tabuleiro id)
	(dolist (el (tabuleirozinho-fios tabuleiro) nil)
			(when (equal (fiozinho-id el) id)
				(return el))))

(defun tabuleiro-fios-posicao(tabuleiro posicao)
	(let((lista nil))
	(dolist (el (tabuleirozinho-fios tabuleiro) nil)
			(cond((or (posicoes-iguais-p(fiozinho-origem el) posicao)(posicoes-iguais-p(fiozinho-destino el) posicao)) (setf lista (cons el lista)))))			
	lista))
	
(defun tabuleiro-moeda-posicao(tabuleiro posicao)
	(dolist (el (tabuleirozinho-moedas tabuleiro) nil)
			(when (posicoes-iguais-p (moedinha-posicao el) posicao)
				(return (moedinha-valor el)))))
			
	
(defun tabuleiro-total-moedas(tabuleiro)
	(let((valor 0))
	(progn
		(dolist (el (tabuleirozinho-moedas tabuleiro) nil)
			(if (not(null el))(setf valor (+ valor (moedinha-valor el)))))				  		
		valor)))
	
(defun tabuleiro-adiciona-fio!(tabuleiro origem destino)
	(progn
	(setf (tabuleirozinho-idfio tabuleiro) (+ 1 (tabuleirozinho-idfio tabuleiro)))
	(setf (tabuleirozinho-fios tabuleiro) (cons (cria-fio (tabuleirozinho-idfio tabuleiro) origem destino) (tabuleirozinho-fios tabuleiro)))))
	
(defun tabuleiro-adiciona-moeda-posicao!(tabuleiro posicao valor)
	(cond ((null (tabuleiro-moeda-posicao tabuleiro posicao))
			(setf (tabuleirozinho-moedas tabuleiro) (cons (make-moedinha :posicao posicao :valor valor)
														(tabuleirozinho-moedas tabuleiro))))
	(t
		(progn
		(setf (moedinha-valor (devolve-moeda posicao (tabuleirozinho-moedas tabuleiro))) valor)
		))))
																												 
(defun tabuleiro-remove-fio-com-id!(tabuleiro id)
	(setf (tabuleirozinho-fios tabuleiro)(retira-fio id (tabuleirozinho-fios tabuleiro))))
		

(defun retira-fio(id fios)
	(cond ((null fios) nil)
		  ((equal id (fiozinho-id (first fios))) (retira-fio id (rest fios)))
		  (t (cons (first fios) (retira-fio id (rest fios))))))

	
(defun tabuleiro-remove-moeda-posicao!(tabuleiro posicao)
	(setf (tabuleirozinho-moedas tabuleiro)(retira-moeda posicao (tabuleirozinho-moedas tabuleiro))))

(defun devolve-moeda(posicao moedas)
	(cond ((null moedas) nil)
		  ((posicoes-iguais-p posicao(moedinha-posicao (first moedas))) (first moedas))
		  (t (devolve-moeda posicao (rest moedas)))))	
(defun retira-moeda(posicao moedas)
	(cond ((null moedas) nil)
		  ((posicoes-iguais-p posicao(moedinha-posicao (first moedas))) (retira-moeda posicao (rest moedas)))
		  (t (cons (first moedas) (retira-moeda posicao (rest moedas))))))
	
	
(defun cria-jogo(tabuleiro)
	(make-joguinho :estado (copia-tabuleiro tabuleiro)
				   :pontosjogador1 0
				   :pontosjogador2 0
				   :proxjogador 1
				   :fiosremovidos nil))
				   
(defun copia-jogo(jogo)
	(make-joguinho :estado (copia-tabuleiro (joguinho-estado jogo))
				   :pontosjogador1 (joguinho-pontosjogador1 jogo)
				   :pontosjogador2 (joguinho-pontosjogador2 jogo)
				   :proxjogador (joguinho-proxjogador jogo)
				   :fiosremovidos (joguinho-fiosremovidos jogo)))
	
(defun jogo-tabuleiro(jogo)
	(joguinho-estado jogo))
	
(defun jogo-jogador(jogo)
	(joguinho-proxjogador jogo))
	
(defun jogo-pontos-jogador1(jogo)
	(joguinho-pontosjogador1 jogo))
	
(defun jogo-pontos-jogador2(jogo)
	(joguinho-pontosjogador2 jogo))
	
(defun jogo-historico-jogadas(jogo)
	(reverse(joguinho-fiosremovidos jogo)))
	
	
;;;APLICA JOGADA
(defun jogo-aplica-jogada!(jogo id)	

	(let ((listaorigem nil)
		  (listadestino nil))

	
	(if(not (null (tabuleiro-fio-com-id (joguinho-estado jogo) id)))
		(progn
		(setf listaorigem (tabuleiro-fios-posicao (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id))))
		(setf listadestino (tabuleiro-fios-posicao (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id))))
		(cond((and (= (length listaorigem) 1) (= (length listadestino) 1)) (cond ((= (jogo-jogador jogo) 1) (setf (joguinho-pontosjogador1 jogo) (+ (joguinho-pontosjogador1 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id)))))
																											(setf (joguinho-pontosjogador1 jogo) (+ (joguinho-pontosjogador1 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id))))))
																				 ((= (jogo-jogador jogo) 2) (setf (joguinho-pontosjogador2 jogo) (+ (joguinho-pontosjogador2 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id)))))
																											(setf (joguinho-pontosjogador2 jogo) (+ (joguinho-pontosjogador2 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id)))))))																		
																		   (tabuleiro-remove-moeda-posicao! (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id)))
																		   (tabuleiro-remove-moeda-posicao! (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id))))
			 ((= (length listaorigem) 1) (cond ((= (jogo-jogador jogo) 1) (setf (joguinho-pontosjogador1 jogo) (+ (joguinho-pontosjogador1 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id))))))
											   ((= (jogo-jogador jogo) 2) (setf (joguinho-pontosjogador2 jogo) (+ (joguinho-pontosjogador2 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id)))))))
										 (tabuleiro-remove-moeda-posicao! (joguinho-estado jogo) (fiozinho-origem (tabuleiro-fio-com-id (joguinho-estado jogo) id))))	
			 ((= (length listadestino) 1) (cond ((= (jogo-jogador jogo) 1) (setf (joguinho-pontosjogador1 jogo) (+ (joguinho-pontosjogador1 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id))))))
												((= (jogo-jogador jogo) 2) (setf (joguinho-pontosjogador2 jogo) (+ (joguinho-pontosjogador2 jogo)(tabuleiro-moeda-posicao (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id)))))))
										  (tabuleiro-remove-moeda-posicao! (joguinho-estado jogo) (fiozinho-destino (tabuleiro-fio-com-id (joguinho-estado jogo) id))))
			 
			 (t (cond ((= (jogo-jogador jogo) 1) (setf (joguinho-proxjogador jogo) 2)) 
					  ((= (jogo-jogador jogo) 2) (setf (joguinho-proxjogador jogo) 1)))))
		
		(setf (joguinho-fiosremovidos jogo) (cons id (joguinho-fiosremovidos jogo)))
		(tabuleiro-remove-fio-com-id! (joguinho-estado jogo) id)))))
	
(defun jogo-terminado-p(jogo)
	(cond((null(tabuleirozinho-fios(joguinho-estado jogo)))t)
		(t nil)))
		
		
;;;Problema

(defun jogador(estado)
	(joguinho-proxjogador estado))

(defun accoes(jogo)
	(let((lista nil))
	(dolist (el (tabuleirozinho-fios (joguinho-estado jogo)) nil)
			(setf lista (cons (fio-id el) lista )))			
	(reverse lista)))
	
(defun resultado(jogo id)
	(let((jogo-copiado (copia-jogo jogo)))
	(jogo-aplica-jogada! jogo-copiado id)
	jogo-copiado))
	
(defun teste-terminal-p(jogo profundidade)
	(declare (ignore profundidade))
	(jogo-terminado-p jogo)
)

(defun utilidade(jogo id)
	(cond ((= id 1) (- (joguinho-pontosjogador1 jogo) (joguinho-pontosjogador2 jogo)))
		  (t (- (joguinho-pontosjogador2 jogo) (joguinho-pontosjogador1 jogo)))))

	
	
;;;MINIMAX SIMPLES
(defun minimax(problema jogador)
    
		(let*((estadoinicial (problema-estado-inicial problema))
			  (jogadorinicial (funcall (problema-jogador problema) estadoinicial))
			  (melhoraccao 0)
			  (nos 0)
			  (alfa most-negative-long-float)
			  (beta most-positive-long-float)
			  (valoradversario 0)
			 )

        (defun maxvalor(estadoactual alfa beta)
			(let ((valor most-negative-long-float)
                  (estadoseguinte 0))
            (cond ((funcall (problema-teste-corte-p problema) estadoactual nil)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                  (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
                        (setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
                        (if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
                            (setf valor (max valor (maxvalor estadoseguinte alfa beta)))
                            (setf valor (max valor (minvalor estadoseguinte alfa beta)))
						)
                        (setf alfa (max alfa valor))
					  )
                      valor
				   )
		    )
			)
		)

        (defun minvalor(estadoactual alfa beta)
			(let ((valor most-positive-long-float)
                  (estadoseguinte 0))
            (cond ((funcall (problema-teste-corte-p problema) estadoactual nil)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                   (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
						(setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
						(if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
							(setf valor (min valor (maxvalor estadoseguinte alfa beta)))
							(setf valor (min valor (minvalor estadoseguinte alfa beta)))
						)
						(setf beta (min beta valor))
					   )
					   valor
					)
			)
			)
		)

        (setf valoradversario 0)
        (dolist (el (funcall (problema-accoes problema) estadoinicial) nil)
            (if (= jogador (funcall (problema-jogador problema) (funcall (problema-resultado problema) estadoinicial el)))
                (setf valoradversario (maxvalor (funcall (problema-resultado problema) estadoinicial el) alfa beta))
                (setf valoradversario (minvalor (funcall (problema-resultado problema) estadoinicial el) alfa beta))
			)
            (cond ((= jogadorinicial jogador)(when (> valoradversario alfa)(setf alfa (max alfa valoradversario))(setf melhoraccao el)))
				  (t (when (< valoradversario beta)
                        (setf beta (min beta valoradversario))
                        (setf melhoraccao el)
					 )
				  )
			)
		)
		(if (= jogadorinicial jogador)
			(values melhoraccao alfa nos)
			(values melhoraccao beta nos)
		)
	)
)


;;;MINIMAX CORTES ALFA BETA 
(defun minimax-alfa-beta(problema jogador)
    
		(let*((estadoinicial (problema-estado-inicial problema))
			  (jogadorinicial (funcall (problema-jogador problema) estadoinicial))
			  (melhoraccao 0)
			  (nos 0)
			  (alfa most-negative-long-float)
			  (beta most-positive-long-float)
			  (valoradversario 0)
			 )

        (defun maxvalor-alfa-beta(estadoactual alfa beta)
			(let ((valor most-negative-long-float)
                  (estadoseguinte 0))
			
            (cond ((funcall (problema-teste-corte-p problema) estadoactual nil)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                  (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
                        (setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
                        (if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
                            (setf valor (max valor (maxvalor-alfa-beta estadoseguinte alfa beta)))
                            (setf valor (max valor (minvalor-alfa-beta estadoseguinte alfa beta)))
						)
                        (if (>= valor beta)
							(progn valor (return))
							(setf alfa (max alfa valor))
						)
					 )
				     valor
				  )
			)
			)
		)

        (defun minvalor-alfa-beta(estadoactual alfa beta)
			(let ((valor most-positive-long-float)
                  (estadoseguinte 0))
			
            (cond ((funcall (problema-teste-corte-p problema) estadoactual nil)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                  (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
					    (setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
						(if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
							(setf valor (min valor (maxvalor-alfa-beta estadoseguinte alfa beta)))
							(setf valor (min valor (minvalor-alfa-beta estadoseguinte alfa beta)))
						)
					    (if (<= valor alfa) 
							(progn valor (return))
							(setf beta (min beta valor))
						)
					 )
					 valor
				   )
		    )
			)
		)

        (setf valoradversario 0)
        (dolist (el (funcall (problema-accoes problema) estadoinicial) nil)
              (if (= jogador (funcall (problema-jogador problema) (funcall (problema-resultado problema) estadoinicial el)))
                   (setf valoradversario (maxvalor-alfa-beta (funcall (problema-resultado problema) estadoinicial el) alfa beta))
                   (setf valoradversario (minvalor-alfa-beta (funcall (problema-resultado problema) estadoinicial el) alfa beta))
			  )
              (cond((= jogadorinicial jogador)
						(when (> valoradversario alfa)
							(setf alfa (max alfa valoradversario))
							(setf melhoraccao el)
						)
						(when (>= alfa beta)(return))
					)
					(t (when (< valoradversario beta)
                        (setf beta (min beta valoradversario))
                        (setf melhoraccao el))
						(when (>= alfa beta)(return))
					)
			  )
		)
		(if (= jogadorinicial jogador)
			(values melhoraccao alfa nos)
			(values melhoraccao beta nos)
		)
		)
)

;;;MINIMAX VBEST
(defun minimax-vbest(problema jogador tempo)
    
		(let*((estadoinicial (problema-estado-inicial problema))
			  (jogadorinicial (funcall (problema-jogador problema) estadoinicial))
			  (melhoraccao 0)
			  (nos 0)
			  (alfa most-negative-long-float)
			  (beta most-positive-long-float)
			  (valoradversario 0)
			  (profundidade 1)
			  (tempoinicial (get-internal-real-time))
			  (tempodecorrido 0)
			 )

        (defun maxvalor-vbest(estadoactual alfa beta)
			(let ((valor most-negative-long-float)
                  (estadoseguinte 0))
			
            (cond ((funcall (problema-teste-corte-p problema) estadoactual profundidade)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                  (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
                        (setf tempodecorrido (- (get-internal-real-time) tempoinicial))
						(setf tempodecorrido (/ tempodecorrido internal-time-units-per-second))
						(when (> tempodecorrido (- tempo 0.1)) 
							(return)
						)
						(setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
                        (if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
                            (setf valor (max valor (maxvalor-vbest estadoseguinte alfa beta)))
                            (setf valor (max valor (minvalor-vbest estadoseguinte alfa beta)))
						)
                        (if (>= valor beta)
							(progn valor (return))
							(setf alfa (max alfa valor))
						)
					 )
				     (progn (setf profundidade (+ profundidade 1)) valor)
				  )
			)
			)
		)

        (defun minvalor-vbest(estadoactual alfa beta)
			(let ((valor most-positive-long-float)
                  (estadoseguinte 0))
			
            (cond ((funcall (problema-teste-corte-p problema) estadoactual profundidade)(setf nos (+ nos 1))(funcall (problema-funcao-avaliacao problema) estadoactual jogador))
                  (t (dolist (el (funcall (problema-accoes problema) estadoactual) nil)
					    (setf tempodecorrido (- (get-internal-real-time) tempoinicial))
						(setf tempodecorrido (/ tempodecorrido internal-time-units-per-second))
						(when (> tempodecorrido (- tempo 0.1)) 
							(return)
						)
						(setf estadoseguinte (funcall (problema-resultado problema) estadoactual el))
						(if (= (funcall (problema-jogador problema) estadoseguinte) jogador)
							(setf valor (min valor (maxvalor-vbest estadoseguinte alfa beta)))
							(setf valor (min valor (minvalor-vbest estadoseguinte alfa beta)))
						)
					    (if (<= valor alfa) 
							(progn valor (return))
							(setf beta (min beta valor))
						)
					 )
					 (progn (setf profundidade (+ profundidade 1)) valor)
				   )
		    )
			)
		)

        (setf valoradversario 0)
        (dolist (el (funcall (problema-accoes problema) estadoinicial) nil)
              (if (= jogador (funcall (problema-jogador problema) (funcall (problema-resultado problema) estadoinicial el)))
                   (setf valoradversario (maxvalor-vbest (funcall (problema-resultado problema) estadoinicial el) alfa beta))
                   (setf valoradversario (minvalor-vbest (funcall (problema-resultado problema) estadoinicial el) alfa beta))
			  )
              (cond((= jogadorinicial jogador)
						(when (> valoradversario alfa)
							(setf alfa (max alfa valoradversario))
							(setf melhoraccao el)
						)
						(when (>= alfa beta)(return))
					)
					(t (when (< valoradversario beta)
                        (setf beta (min beta valoradversario))
                        (setf melhoraccao el))
						(when (>= alfa beta)(return))
					)
			  )
		)
		(if (= jogadorinicial jogador)
			(values melhoraccao alfa nos)
			(values melhoraccao beta nos)
		)
		)
)

(defun jogador-minimax-simples(jogo jogador tempo)
	(declare (ignore tempo))
	
	(let ((resultado 0))
	
	(progn
		(setf resultado (minimax (make-problema :estado-inicial jogo
								:jogador #'jogo-jogador 
								:accoes #'accoes 
								:resultado #'resultado 
								:teste-corte-p #'teste-terminal-p 
								:funcao-avaliacao #'utilidade) 
				  jogador)
		)	
	)
	)
)

(defun jogador-minimax-vbest(jogo jogador tempo)
	
	(let ((resultado 0))
	
	(progn
		(setf resultado (minimax-vbest (make-problema :estado-inicial jogo
								:jogador #'jogo-jogador 
								:accoes #'accoes 
								:resultado #'resultado 
								:teste-corte-p #'teste-terminal-p 
								:funcao-avaliacao #'utilidade) 
				  jogador tempo)
		)	
	)
	)
)
		  

	
(load "interface-moedas.fas")
(load "exemplos.fas")
;;;(load (compile-file "interface-moedas.lisp"))
;;;(load (compile-file "exemplos.lisp"))
	
	

	
	
	
