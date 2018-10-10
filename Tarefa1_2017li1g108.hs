{-|
Module      : Tarefa1_2017li1g108
Description : Resolução da Tarefa 1

Construtor de mapas a partir de caminhos
-}

module Tarefa1_2017li1g108 where

import LI11718


-- | Função com vários caminhos para teste
testesT1 :: [Caminho]
testesT1 = [[Avanca,Avanca,CurvaDir,Sobe,CurvaDir,Avanca,Avanca,CurvaDir,Desce,CurvaDir],[Avanca,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Desce,CurvaDir],
            [CurvaEsq,Avanca,Avanca,CurvaDir,Sobe,Sobe,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Avanca,Desce,CurvaDir,CurvaEsq,CurvaDir,CurvaDir],
            [Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Sobe,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,Avanca,CurvaEsq,CurvaEsq,Avanca,Avanca],
            [Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Desce,CurvaDir,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Sobe,Desce,CurvaEsq,Desce,Sobe,CurvaDir,CurvaDir,Avanca,Avanca,Avanca],
            [Sobe,Sobe,Sobe],
            [Desce,Desce,Desce],
            [Avanca,Avanca,Avanca],
            [Sobe,Desce,Sobe],
            [Desce,Sobe,Desce],
            [CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq],
            [CurvaDir,CurvaDir,CurvaDir,CurvaDir],
            [Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca],
            [Avanca,Avanca,Avanca,CurvaDir,Avanca],
            [Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,CurvaDir,Avanca,Avanca]
            ]

-- | Função que a partir de um caminho devolve um mapa
constroi :: Caminho -> Mapa
constroi c = Mapa (partida c, dirInit) (caminhoTabuleiro c)

-- | Função que dado um caminho devolve um tabuleiro
caminhoTabuleiro :: Caminho -> Tabuleiro
caminhoTabuleiro c  | last c == CurvaDir && last (orientacaoTail c dirInit) == Norte = [replicate a (Peca Lava 0)] ++ ultimateJuntaLavaAFesta (h:t) a ++ [replicate a (Peca Lava 0)]
                    | last c == CurvaEsq && last (orientacaoTail c dirInit) == Sul = [replicate a (Peca Lava 0)] ++ ultimateJuntaLavaAFesta (h:t) a ++ [replicate a (Peca Lava 0)]
                    | last (orientacaoTail c dirInit) == Norte = [replicate a (Peca Lava 0)] ++ [replicate a (Peca Lava 0)] ++ ultimateJuntaLavaAFesta (h:t) a ++ [replicate a (Peca Lava 0)]
                    | last (orientacaoTail c dirInit) == Sul = [replicate a (Peca Lava 0)] ++ ultimateJuntaLavaAFesta (h:t) a ++ [replicate a (Peca Lava 0)] ++ [replicate a (Peca Lava 0)]
                    | otherwise = [replicate a (Peca Lava 0)] ++ ultimateJuntaLavaAFesta (h:t) a ++ [replicate a (Peca Lava 0)]
                    where
                        (h:t) = organizaTabuleiro (organizaPecasLinhas (listaPecasPosicao c))
                        (a,b) = dimensao c

-- | Transforma um Passo numa Peça, dada uma altura e uma orientação
passoPeca :: Passo -> Altura -> Orientacao -> Peca
passoPeca p h o     | p == Avanca       = Peca Recta h
                    | p == CurvaDir     = Peca (Curva o) h
                    | p == CurvaEsq     = Peca (Curva (rodaDir o)) h
                    | p == Sobe         = Peca (Rampa o) h
                    | p == Desce        = Peca (Rampa (inversoOrientacao o)) (h-1)

{- | Devolve o "Inverso" de uma orientação

>>> inversoOrientacao Norte = Sul

-}
inversoOrientacao :: Orientacao -> Orientacao
inversoOrientacao a | a == Norte    = Sul
                    | a == Sul      = Norte
                    | a == Este     = Oeste
                    | a == Oeste    = Este

-- | Dado um caminho devolve uma lista com pares de Peças e Posições
listaPecasPosicao :: Caminho -> [(Peca, Posicao)]
listaPecasPosicao c     = zip (listaPecas c a o) (posicaoPecas c p o a)
                        where
                            a = 0
                            o = Este
                            p = partida c

-- | Dado um caminho, uma posição,orientação e altura iniciais, devolve uma lista de posições de peças,
posicaoPecas :: Caminho -> Posicao -> Orientacao -> Altura -> [Posicao]
posicaoPecas [] (a,b) o r       = []
posicaoPecas [c] (a,b) o r      = [(a,b)]
posicaoPecas (q:i:f) (a,b) o p  | h == x                                                 = (a,b) : posicaoPecas (i:f) (mover (a,b) o) o r

                                | q == Desce                && o == Sul                  = (a,b) : posicaoPecas (i:f) (a, b+1) Sul r
                                | q == Desce                && o == Norte                = (a,b) : posicaoPecas (i:f) (a, b-1) Norte r
                                | q == Desce                && o == Oeste                = (a,b) : posicaoPecas (i:f) (a-1, b) Oeste r
                                | q == Desce                && o == Este                 = (a,b) : posicaoPecas (i:f) (a+1, b) Este r

                                | h == Peca (Rampa o) r                                  = (a,b) : posicaoPecas (i:f) (mover (a,b) o) o r

                                | q == CurvaEsq             && h == Peca (Curva Oeste) r = (a,b) : posicaoPecas (i:f) (a+1, b) Este r
                                | q == CurvaEsq             && h == Peca (Curva Este) r  = (a,b) : posicaoPecas (i:f) (a-1, b) Oeste r
                                | q == CurvaEsq             && h == Peca (Curva Norte) r = (a,b) : posicaoPecas (i:f) (a, b+1) Sul r
                                | q == CurvaEsq             && h == Peca (Curva Sul) r   = (a,b) : posicaoPecas (i:f) (a, b-1) Norte r

                                | h == Peca (Curva Norte) r                              = (a,b) : posicaoPecas (i:f) (a+1, b) Este r
                                | h == Peca (Curva Sul) r                                = (a,b) : posicaoPecas (i:f) (a-1, b) Oeste r
                                | h == Peca (Curva Este) r                               = (a,b) : posicaoPecas (i:f) (a, b+1) Sul r
                                | h == Peca (Curva Oeste) r                              = (a,b) : posicaoPecas (i:f) (a, b-1) Norte r
                                where
                                     x          = Peca Recta r
                                     (h:z:t)    = listaPecas (q:i:f) r o
                                     r          = head (alturaTail (q:i:f) p)
                                     mover (a,b) o | o == Norte = (a, b-1)
                                                   | o == Sul   = (a, b+1)
                                                   | o == Este  = (a+1, b)
                                                   | o == Oeste = (a-1, b)

-- | Dado um caminho, uma altura e orientação iniciais, devolve uma lista de peças correspondente ao caminho
listaPecas :: Caminho -> Altura -> Orientacao -> [Peca]
listaPecas [] a o    = []
listaPecas (h:t) a o = passoPeca h a x : (listaPecas t z p)
                     where
                        y = head (alturaTail (h:t) a)
                        x = head (orientacaoTail (h:t) o)
                        z = alturaProximaPeca h a
                        p = head (tail (orientacaoTail (h:t) o))

-- | Dado um caminho e uma orientação, devolve uma lista com as orientações correspondestes a cada Passo
orientacaoTail :: Caminho -> Orientacao -> [Orientacao]
orientacaoTail [] o    = []
orientacaoTail (h:t) o | h == Avanca || h == Sobe               = o : orientacaoTail t o
                       | h == Desce                             = o : orientacaoTail t o
                       | h == CurvaDir                          = o : orientacaoTail t (rodaDir o)
                       | h == CurvaEsq                          = o : orientacaoTail t (rodaEsq o)

-- | Dado um passo e uma Altura, devolve a altura do passo seguinte
alturaProximaPeca :: Passo -> Altura -> Altura
alturaProximaPeca p a | p == Sobe = a+1
                      | p == Desce = (a-1)
                      | otherwise = a

-- | Dado um caminho e uma altura, devolve uma lista com as alturas correspondestes a cada Altura
alturaTail :: Caminho -> Altura -> [Altura]
alturaTail [] b         = []
alturaTail [a] b        = [b]

alturaTail (h:x:t) a    | h == Sobe && x == Desce   = a : alturaTail (x:t) a
                        | x == Desce                = a : alturaTail (x:t) (a-1)
                        | h == Sobe                 = a : alturaTail (x:t) (a+1)
                        | otherwise                 = a : alturaTail (x:t) a

-- | Dada uma orientação, devolve a orientação correspondente a uma rotação à direita
rodaDir :: Orientacao -> Orientacao
rodaDir o | o == Norte = Este
          | o == Este  = Sul
          | o == Sul   = Oeste
          | o == Oeste = Norte

-- | Dada uma orientação, devolve a orientação correspondente a uma rotação à esquerda
rodaEsq :: Orientacao -> Orientacao
rodaEsq o | o == Norte = Oeste
          | o == Oeste = Sul
          | o == Sul   = Este
          | o == Este  = Norte

-- | Agrupa todas as peças (diferentes de Lava) e posições correspondentes à mesma linha de um tabuleiro
organizaPecasLinhas :: [(Peca,Posicao)] -> [[(Peca,Posicao)]]
organizaPecasLinhas []      = []
organizaPecasLinhas l   = ordenaPecasLinha(aux a) : organizaPecasLinhas (aux1 a (aux a))
                            where
                                aux :: [(Peca,Posicao)] -> [(Peca,Posicao)]
                                aux [a]             = [a]
                                aux ((a,b):(p,c):t) | snd b == snd c    = (p,c) : aux ((a,b):t)
                                                    | otherwise         = aux ((a,b):t)
                                aux1 :: [(Peca,Posicao)] -> [(Peca,Posicao)] -> [(Peca,Posicao)]
                                aux1 [] (a:t)               = []
                                aux1 ((a,b):t) ((d,c):z)    | snd b == snd c    = aux1 t ((d,c):z)
                                                            | otherwise         = (a,b) : aux1 t ((d,c):t)
                                a = temRepetidas l

-- | Verifica e elimina, caso existam, peças com a mesma posição, deixando apenas a ultima peça que ocupa a posição
temRepetidas :: [(Peca,Posicao)] -> [(Peca,Posicao)]
temRepetidas [] = []
temRepetidas [a] = [a]
temRepetidas (h:t) | quantosRepetidos h (h:t) > 1 = temRepetidas t
                   | otherwise = h: temRepetidas (t)
                     where
                           quantosRepetidos a [] = 0
                           quantosRepetidos a (h:t) | snd a == snd h = 1 + quantosRepetidos a t
                                                    | otherwise = quantosRepetidos a t


-- | Ordena as linhas de um tabuleiro para a sua posição correta
organizaTabuleiro :: [[(Peca,Posicao)]] -> [[(Peca,Posicao)]]
organizaTabuleiro [a]       = [a]
organizaTabuleiro (h:x:t)   = ordena h (organizaTabuleiro (x:t))
                            where
                                ordena :: [(Peca,Posicao)] -> [[(Peca,Posicao)]] -> [[(Peca,Posicao)]]
                                ordena a [] = [a]
                                ordena (x:y) (h:t)  | y1 < y2   = (x:y) : (h:t)
                                                    | otherwise = h : ordena (x:y) t
                                                    where
                                                        (a,b)   = x
                                                        (x1,y1) = b
                                                        (r:o)   = h
                                                        (n,m)   = r
                                                        (x2,y2) = m

-- | Preenche as posições entre peças da linha com Lava
juntaLavaAoMeio :: [(Peca,Posicao)] -> [Peca]
juntaLavaAoMeio []              = [Peca Lava 0]
juntaLavaAoMeio [(a,b)]         = [a,Peca Lava 0]
juntaLavaAoMeio ((a,b):(x,y):t) | x2 == x1 + 1  = a : juntaLavaAoMeio ((x,y):t)
                                | otherwise     = [a] ++ replicate (x2 - x1 - 1) (Peca Lava 0) ++ juntaLavaAoMeio ((x,y):t)
                                where
                                    (x1,y1) = b
                                    (x2,y2) = y

-- | Adiciona Lava ao inicio e entre peças da linha
juntaLavaAoInicio :: [(Peca,Posicao)] -> [Peca]
juntaLavaAoInicio ((a,b):t) = replicate (fst b) (Peca Lava 0) ++ juntaLavaAoMeio ((a,b):t)

-- | Constroi uma lista de peças, a partir de uma lista de peças e posições
juntaLavaAFesta :: [(Peca,Posicao)] -> Int -> [Peca]
juntaLavaAFesta a x | length (juntaLavaAoInicio a) == x = juntaLavaAoInicio a
                    | otherwise                         = juntaLavaAoInicio a ++ replicate (x - length (juntaLavaAoInicio a)  ) (Peca Lava 0)

-- | Dadas todas as linhas (com peças diferentes de lava), e o tamanho pretendidio de cada linha, adiciona Lava a todos os sitios necessarios para se obter uma linha do tabuleiro
ultimateJuntaLavaAFesta :: [[(Peca,Posicao)]] -> Int -> [[Peca]]
ultimateJuntaLavaAFesta t x = map (`juntaLavaAFesta` x) t

-- | Ordena uma lista de pares Peça Posição por ordem que aparecem no mapa, da esquerda para a direira
ordenaPecasLinha :: [(Peca,Posicao)] -> [(Peca,Posicao)]
ordenaPecasLinha []     = []
ordenaPecasLinha (a:t)  = organiza a (ordenaPecasLinha t)
                        where
                            organiza a [] = [a]
                            organiza (x,y) ((a,b):t) | fst y <= fst b   = (x,y):(a,b):t
                                                     | otherwise        = (a,b) : organiza (x,y) t
