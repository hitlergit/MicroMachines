
module Tarefa2_2017li1g108 where

import LI11718

{- Module     : Tarefa2_2017li1g108
  Description : Resolução da Tarefa 2

Verifica a validade dos Mapas
-}

-- | Função com vários tabuleiros para teste
testesT2 :: [Tabuleiro]
testesT2 = [[[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca (Curva Norte) 2, Peca (Curva Este) 2],
           [Peca (Curva Oeste) 2, Peca (Curva Sul) 2]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0],
           [Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Este) 0],
           [Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0],
           [Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Oeste)0, Peca (Rampa Este) 0, Peca (Curva Este)0, Peca Lava 0],
           [Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Rampa Oeste) 0, Peca (Rampa Este) 0, Peca (Curva Sul)0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Este) 0, Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca  Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Oeste) 0, Peca (Rampa Oeste) (-1), Peca (Curva Este) (-2), Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta (-2), Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta (-2), Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Rampa Oeste) 0, Peca (Rampa Oeste) (-1), Peca (Curva Sul) (-2), Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0,Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Curva Norte) 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Oeste) 0, Peca (Rampa Este) 1, Peca (Curva Este) 0,Peca Lava 0],
           [Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Sul) 1, Peca Lava 0],
           [Peca Lava 0, Peca (Rampa Sul) 1, Peca Lava 0, Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca (Rampa Oeste) 1, Peca (Rampa Este) 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]],
           [[Peca Lava 0, Peca Lava 0, Peca Lava 1, Peca Lava 0, Peca Lava 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Norte) 0,  Peca Recta 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],
           [Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0],
           [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]]


-- | Função que testa a veracidade de um Mapa
valida :: Mapa -> Bool
valida m = igualtamanhomapa t && rodeadolava t && posinicial m  && alturaLava (0,0) t && novaAlturaValida (0,0) t && percursoValido (0,0) t && percursoUnico (0,0) t && caminhofechado (0,0) t && caminhorecta p o t
   where t = mapTabuleiro m
         o = orientacaoTabuleiro m
         p = posicaoinicialTabuleiro m

-- | Função que dado um Mapa devolve um Tabuleiro
mapTabuleiro :: Mapa -> Tabuleiro
mapTabuleiro (Mapa (p,o) t) = t


-- | Função que dado um Mapa devolve a orientação inicial
orientacaoTabuleiro :: Mapa -> Orientacao
orientacaoTabuleiro (Mapa (p,o) t) = o


-- | Função que dado um Mapa devolve a posição inicial
posicaoinicialTabuleiro :: Mapa -> Posicao
posicaoinicialTabuleiro (Mapa (p,o) t) = p


-- | Função que dado uma Posicao e um Tabuleiro devolve o Tipo da Peça nessa posição
posTipoPeca :: Posicao -> Tabuleiro -> Tipo
posTipoPeca (0,0) (((Peca (x) h): t):g) = x
posTipoPeca (0,y) (h:t)  = posTipoPeca (0, (y-1)) t
posTipoPeca (x,0) (h:t)  = posTipoPeca ((x-1), 0) ((tail h):t)
posTipoPeca (x,y) ([]:t) = posTipoPeca ((x-1), y) t
posTipoPeca (x,y) (h:t)  = posTipoPeca (x, (y-1)) t


-- | Função que dado uma Posicao e um Tabuleiro devolve a Altura da Peça nessa posição
posAltPeca :: Posicao -> Tabuleiro -> Altura
posAltPeca (0,0) (((Peca (x) h): t):g) = h
posAltPeca (0,y) (h:t)  = posAltPeca (0, (y-1)) t
posAltPeca (x,0) (h:t)  = posAltPeca ((x-1), 0) ((tail h):t)
posAltPeca (x,y) ([]:t) = posAltPeca ((x-1), y) t
posAltPeca (x,y) (h:t)  = posAltPeca (x, (y-1)) t


-- | Função que dado um Mapa confirma que a posição inicial é compatível com a peça inicial
posinicial :: Mapa -> Bool
posinicial (Mapa ((x,y), o) t) | posTipoPeca (x,y) t == Lava = False
                               | o == Este  && (a == Curva Oeste || a == Curva Norte || a == Rampa Sul  || a == Rampa Norte) = False
                               | o == Oeste && (a == Curva Este  || a == Curva Sul   || a == Rampa Sul  || a == Rampa Norte) = False
                               | o == Norte && (a == Curva Oeste || a == Curva Sul   || a == Rampa Este || a == Rampa Oeste) = False
                               | o == Sul   && (a == Curva Este  || a == Curva Norte || a == Rampa Este || a == Rampa Oeste) = False
                               | o == Sul   && a == Recta && (g == Lava || g == Curva Este  || g == Curva Norte)             = False
                               | o == Norte && a == Recta && (i == Lava || i == Curva Oeste || i == Curva Oeste)             = False
                               | o == Oeste && a == Recta && (e == Lava || e == Curva Sul   || e == Curva Este)              = False
                               | o == Este  && a == Recta && (c == Lava || c == Curva Norte || c == Curva Oeste)             = False
                               | otherwise = True
        where a = posTipoPeca (x,y) t
              g = posTipoPeca (x,(y+1)) t
              i = posTipoPeca (x,(y-1)) t
              c = posTipoPeca ((x+1),y) t
              e = posTipoPeca ((x-1),y) t


-- | Função que dado um Tabuleiro confirma que este é retangular, ou seja, que todas as listas têm o mesmo tamanho
igualtamanhomapa :: Tabuleiro -> Bool
igualtamanhomapa [] = True
igualtamanhomapa [a] = True
igualtamanhomapa (a:b:c) | length a == length b = igualtamanhomapa (b:c)
                         | otherwise            = False

-- | Função que dado uma Posicao e um Tabuleiro confirma as alturas das peças Tipo Lava, a partir dessa posição
alturaLava :: Posicao -> Tabuleiro -> Bool
alturaLava (x,y) t | x == ncolunasx t && y == nlinhasy t && a == Lava && b == 0 = True
                   | x == ncolunasx t && (a /= Lava || b /= 0)                  = False
                   | x == ncolunasx t && a == Lava && b == 0                    = alturaLava (0,(y+1)) t
                   | a == Lava && b == 0                                        = alturaLava ((x+1),y) t
                   | a == Lava && b /= 0                                        = False
                   | otherwise                                                  = alturaLava ((x+1),y) t
                   where a = posTipoPeca (x,y) t
                         b = posAltPeca (x,y) t


-- | Função que dado uma Posicao e um Tabuleiro confirma as alturas das diferentes peças do tabuleiro, a partir dessa posição
novaAlturaValida :: Posicao -> Tabuleiro -> Bool
novaAlturaValida (x,y) t | x == ncolunasx t && y == nlinhasy t              = True
                         | x == ncolunasx t                                 = novaAlturaValida (0,(y+1)) t
                         | a == Rampa Este  && c == Rampa Oeste && (b /= d) = False
                         | a == Rampa Oeste && c == Rampa Este && (b /= d)  = False
                         | a == Rampa Este  && d /= (b+1)                   = False
                         | a == Rampa Oeste && f /= (b+1)                   = False
                         | a == Rampa Norte && g == Rampa Sul && (b /= h)   = False
                         | a == Rampa Sul   && g == Rampa Norte && (b /= h) = False
                         | a == Rampa Norte && j /= (b+1)                   = False
                         | a == Rampa Sul   && h /= (b+1)                   = False
                         | a == Curva Este  && b /= h                       = False
                         | (a /= Rampa Este && a /= Rampa Oeste) && b == d  = u
                         | otherwise                                        = u
   where a = posTipoPeca (x,y) t
         b = posAltPeca (x,y) t
         c = posTipoPeca ((x+1),y) t
         d = posAltPeca ((x+1),y) t
--       e = posTipoPeca ((x-1),y) t
         f = posAltPeca ((x-1),y) t
         g = posTipoPeca (x,(y+1)) t
         h = posAltPeca (x,(y+1)) t
--       i = posTipoPeca (x,(y-1)) t
         j = posAltPeca (x,(y-1)) t
         u = novaAlturaValida ((x+1),y) t


-- | Função que dado um Tabuleiro confirma se está rodeado por lava
rodeadolava :: Tabuleiro -> Bool
rodeadolava t = poslava (0,0) t


-- | Funcao que dada uma Posicao e um Tabuleiro confirma se esta rodeado por lava a partir da posição dada inicialmente
poslava :: Posicao -> Tabuleiro -> Bool
--poslava _ [] = True
--poslava (x,y) ([]:c) = poslava (0,(y+1)) c
poslava (x,y) t | x == ncolunasx t && y == nlinhasy t && a == Lava = True
                | x == ncolunasx t && a == Lava                    = poslava (0,(y+1)) t
                | x == ncolunasx t && a /= Lava                    = False
                | y == nlinhasy t  && a == Lava                    = poslava ((x+1),y) t
                | y == nlinhasy t  && a /= Lava                    = False
                | x == 0 && a == Lava                              = poslava ((x+1),y) t
                | y == 0 && a == Lava                              = poslava ((x+1),y) t
                | (y /= 0 && x /= 0)                               = poslava ((x+1),y) t
                | otherwise                                        = False
    where a = posTipoPeca (x,y) t


-- | Função que dado um Tabuleiro devolve o número de linhas que o tabuleiro tem, sendo a primeira 0
nlinhasy :: Tabuleiro -> Int
nlinhasy [] = 0
nlinhasy t = length t -1


-- | Função que dado um Tabuleiro devolve o numero de colunas que o tabuleiro tem, sendo a primeira 0
ncolunasx :: Tabuleiro -> Int
ncolunasx (l:t) = length l -1


-- | Função que dado uma Posicao e um Tabuleiro confirma se a disposição das peças é possível
percursoValido :: Posicao -> Tabuleiro -> Bool
percursoValido (x,y) t | x == ncolunasx t && y == nlinhasy t  = True
                       | x == ncolunasx t                     = percursoValido (0,(y+1)) t
                       | ((a == Curva Norte || a == Curva Oeste) && (c == Curva Norte || c == Curva Oeste || c == Rampa Norte || c == Rampa Sul || c == Lava)) = False
                       | ((a == Curva Sul   || a == Curva Este)  && (c == Rampa Oeste || c == Rampa Este  || c == Curva Este  || c == Curva Sul ))             = False
                       | otherwise                            = percursoValido ((x+1),y) t
                       where a = posTipoPeca (x,y) t
                             c = posTipoPeca ((x+1),y) t


-- | Função que dado uma Posicao e um Tabuleiro confirma que o percurso passivel de se realizar é único
percursoUnico :: Posicao -> Tabuleiro -> Bool
percursoUnico (x,y) t | x == ncolunasx t && y == nlinhasy t                                                                                                = True
                      | x == ncolunasx t                                                                                                                   = percursoUnico (0,(y+1)) t
                      | a /= Lava && c == Lava && e == Lava && g == Lava && i == Lava                                                                      = False
                      | a == Curva Norte && (g == Curva Este  || g == Curva Norte || g == Rampa Este || g == Rampa Oeste || g == Lava)                     = False
                      | a == Curva Sul   && (i == Curva Oeste || i == Curva Sul   || i == Rampa Este || i == Rampa Oeste || i == Lava)                     = False
                      | a == Curva Este  && (g == Curva Este  || g == Curva Norte || g == Rampa Este || g == Rampa Oeste || g == Lava)                     = False
                      | a == Curva Oeste && (i == Curva Oeste || i == Curva Sul   || i == Rampa Este || i == Rampa Oeste || i == Lava)                     = False
                      | a == Recta && ((c == Recta && e == Lava) || (c == Lava && e == Recta)) && ((i == Recta && g == Lava) || (i == Lava && g == Recta)) = False
                      | otherwise                                                                                                                          = percursoUnico ((x+1),y) t
             where a = posTipoPeca (x,y) t
                   c = posTipoPeca ((x+1),y) t
                   e = posTipoPeca ((x-1),y) t
                   g = posTipoPeca (x,(y+1)) t
                   i = posTipoPeca (x,(y-1)) t


-- | Função que dado uma Posicao e um Tabuleiro verifica se o caminho é fechado. Como tal a orientação final coincide com a inicial
caminhofechado :: Posicao -> Tabuleiro -> Bool
caminhofechado (x,y) t | x == ncolunasx t && y == nlinhasy t                            = True
                       | x == ncolunasx t                                               = caminhofechado (0,(y+1)) t
                       | a /= Lava && c == Lava && e == Lava && g == Lava && i == Lava  = False
                       | a /= Lava && c == Lava && e == Lava && g == Lava               = False
                       | a /= Lava && c == Lava && e == Lava && i == Lava               = False
                       | a /= Lava && e == Lava && g == Lava && i == Lava               = False
                       | otherwise                                                      = caminhofechado ((x+1),y) t
                       where a = posTipoPeca (x,y) t
                             c = posTipoPeca ((x+1),y) t
                             e = posTipoPeca ((x-1),y) t
                             g = posTipoPeca (x,(y+1)) t
                             i = posTipoPeca (x,(y-1)) t

 -- | Função que dado uma Posicao, Orientacao (inicial do Mapa) e um Tabuleiro confirma se, caso a peça inicial seja uma Reta o percurso é possivel
caminhorecta :: Posicao -> Orientacao -> Tabuleiro -> Bool
caminhorecta (x,y) o t | x == ncolunasx t && y == nlinhasy t   = True
                       | x == ncolunasx t                      = caminhorecta (0,(y+1)) o t
                       | a == Recta && o == Norte && (i == Lava || i == Curva Oeste || i == Curva Sul   || i == Rampa Este || i == Rampa Oeste) = False
                       | a == Recta && o == Este  && (c == Lava || c == Curva Norte || c == Curva Oeste || c == Rampa Sul  || c == Rampa Norte) = False
                       | a == Recta && o == Oeste && (e == Lava || e == Curva Este  || e == Curva Sul   || e == Rampa Sul  || e == Rampa Norte) = False
                       | a == Recta && o == Sul   && (g == Lava || g == Curva Norte || g == Curva Este  || g == Rampa Este || g == Rampa Oeste) = False
                       | a /= Recta                            = True
                       | o == Norte                            = caminhorecta (x,(y-1)) o t
                       | o == Sul                              = caminhorecta (x,(y+1)) o t
                       | o == Este                             = caminhorecta ((x+1),y) o t
                       | o == Oeste                            = caminhorecta ((x-1),y) o t
        where a = posTipoPeca (x,y) t
              c = posTipoPeca ((x+1),y) t
              e = posTipoPeca ((x-1),y) t
              g = posTipoPeca (x,(y+1)) t
              i = posTipoPeca (x,(y-1)) t
