{-|
Module      : Tarefa3_2017li1g108
Description : Resolução da Tarefa 3

Sistema de colisão e Movimentação do Carro pelo mapa
-}

module Tarefa3_2017li1g108 where

import LI11718
import Data.Maybe

-- | Função com tuplos da forma (Tabuleiro,Tempo,Camninho) para fim de testes
testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta (-1),Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], 1,(Carro {posicao = (2.5,3.5), direcao = 45, velocidade = (1,1)})),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] ,1, (Carro {posicao = (3.4,4.4), direcao = 45, velocidade = (0,0)})),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] ,1, (Carro {posicao = (4.5,1.4), direcao = 45, velocidade = (1,1)})),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], 1, (Carro {posicao = (4.5,1.4),direcao = 45, velocidade=(1,1)})),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], 1, (Carro {posicao = (4.5,1.4), direcao = 45, velocidade =(1,1)}))]

-- | Corre uma lista de testes do tipo (Tabuleiro, Tempo, Carro)
testaT3 :: [(Tabuleiro,Tempo,Carro)] -> [Maybe Carro]
testaT3 []          = []
testaT3 ((m,t,c):z) = movimenta m t c : testaT3 z

-- | Função que dado um tabuleiro, tempo e carro, devolve o novo estado do carro da forma de "Just c" onde c é o novo estdo do carro, ou nothing caso o carro seja destruido
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t c     | t1 == Lava = Nothing
                    | otherwise  = novaPosicao m t c
                    where 
                        Peca t1 h1 = pecaAtual m c

-- | Função que compara a altura da peça atual do carro e a peça seguinte de acordo com a velocidade do carro, e devolve Nothing quando a altura da peça atual é maior
caiPeca :: Carro -> Tabuleiro -> Tempo -> Carro -> Maybe Carro
caiPeca c' m t c    | t2 == Rampa Norte && h1 > h2  = movimenta m t c
                    | t2 == Rampa Sul   && h1 > h2  = movimenta m t c
                    | t2 == Rampa Oeste && h1 > h2  = movimenta m t c 
                    | t2 == Rampa Este  && h1 > h2  = movimenta m t c
                    | t2 `elem` tC                  = bateCurva c' m t c
                    | h1 > h2                       = Nothing
                    | otherwise                     = movimenta m t c
                    where
                        Peca t2 h2  = pecaAtual m c
                        Peca t1 h1  = pecaAtual m c'
                        tC          = [Curva Norte, Curva Sul, Curva Oeste, Curva Este]
--                    | t2 `elem` tC                  = bateCurva c' m t c
-- | Devolve o carro atualizado caso o tempo seja 0
novaPosicao :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
novaPosicao m t c   | t == 0    = Just c
                    | otherwise = caiPeca c m 0 (semColisao t c) 

-- | Calcula o carro com a posição atualiazada, de acordo com o tempo e a velocidade
semColisao :: Tempo -> Carro -> Carro
semColisao t c      = Carro {posicao = pFinal, direcao = direcao c, velocidade = velocidade c}
                    where 
                        pFinal = (a+(h*t), b+(v*t))
                        (a,b)   = posicao c
                        (h,v)   = velocidade c
-- | Calcula a peça correspondente a uma posicão no tabuleiro
pecaAtual :: Tabuleiro -> Carro -> Peca
pecaAtual m c       = (m !! b) !! a
                    where 
                        (x, y)  = posicao c
                        (a,b)   = (fromIntegral (truncate x),fromIntegral (truncate y))

-- | Determina se o carro está na parte da lava de uma peça do tipo Curva
bateCurva :: Carro -> Tabuleiro -> Tempo -> Carro -> Maybe Carro
bateCurva c' m t c  | pa == Curva Este  && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y') = Nothing
                    | pa == Curva Oeste && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y') = Nothing
                    | pa == Curva Norte && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y') = Nothing
                    | pa == Curva Sul   && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y') = Nothing
                    | otherwise                                                                                                                                     = Just c 
                    where                      
                        (x,y)   = posicao c'
                        (x',y') = posicao c 
                        (a,b)   = (truncate x', truncate y')
                        Peca pa ha  = pecaAtual m c

-- | Calcula a distancia entre dois pontos
dist :: Ponto -> Ponto -> Double
dist (x,y) (a,b)    = sqrt ((a-x)^2 + (b-y)^2)

-- | Calcula uma interseção entre duas rectas, formadas pelos dois pares (Ponto,Ponto), podendo devolver um Just Ponto ou Nothing, dependendo da existência da interseção
maybeIntersection :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
maybeIntersection ((x1, y1), (x2, y2)) ((a1, b1), (a2, b2)) | d ==  0 = Nothing
                                                            | otherwise = Just (h aX xX, h aY xY)
                                                            where
                                                                (aX, xX) = (a1 - a2, x1 - x2)
                                                                (aY, xY) = (b1 - b2, y1 - y2)
                                                                d = xX * aY - xY * aX
                                                                h xy ab = ((((x1 * y2) - (y1 * x2)) * xy) - (((a1 * b2) - (b1 * a2)) * ab)) /d
