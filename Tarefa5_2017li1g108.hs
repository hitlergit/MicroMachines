{-|
Module      : Tarefa5_2017li1g108
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Mapas
import Tarefa4_2017li1g108
import Tarefa3_2017li1g108
import Tarefa2_2017li1g108
import Tarefa1_2017li1g108
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float
import Data.Char
import Data.List
import Data.Maybe


{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}
main :: IO ()
main = do planeta0 <- loadBMP "planet_stock_png_by_ravenmaddartwork-d2zay40.bmp"
          planeta1 <- loadBMP "planet_venus_png_by_ravenmaddartwork-d2zavun.bmp"
          space   <- loadBMP "universo.bmp"
          putStrLn "escolha um jogo (1 a 4): "
          jogada <- escolheJogo
          play (InWindow "Micro Planets" (((ncolunasx (tabuleiroJogo jogada)) * 50), ((nlinhasy  (tabuleiroJogo jogada)) * 50)) (200, 100)) --dm
               red 
               fr
               (estadoInicial jogada (Scale 0.02556 0.02556 planeta0) (Scale 0.02556 0.02556 planeta1) (Scale 1.5 1.5 space)) 
               desenhaEstado 
               reageEvento 
               reageTempo

{-|
Estado com tempo do jogo, jogo, imagens e as posições e direções de cada planeta
-}
type Estado = (Tempo, Jogo, Picture, Picture, Picture, [Float], [Float])

{-|
Função que recebe o jogo escolhido pelo jogador, as imagens dos planetas e do espaço e devolve um Estado
-}
estadoInicial :: Jogo -> Picture -> Picture -> Picture -> Estado
estadoInicial j c1 c2 la = (60, j, c1, c2, la, [x0, y0, 0, 0, d0, 0], [x1, y1, 0, 0, d1, 0])
    where x1 = (double2Float (( a*50 - (w/2)) - 25)) 
          y1 = (double2Float ((-b*50 + (h/2)) + 25))
          x0 = (double2Float (( d*50 - (w/2)) - 25)) 
          y0 = (double2Float ((-e*50 + (h/2)) + 25))
          w  = fromIntegral (ncolunasx (tabuleiroJogo j)* 50)
          h  = fromIntegral (nlinhasy  (tabuleiroJogo j) * 50)
          (a, b) = posicao (carroJogo j 1)
          (d, e) = posicao (carroJogo j 0)
          d0 = double2Float (direcao (carroJogo j 0))
          d1 = double2Float (direcao (carroJogo j 1))

{-|
Frame rate
-}
fr :: Int
fr = 60

{-|
Dimensões da janela
-}
width, height :: Jogo -> Int
width  j = ncolunasx (tabuleiroJogo j) * 50
height j = nlinhasy  (tabuleiroJogo j) * 50

{-|
Posição no ecrã
-}
offset :: Int
offset = 200


--dm :: Display
--dm = InWindow "Novo Jogo" (width, height) (offset, offset)


{-|
Função que devolve o jogo do Estado fornecido
-}
simplifica :: Estado -> Jogo
simplifica (t, j, c1, c2, la, l0, l1) = j

{-|
Função que devolve o tabuleiro do Jogo fornecido
-}
tabuleiroJogo (Jogo (Mapa (i,o) t) p c n h) = t

{-|
Função que dado um jogo e o número do carro indica a direção desse carro
-}
carroDirecao :: Jogo -> Int -> Angulo
carroDirecao (Jogo m p c n h) x = direcao (c !! x)

{-|
Função que dado um jogo e o número dum carro devolve esse Carro 
-}
carroJogo :: Jogo -> Int -> Carro
carroJogo (Jogo m p c n h) x = c !! x

{-|
Função que transforma os pontos do jogo para o referencial usado no Gloss 
(aonde o ponto (0,0) representa o meio do ecra e nao a ponta superior esquerda do tabuleiro)
-}
tranformaPontos :: Ponto -> Jogo -> Ponto
tranformaPontos (x, y) j = (50* x -(fromIntegral (width j)/2), -50*y + fromIntegral (height j)/2)

{-|
Função usada para desenhar o Estado atual,
que inclui o Tabuleiro e os Carros
-}
desenhaEstado :: Estado -> Picture
desenhaEstado  e@(t, j, c0, c1, la, l0, l1) = pictures [desenhaTabuleiro e (0,0) tab, desenhaPartida e, desenhaCarro12 e]
    where tab = tabuleiroJogo j


{-|
Função que desenha o Tabuleiro,
junta as peças todas do tabuleiro
-}
desenhaTabuleiro :: Estado -> Posicao -> Tabuleiro -> Picture
desenhaTabuleiro e (x,y) t | x == ncolunasx t && y == nlinhasy t = d
                           | x == ncolunasx t                    = pictures [d, desenhaTabuleiro e (0, y+1) t]
                           | otherwise                           = pictures [d, desenhaTabuleiro e (x+1, y) t]
        where d = desenhaPeca e (Peca (posTipoPeca (x,y) t) (posAltPeca (x,y) t)) (x, y)


{-|
Função que desenha uma peça
-}
desenhaPeca :: Estado -> Peca -> Posicao -> Picture
desenhaPeca p (Peca a h) (x, y) | a == Lava        =  translate e f $ Scale 0.07 0.07 lava --color orange $ rectangleSolid 50 50
                                | a == Recta       =  translate e f $ color (greyN ch) $ rectangleSolid 50 50
                                | a == Curva Norte =  translate e f $ pictures [Scale 0.07 0.07 lava, color (greyN ch) $ Polygon [(-25,-25),(25,-25),(25,25),(-25,-25)]]
                                | a == Curva Este  =  translate e f $ pictures [Scale 0.07 0.07 lava, color (greyN ch) $ Polygon [(-25,-25),(25,-25),(-25,25),(-25,-25)]]
                                | a == Curva Sul   =  translate e f $ pictures [Scale 0.07 0.07 lava, color (greyN ch) $ Polygon [(-25,-25),(25,25),(-25,25),(-25,-25)]]
                                | a == Curva Oeste =  translate e f $ pictures [Scale 0.07 0.07 lava, color (greyN ch) $ Polygon [(25,25),(-25,25),(25,-25),(25,25)]]
                                | a == Rampa Este  =  translate e f $ pictures [color (greyN ch) $ rectangleSolid 50 50, color white $ Polygon [(0,0),(-20,20),(20,0),(-20,-20),(0,0)]]
                                | a == Rampa Oeste =  translate e f $ pictures [color (greyN ch) $ rectangleSolid 50 50, color white $ Polygon [(0,0),(20,20),(-20,0),(20,-20),(0,0)]]
                                | a == Rampa Norte =  translate e f $ pictures [color (greyN ch) $ rectangleSolid 50 50, color white $ Polygon [(0,0),(-20,-20),(0,20),(20,-20),(0,0)]]
                                | a == Rampa Sul   =  translate e f $ pictures [color (greyN ch) $ rectangleSolid 50 50, color white $ Polygon [(0,0),(20,-20),(0,20),(-20,-20),(0,0)]]
         where (b, c) = tranformaPontos (fromIntegral x, fromIntegral y) (simplifica p)
               e = double2Float b 
               f = double2Float c
               ch = (abs ((fromIntegral h) / 10)) + 0.2
               (t, j, c1, c2, lava, l0, l1) = p



{-|
Função que desenha a posição da partida
-}
desenhaPartida :: Estado -> Picture
desenhaPartida e = translate g f $ pictures [color green $ rectangleSolid 5 50]
        where (b, d) = tranformaPontos (fromIntegral x, fromIntegral y) (simplifica e)
              g = double2Float b - 25
              f = double2Float d
              (tp, (Jogo (Mapa ((x,y),o) t) p c n h), c1, c2, lava, l0, l1) = e


{-|
Função que desenha os Carros do Jogo
-}
desenhaCarro12 :: Estado -> Picture
desenhaCarro12 (t, j, c0, c1, la, [x0, y0, vx0, vy0, d0, d00], [x1, y1, vx1, vy1, d1, d01]) = pictures [translate x0 y0 (rotate d0 c0), translate x1 y1 (rotate d1 c1)]

{-|
Função que permite que o jogador controle os diferentes carros com o teclado
-}
reageEvento :: Event -> Estado -> Estado

reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, vx,  2, d, 1], l1)
  where e = atualizaMovimentaA 1 j 0 a01
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, vx, -2, d, 1], l1)
  where e = atualizaMovimentaA 1 j 0 a02
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, -2, vy, d, 1], l1) 
  where e = atualizaMovimentaA 1 j 0 a03
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y,  2, vy, d, 1], l1) 
  where e = atualizaMovimentaA 1 j 0 a04

reageEvento (EventKey (SpecialKey KeyUp)    Up   _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, vx, 0, d, d00], l1)
  where e = atualizaMovimentaA 1 j 0 a05
reageEvento (EventKey (SpecialKey KeyDown)  Up   _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, vx, 0, d, d00], l1)
  where e = atualizaMovimentaA 1 j 0 a05
reageEvento (EventKey (SpecialKey KeyLeft)  Up   _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, 0, vy, d, d00], l1)
  where e = atualizaMovimentaA 1 j 0 a05
reageEvento (EventKey (SpecialKey KeyRight) Up   _ _) (t, j, c0, c1, la, [x, y, vx, vy, d, d00], l1) = (t, e, c0, c1, la, [x, y, 0, vy, d, d00], l1)
  where e = atualizaMovimentaA 1 j 0 a05


reageEvento (EventKey (Char 'w')  Down _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, vx,  2, d, 1])
  where e = atualizaMovimentaA 1 j 1 a01
reageEvento (EventKey (Char 's')  Down _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, vx, -2, d, 1])
  where e = atualizaMovimentaA 1 j 1 a02
reageEvento (EventKey (Char 'a')  Down _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, -2, vy, d, 1])
  where e = atualizaMovimentaA 1 j 1 a03
reageEvento (EventKey (Char 'd')  Down _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y,  2, vy, d, 1])
  where e = atualizaMovimentaA 1 j 1 a04

reageEvento (EventKey (Char 'w')  Up   _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, vx, 0, d, d01])
  where e = atualizaMovimentaA 1 j 1 a05
reageEvento (EventKey (Char 's')  Up   _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, vx, 0, d, d01])
  where e = atualizaMovimentaA 1 j 1 a05
reageEvento (EventKey (Char 'a')  Up   _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, 0, vy, d, d01])
  where e = atualizaMovimentaA 1 j 1 a05
reageEvento (EventKey (Char 'd')  Up   _ _) (t, j, c0, c1, la, l0, [x, y, vx, vy, d, d01])           = (t, e, c0, c1, la, l0, [x, y, 0, vy, d, d01])
  where e = atualizaMovimentaA 1 j 1 a05

reageEvento _ s                                                                                      = s

{-|
Função que recebe um tempo, um jogo, o número do jogador e uma ação aplicada a este e atualiza o Jogo 
-}
atualizaMovimentaA :: Tempo -> Jogo -> Int -> Acao -> Jogo
atualizaMovimentaA tick e j a = Jogo (mapa e) (pista e) x (nitros (atualiza (1/60) e j a)) (historico (atualiza (1/60) e j a))
                            where 
                                Mapa ind m = mapa e
                                car = (carros (atualiza (1/60) e j a))!! j
                                x = substitui j s (carros e)
                                s = if r == Nothing then c01 (mapa e) else fromJust r
                                r = movimenta m (1/60) car

{-|
Função que não permite que os carros passem através de um certo limite,
neste caso o limite é a janela do jogo
-}
reageTempo :: Float -> Estado -> Estado
reageTempo n e@(t, j, c0, c1, la, l0, l1) | (x0 + vx0) >  wg = (t, j, c0, c1, la, [x00, y00, 0, 0, di00, 0], l1)
                                          | (x0 + vx0) < -wg = (t, j, c0, c1, la, [x00, y00, 0, 0, di00, 0], l1)
                                          | (y0 + vy0) >  wg = (t, j, c0, c1, la, [x00, y00, 0, 0, di00, 0], l1)
                                          | (y0 + vy0) < -wg = (t, j, c0, c1, la, [x00, y00, 0, 0, di00, 0], l1)
                                          | (x1 + vx1) >  wg = (t, j, c0, c1, la, l0, [x01, y01, 0, 0, di01, 0])
                                          | (x1 + vx1) < -wg = (t, j, c0, c1, la, l0, [x01, y01, 0, 0, di01, 0])
                                          | (y1 + vy1) >  wg = (t, j, c0, c1, la, l0, [x01, y01, 0, 0, di01, 0])
                                          | (y1 + vy1) < -wg = (t, j, c0, c1, la, l0, [x01, y01, 0, 0, di01, 0])
                                          |otherwise        = (t, j, c0, c1, la, [x0+vx0, y0+vy0, vx0, vy0, d0+d00, d00], [x1+vx1, y1+vy1, vx1, vy1, d1+d01, d01])
                       where [x0, y0, vx0, vy0, d0, d00] = l0
                             [x1, y1, vx1, vy1, d1, d01] = l1
                             wg = (fromIntegral (width j))/ 2 - 25
                             x01 = (double2Float (( a*50 - (w/2)) - 25)) 
                             y01 = (double2Float ((-b*50 + (h/2)) + 25))
                             x00 = (double2Float (( d*50 - (w/2)) - 25)) 
                             y00 = (double2Float ((-e*50 + (h/2)) + 25))
                             w  = fromIntegral (ncolunasx (tabuleiroJogo j) * 50)
                             h  = fromIntegral (nlinhasy  (tabuleiroJogo j) * 50)
                             (a, b) = posicao (carroJogo j 1)
                             (d, e) = posicao (carroJogo j 0)
                             di00 = double2Float (direcao (carroJogo j 0))
                             di01 = double2Float (direcao (carroJogo j 1))
                            
                            
{-|
Função que permite que o jogador escolha um jogo 
de entre os 32 jogos já definidos para os testes da Tarefa4 
-}
escolheJogo :: IO Jogo
escolheJogo = do n <- leInt
            --  r <- escolheJogo1
                 return (l!!(n-1)) 
   where l = [j01, j02, j03, j04]
    

{-|
Função que lê um número
-}
leInt :: IO Int
leInt = do c <- getChar
           return (read [c])


{-|
Jogos que o jogador poderá escolher
-}
m01 = constroi [Avanca,Avanca,CurvaDir,Sobe,CurvaDir,Avanca,Avanca,CurvaDir,Desce,CurvaDir]
m02 = constroi [CurvaEsq,Avanca,Avanca,CurvaDir,Sobe,Sobe,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Avanca,Desce,CurvaDir,CurvaEsq,CurvaDir,CurvaDir]
m03 = constroi [Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Sobe,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,Avanca,CurvaEsq,CurvaEsq,Avanca,Avanca]
m04 = constroi [Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Desce,CurvaDir,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Sobe,Desce,CurvaEsq,Desce,Sobe,CurvaDir,CurvaDir,Avanca,Avanca,Avanca]


a01  = Acao {acelerar = True,  travar = False, direita = False, esquerda = False, nitro = Nothing } -- Acelera
a02  = Acao {acelerar = False, travar = True,  direita = False, esquerda = False, nitro = Nothing } -- Trava
a03  = Acao {acelerar = False, travar = False, direita = True,  esquerda = False, nitro = Nothing } -- Direita
a04  = Acao {acelerar = False, travar = False, direita = False, esquerda = True,  nitro = Nothing } -- Esquerda
a05  = Acao {acelerar = False, travar = False, direita = False, esquerda = False, nitro = Nothing } -- Nothing
a06  = Acao {acelerar = False, travar = False, direita = False, esquerda = False, nitro = Just 0  } -- nitro 0

c01:: Mapa -> Carro
c01 m = Carro {posicao = (x, y+0.5), direcao = 0.00, velocidade = (0,0)}
    where x = fromIntegral (fst (posicaoinicialTabuleiro m))
          y = fromIntegral (snd (posicaoinicialTabuleiro m))
c02   = Carro {posicao = (2.0,1.5), direcao = 0.00, velocidade = (0,0)}


n01 = [5,5,5]

h01 :: [[Posicao]]
h01 = [[(2,3)],[(3,4)]]

j01  = Jogo m1 asfalto [c01 m1,c01 m01] n01 h01
j02  = Jogo m2 terra   [c01 m2,c01 m02] n01 h01
j03  = Jogo m3 gelo    [c01 m3,c01 m03] n01 h01
j04  = Jogo m4 asfalto [c01 m4,c01 m04] n01 h01
