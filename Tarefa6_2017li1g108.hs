{-|
Module      : Tarefa6_2017li1g108
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g108 where
import Tarefa4_2017li1g108 
import Data.Maybe
import LI11718

{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick e j   = Acao {acelerar = needGas tick e j, travar = needBrakes tick e j, esquerda = needEsq tick e j, direita = needDir tick e j, nitro = needNitro tick e j }

{-| Transforma qualquer ângulo em graus, num ângulo entre 0 e 360 graus

Exemplos:

>>> l (-90)
270

>>> l 567
207

-}
l :: Angulo -> Angulo
l o     | o >= 0 && o <  360        = o 
        | o < 0 && o >= -360        = o + 360
        | o < -360                  = l (o+360)
        | o >= 360                  = l (o-360)

-- | Calcula a peça que está na posição dada no tabuleiro
calcP :: Tabuleiro -> Posicao -> Peca
calcP t (a,b)         = t !! b !! a

{-| Devolve a cabeça da tail de uma lista, e caso o tamanho da lista seja 1, devolve a cabeça da lista

Exemplos:

>>> daHead [1,2,3,4] 
2

>>> daHead [7]
7

-}
daHead :: [a] -> a                       
daHead a    | length a == 1     = head a
            | otherwise         = head (tail a)


-- | Calcula a peça seguinte do carro, de acordo com a sua posição e o seu histórico                                              
pSeg :: Tempo -> Jogo -> Int -> Peca
pSeg tick e j   = calcP t (posSeg tick e j)
                where 
                    Mapa ind t  = mapa e
                    (zzzz,yyyy) = posicao ((carros e)!!j)
                    (x,y)       = (truncate zzzz, truncate yyyy)
                    (a,b)      = head (historico e)!!j
                    Peca pa ha  = calcP t (x,y)

-- | Calcula a posição seguinte do carro, de acordo com a sua posição e o seu histórico
posSeg :: Tempo -> Jogo -> Int -> Posicao
posSeg tick e j | null ((historico e) !! j) || null (historico e) = (x,y)
                | pa == Recta && length ((historico e)!! j) == 1 = (x+1,y)
                | isRampa pa  && length ((historico e)!! j) == 1 = (x+1,y)
                | pa == Curva Este && length ((historico e)!! j) == 1 = (x,y+1)
                | pa == Curva Sul && length ((historico e)!! j) == 1 = (x,y-1)
                | pa == Recta       && a > x && b == y     = (x-1,y)
                | pa == Recta       && a < x && b == y     = (x+1,y)
                | pa == Recta       && a == x && b > y     = (x,y-1)
                | pa == Recta       && a == x && b < y     = (x,y+1)
                | isRampa pa        && a > x && b == y     = (x-1,y)
                | isRampa pa        && a < x && b == y     = (x+1,y)
                | isRampa pa        && a == x && b > y     = (x,y-1)
                | isRampa pa        && a == x && b < y     = (x,y+1)
                | pa == Curva Norte && a == x && b > y     = (x+1,y)
                | pa == Curva Norte && a > x && b == y     = (x,y+1)
                | pa == Curva Este  && a < x && b == y     = (x,y+1)
                | pa == Curva Este  && a == x && b > y     = (x-1,y)
                | pa == Curva Sul   && a == x && b < y     = (x-1,y)
                | pa == Curva Sul   && a < x && b == y     = (x,y-1)
                | pa == Curva Oeste && a > x && b == y     = (x,y-1)
                | pa == Curva Oeste && a == x && b < y     = (x+1,y)
                | pa == Lava                                = (x,y)
                | otherwise                                 = (x,y)
                where 
                    Mapa ind t  = mapa e
                    (zzzz,yyyy) = posicao ((carros e)!!j)
                    (x,y)       = (truncate zzzz, truncate yyyy)
                    (a,b)      = daHead ((historico e)!!j)
                    Peca pa ha  = calcP t (x,y)

{-|
Determina se um tipo é uma rampa 

Exemplos: 

>>> isRampa (Rampa Norte)
True

>>> isRampa Recta 
False

>>> isRampa (Curva Norte)
False

-}
isRampa :: Tipo -> Bool
isRampa (Rampa _)    = True
isRampa _            = False

-- | Calcula a posição do carro, depois de aplicar a velocidade do mesmo no tempo dado
posAtualiza :: Tempo -> Jogo -> Int -> Ponto
posAtualiza tick e j    = (x+(v1*tick), y+(v2*tick))
                        where 
                            (x,y)   = posicao ((carros e)!!j)
                            new     = atualiza tick e j Acao{acelerar = False, travar = False, direita = False, esquerda = False, nitro = Nothing}
                            (v1,v2) = velocidade ((carros new)!!j)

-- | Determina se a ação ideal, de acordo com as condições em que o carro em questão se encontra, necessita acelerar
needGas :: Tempo -> Jogo -> Int -> Bool
needGas tick e j    | (v1,v2) == (0,0)              = True
                    | (pa `elem` tC || ps `elem` tC || s1 `elem` tC) && curvaDirEsq tick e j /= Nd  = False
                    | ha < hs && not (isRampa pa)   = False
                    | ps == Lava                    = False
                    | ha > hs && not (isRampa ps)   = False 
                    | s1 == Lava                    = False
                    | caiCurva tick e j             = False
                    | otherwise                     = True 
                    where
                        tC                  = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                        (a,p)               = posAtualiza tick e j
                        (v1,v2)             = velocidade ((carros e)!!j)
                        (x,y)               = posicao ((carros e)!!j)
                        Mapa ind t          = mapa e
                        (ww,xx)             = (truncate a, truncate p)
                        Peca ps hs          = calcP t (ww,xx)
                        Peca pa ha          = t !! truncate y !! truncate x
                        new                 = atualiza tick e j Acao{acelerar = True, travar = False, direita = False, esquerda = False, nitro = Nothing}
                        (vv,zz)             = velocidade ((carros new)!!j)
                        (vv',zz')           = (x+(vv*tick), y+(zz*tick))
                        Peca s1 x1          = t !! truncate zz' !! truncate vv'

-- | Determina se a ação ideal, de acordo com as condições em que o carro em questão se encontra, necessita travar
needBrakes :: Tempo -> Jogo -> Int -> Bool
needBrakes tick e j | needGas tick e j              = False
                    | (v1,v2) == (0,0)              = False
                    | ha < hs && not (isRampa pa)   = True 
                    | ha > hs && not (isRampa ps)   = True 
                    | ps == Lava                    = True 
                    | caiCurva tick e j             = True
                    | otherwise                     = False
                    where
                        tC                  = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                        (a,p)               = posAtualiza tick e j
                        (v1,v2)             = velocidade ((carros e)!!j)
                        (x,y)               = posicao ((carros e)!!j)
                        Mapa ind t          = mapa e
                        (ww,xx)             = (truncate a, truncate p)
                        Peca ps hs          = calcP t (ww,xx)
                        Peca pa ha          = t !! truncate y !! truncate x
                        new                 = atualiza tick e j Acao{acelerar = False, travar = True, direita = False, esquerda = False, nitro = Nothing}
                        (vv,zz)             = velocidade ((carros new)!!j)
                        (vv',zz')           = (x+(vv*tick), y+(zz*tick))
                        Peca s1 x1          = t !! truncate zz' !! truncate vv'

-- | Determina se a ação ideal, de acordo com as condições em que o carro em questão se encontra, necessita rodar à direita
needDir :: Tempo -> Jogo -> Int -> Bool
needDir tick e j    | curvaDirEsq tick e j == Dir                   = True
                    | (aa,bb) == (x'+1,y') && (ww,xx) == (x',y'-1)  = True
                    | (aa,bb) == (x'-1,y') && (ww,xx) == (x',y'+1)  = True
                    | (aa,bb) == (x',y'-1) && (ww,xx) == (x'-1,y')  = True
                    | (aa,bb) == (x',y'+1) && (ww,xx) == (x'+1,y')  = True 
                    | (aa,bb) == (x'+1,y') && pa == Recta && l o < 180 && l o /= 0 = True
                    | (aa,bb) == (x'-1,y') && pa == Recta && l o > 180 = True
                    | (aa,bb) == (x',y'-1) && pa == Recta && l o > 90 && l o < 270 = True
                    | (aa,bb) == (x',y'+1) && pa == Recta && l o < 90 && l o > 270  = True 
                    | curvaDirEsq tick e j == Nd                    = False 
                    | otherwise                                     = False
                    where
                        tC                  = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                        new                 = atualiza tick e j Acao{acelerar = False, travar = False, direita = True, esquerda = False, nitro = Nothing}
                        (vv,zz)             = velocidade ((carros new)!!j)
                        (vv',zz')           = (x+(vv*tick), y+(zz*tick))
                        (vvv,zzz)           = (truncate vv', truncate zz')
                        Peca s1 x1          = t !! truncate zz' !! truncate vv'
                        (a,p)               = posAtualiza tick e j
                        (v1,v2)             = velocidade ((carros e)!!j)
                        (x,y)               = posicao ((carros e)!!j)
                        (x',y')             = (truncate x, truncate y)
                        Mapa ind t          = mapa e
                        (ww,xx)             = (fromIntegral (truncate a),fromIntegral (truncate p))
                        Peca ps hs          = calcP t (ww, xx)
                        Peca pa ha          = t !! truncate y !! truncate x
                        (aa,bb)             = posSeg tick e j
                        o                   = direcao ((carros e)!!j)

-- | Determina se a ação ideal, de acordo com as condições em que o carro em questão se encontra, necessita rodar à esquerda                                        
needEsq :: Tempo -> Jogo -> Int -> Bool
needEsq tick e j    | curvaDirEsq tick e j == Esq                   = True
                    | (ww,xx) == (x'+1,y') && (aa,bb) == (x',y'-1)  = True
                    | (ww,xx) == (x'-1,y') && (aa,bb) == (x',y'+1)  = True
                    | (ww,xx) == (x',y'-1) && (aa,bb) == (x'-1,y')  = True
                    | (ww,xx) == (x',y'+1) && (aa,bb) == (x'+1,y')  = True  
                    | (aa,bb) == (x'+1,y') && pa == Recta && l o > 180 && l o /= 0 = True
                    | (aa,bb) == (x'-1,y') && pa == Recta && l o < 180 = True
                    | (aa,bb) == (x',y'-1) && pa == Recta && l o < 90 && l o > 270 = True
                    | (aa,bb) == (x',y'+1) && pa == Recta && l o > 90 && l o < 270  = True 
                    | curvaDirEsq tick e j == Nd                    = False 
                    | otherwise                                     = False
                    where
                        tC                  = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                        new                 = atualiza tick e j Acao{acelerar = False, travar = False, direita = False, esquerda = True, nitro = Nothing}
                        (vv,zz)             = velocidade ((carros new)!!j)
                        (vv',zz')           = (x+(vv*tick), y+(zz*tick))
                        (vvv,zzz)           = (truncate vv', truncate zz')
                        (a,p)               = posAtualiza tick e j
                        (v1,v2)             = velocidade ((carros e)!!j)
                        (x,y)               = posicao ((carros e)!!j)
                        (x',y')             = (truncate x, truncate y)
                        Mapa ind t          = mapa e
                        (ww,xx)             = (fromIntegral (truncate a),fromIntegral (truncate p))
                        Peca ps hs          = calcP t (ww, xx)
                        Peca pa ha          = t !! truncate y !! truncate x
                        (aa,bb)             = posSeg tick e j
                        Peca s1 x1          = t !! truncate zz' !! truncate vv'
                        o                   = direcao ((carros e)!!j)

-- | Determina se a ação ideal, de acordo com as condições em que o carro em questão se encontra, necessitaDir nitro
needNitro :: Tempo -> Jogo -> Int -> Maybe Int
needNitro tick e j  | length hist > 1 && posSeg tick e j `elem` tail hist    = Nothing 
                    | ps `elem` tC                              = Nothing
                    | s1 `elem` tC                              = Nothing
                    | ha < 0            && hs < 0               = Just j
                    | s1 == Lava                                = Nothing
                    | ps == Lava                                = Nothing
                    | curvaDirEsq tick e j /= Nd                = Nothing
                    | pa == Recta       && px == Recta          = Just j 
                    | pa == Curva Este  && px == Curva Oeste    = Just j 
                    | pa == Curva Norte && px == Curva Sul      = Just j
                    | pa == Curva Oeste && px == Curva Este     = Just j 
                    | pa == Curva Sul   && px == Curva Norte    = Just j
                    | isRampa pa        && isRampa px           = Just j
                    | otherwise                                 = Nothing
                    where
                        hist                = historico e !! j
                        tC                  = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                        (a,p)               = posAtualiza tick e j
                        (v1,v2)             = velocidade ((carros e)!!j)
                        (x,y)               = posicao ((carros e)!!j)
                        Mapa ind t          = mapa e
                        (ww,xx)             = (truncate a, truncate p)
                        Peca px hx          = pSeg tick e j 
                        Peca ps hs          = calcP t (ww,xx)
                        Peca pa ha          = t !! truncate y !! truncate x
                        new                 = atualiza tick e j Acao{acelerar = True, travar = False, direita = False, esquerda = False, nitro = Just j}
                        (vv,zz)             = velocidade ((carros new)!!j)
                        (vv',zz')           = (x+(vv*tick), y+(zz*tick))
                        Peca s1 x1          = t !! truncate zz' !! truncate vv'

-- | Determina se é vantajoso aplicar nitro nos outros carros do jogo
daNitro :: Tempo -> Jogo -> Int -> Maybe Int
daNitro tick e j    | length (carros e) == 1                    = Nothing
                    | otherwise                                 = estaNaCurva (length (carros e))
                    where
                            tC              = [Curva Norte, Curva Sul, Curva Este, Curva Oeste]
                            estaNaCurva x   | i <= 0                                                                = Nothing
                                            | i /= j && daTipo (peca (i-1)) == Lava && ha >= 0                      = Just i
                                            | i /= j && ha > daAlt (peca (i-1)) && not (isRampa (daTipo (peca i)))  = Just i
                                            | otherwise                                                             = estaNaCurva (x-1)
                                            where
                                                Peca pa ha      = calcP t (pos i)
                                                i = x-1
                            
                            new i               = atualiza tick e j Acao{acelerar = False, travar = False, direita = False, esquerda = False, nitro = Just i}
                            pos i               = truncaPos (posicao ((carros e)!!i))
                            vel i               = velocidade ((carros (new i))!!i)
                            peca i              = calcP t (truncate (a+(v*tick)),truncate (b+(h*tick)))
                                                where 
                                                    (a,b) = vel i
                                                    (v,h) = posicao ((carros e)!!i)
                            
                            Mapa ind t          = mapa e 

-- | Tipos de possivel alterações à direção do carro
data Dir 
    -- | Virar à esquerda
    = Esq 
    -- | Virar à direita
    | Dir
    -- | Não Virar
    | Nd 
         deriving (Eq,Show)

-- | Determina se, perante uma curva, é necessário virar à esquerda ou à direita, ou se a direção do carro já é apropriada
curvaDirEsq :: Tempo -> Jogo -> Int -> Dir
curvaDirEsq tick e j    | (null hist || null (historico e) || length hist == 1) && pa == Curva Este && l o < 275 && l o > 265                   = Nd
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Sul  && l o < 95  && l o > 85                    = Nd
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Este && vel == (0,0) && l o < 275 && l o > 265   = Nd
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Sul  && vel == (0,0) && l o < 95  && l o > 85    = Nd

                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Este                     = Dir
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Sul                      = Esq
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Este && vel == (0,0)     = Dir
                        | (null hist || null (historico e) || length hist == 1) && pa == Curva Sul  && vel == (0,0)     = Esq

                        | pa == Curva Este  && ps == Curva Oeste && l o > 305 && l o < 320  = Nd
                        | pa == Curva Oeste && ps == Curva Este  && l o > 305  && l o < 320 = Nd
                        | pa == Curva Sul   && ps == Curva Norte && l o > 40 && l o < 50    = Nd
                        | pa == Curva Norte && ps == Curva Sul   && l o > 40 && l o < 50    = Nd

                        | pa == Curva Este  && ultPos  == (x'-1,y') && l o < 275 && l o > 265   = Nd
                        | pa == Curva Este  && ultPos  == (x',y'+1) && l o < 185 && l o > 175   = Nd
                        | pa == Curva Sul   && ultPos  == (x',y'-1) && l o < 185 && l o > 175   = Nd
                        | pa == Curva Sul   && ultPos  == (x'-1,y') && l o < 95  && l o > 85    = Nd
                        | pa == Curva Oeste && ultPos  == (x'+1,y') && l o < 95  && l o > 85    = Nd
                        | pa == Curva Oeste && ultPos  == (x',y'-1) && l o < 5   && l o > 355   = Nd                  
                        | pa == Curva Norte && ultPos  == (x',y'+1) && l o < 5   && l o > 355   = Nd
                        | pa == Curva Norte && ultPos  == (x'+1,y') && l o < 275 && l o > 265   = Nd
                        
                        | not (isRampa pa)  && ps == Curva Este  && nextPos == (x'+1,y') && vel == (0,0)     = Dir
                        | not (isRampa pa)  && ps == Curva Este  && nextPos == (x',y'-1) && vel == (0,0)     = Esq
                        | not (isRampa pa)  && ps == Curva Sul   && nextPos == (x',y'+1) && vel == (0,0)     = Dir
                        | not (isRampa pa)  && ps == Curva Sul   && nextPos == (x'+1,y') && vel == (0,0)     = Esq
                        | not (isRampa pa)  && ps == Curva Oeste && nextPos == (x'-1,y') && vel == (0,0)     = Dir
                        | not (isRampa pa)  && ps == Curva Oeste && nextPos == (x',y'+1) && vel == (0,0)     = Esq
                        | not (isRampa pa)  && ps == Curva Norte && nextPos == (x',y'-1) && vel == (0,0)     = Dir
                        | not (isRampa pa)  && ps == Curva Norte && nextPos == (x'-1,y') && vel == (0,0)     = Esq

                        | not (isRampa pa)  && ps == Curva Este  && nextPos == (x'+1,y')                     = Dir
                        | not (isRampa pa)  && ps == Curva Este  && nextPos == (x',y'-1)                     = Esq
                        | not (isRampa pa)  && ps == Curva Sul   && nextPos == (x',y'+1)                     = Dir
                        | not (isRampa pa)  && ps == Curva Sul   && nextPos == (x'+1,y')                     = Esq
                        | not (isRampa pa)  && ps == Curva Oeste && nextPos == (x'-1,y')                     = Dir
                        | not (isRampa pa)  && ps == Curva Oeste && nextPos == (x',y'+1)                     = Esq
                        | not (isRampa pa)  && ps == Curva Norte && nextPos == (x',y'-1)                     = Dir
                        | not (isRampa pa)  && ps == Curva Norte && nextPos == (x'-1,y')                     = Esq
                        
                        | pa == Curva Este  && ultPos  == (x'-1,y')                     = Dir
                        | pa == Curva Este  && ultPos  == (x',y'+1)                     = Esq
                        | pa == Curva Sul   && ultPos  == (x',y'-1)                     = Dir
                        | pa == Curva Sul   && ultPos  == (x'-1,y')                     = Esq
                        | pa == Curva Oeste && ultPos  == (x'+1,y')                     = Dir
                        | pa == Curva Oeste && ultPos  == (x',y'-1)                     = Esq
                        | pa == Curva Norte && ultPos  == (x',y'+1)                     = Dir
                        | pa == Curva Norte && ultPos  == (x'+1,y')                     = Esq 
                        
                        | pa == Curva Este  && ultPos  == (x'-1,y') && vel == (0,0)     = Dir
                        | pa == Curva Este  && ultPos  == (x',y'+1) && vel == (0,0)     = Esq
                        | pa == Curva Sul   && ultPos  == (x',y'-1) && vel == (0,0)     = Dir
                        | pa == Curva Sul   && ultPos  == (x'+1,y') && vel == (0,0)     = Esq
                        | pa == Curva Oeste && ultPos  == (x'+1,y') && vel == (0,0)     = Dir
                        | pa == Curva Oeste && ultPos  == (x',y'-1) && vel == (0,0)     = Esq
                        | pa == Curva Norte && ultPos  == (x',y'+1) && vel == (0,0)     = Dir
                        | pa == Curva Norte && ultPos  == (x'+1,y') && vel == (0,0)     = Esq 

                        | otherwise                                                     = dirIdeal tick e j
                        where 
                            Peca ps hs  = pSeg tick e j
                            (x,y)       = posicao ((carros e)!!j)
                            (x',y')     = (truncate x, truncate y)
                            nextPos     = posSeg tick e j
                            Peca pa ha  = t !! truncate y !! truncate x
                            hist        = historico e !! j
                            ultPos      = daHead hist
                            Mapa ind t  = mapa e
                            vel         = velocidade ((carros e)!!j)
                            o           = direcao ((carros e)!!j)
 
-- | Determina se é necessário virar à direita ou à esquerda, para corrigir a direção do carro se necessário
dirIdeal :: Tempo -> Jogo -> Int -> Dir
dirIdeal tick e j   | nextPos == (x+1,y) && l o == 0                            = Nd
                    | nextPos == (x+1,y) && l o <= 180                          = Dir 
                    | nextPos == (x+1,y) && l o > 180                           = Esq 
                    | nextPos == (x-1,y) && l o == 180                          = Nd
                    | nextPos == (x-1,y) && l o <  180                          = Esq 
                    | nextPos == (x-1,y) && l o > 180                           = Dir 
                    | nextPos == (x,y-1) && l o == 90                           = Nd
                    | nextPos == (x,y-1) && l o > 90 && l o <= 270              = Dir
                    | nextPos == (x,y-1) && l o < 90 && l o >= 0 && l o > 270   = Esq
                    | nextPos == (x,y+1) && l o == 270                          = Nd
                    | nextPos == (x,y+1) && l o > 90 && l o >  270              = Esq
                    | nextPos == (x,y+1) && l o < 90 && l o >= 0 && l o > 270   = Dir
                    | otherwise                                                 = Nd
                    where 
                        Mapa ind t  = mapa e 
                        (a',b')       = head hist 
                        Peca a b    = calcP t (head (tail hist))
                        hist        = historico e !! j
                        nextPos     = posSeg tick e j 
                        (x,y)       = truncaPos (posicao ((carros e)!!j))
                        o           = direcao ((carros e)!!j)

-- | Determina se o carro passa para a parte da lava numa curva
caiCurva :: Tempo -> Jogo -> Int -> Bool
caiCurva tick e j   | pa == Curva Este  && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y') = True
                    | pa == Curva Oeste && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y') = True
                    | pa == Curva Norte && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y') = True
                    | pa == Curva Sul   && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y') = True
                    
                    | pa == Curva Este  && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | pa == Curva Oeste && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | pa == Curva Norte && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | pa == Curva Sul   && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    
                    | ps == Curva Este  && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a',fromIntegral b'),(fromIntegral (a'+1),fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y')  = True
                    | ps == Curva Oeste && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral a',fromIntegral b'),(fromIntegral (a'+1),fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (x',y')  = True
                    | ps == Curva Norte && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a'+1),fromIntegral b'),(fromIntegral a',fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y')  = True
                    | ps == Curva Sul   && isJust    (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a'+1),fromIntegral b'),(fromIntegral a',fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(x',y')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (x',y')  = True
                    
                    | ps == Curva Este  && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a',fromIntegral b'),(fromIntegral (a'+1),fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | ps == Curva Oeste && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a',fromIntegral b'),(fromIntegral (a'+1),fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral a,fromIntegral b),(fromIntegral (a+1),fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | ps == Curva Norte && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a'+1),fromIntegral b'),(fromIntegral a',fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | ps == Curva Sul   && isJust    (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a'+1),fromIntegral b'),(fromIntegral a',fromIntegral (b'+1)))) && dist (x,y) (fromJust (maybeIntersection ((x,y),(vv',zz')) ((fromIntegral (a+1),fromIntegral b),(fromIntegral a,fromIntegral (b+1))))) < dist (x,y) (vv',zz') = True
                    | otherwise                                                                             = False 
                    where
                        Mapa ind t  = mapa e
                        Peca pa ha  = calcP t (a,b) 
                        (x,y)       = posicao ((carros e)!!j)
                        (x',y')     = posAtualiza tick e j    
                        (a,b)       = truncaPos (x,y)   
                        new         = atualiza tick e j Acao{acelerar = True, travar = False, direita = False, esquerda = False, nitro = Just j}
                        (vv,zz)     = velocidade ((carros new)!!j)
                        (vv',zz')   = (x+(vv*tick), y+(zz*tick))
                        (a',b')     = truncaPos (x',y')
                        Peca ps hs  = calcP t (a',b')  

{-|
Dada uma peça, devolve o seu tipo

Exemplos: 

>>> daTipo (Peca Recta 0)
Recta

>>> daTipo (Peca (Curva Norte) (-3))
Curva Norte 

-}
daTipo :: Peca -> Tipo
daTipo a    = tP
            where
                Peca tP hP = a   

{-|
Dada uma peça, devolve a sua altura

Exemplos: 

>>> daAlt (Peca (Rampa Norte) 7)
7

>>> daAlt (Peca (Curva Este) (-3))
-3 

-}
daAlt :: Peca -> Altura
daAlt a     = hP
            where 
                Peca tP hP = a

-- | Calcula a distancia entre dois pontos
dist :: Ponto -> Ponto -> Double
dist (x,y) (a,b)    = sqrt ((a-x)^2 + (b-y)^2)

{-|
Dado um ponto, devolve uma posição

Exemplos: 

>>> truncaPos ((-35.8), 10.9)
(-35,10)

>>> truncaPos (28, 8.3)
(28,3)

-}
truncaPos :: Ponto -> Posicao 
truncaPos (a,b) = (truncate a, truncate b)

-- | Calcula o ponto entre duas rectas formadas por um par (Ponto,Ponto) caso exista, devolvendo Nothing caso não exista
maybeIntersection :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
maybeIntersection ((x1, y1), (x2, y2)) ((a1, b1), (a2, b2)) | d ==  0 = Nothing
                                                            | otherwise = Just (h aX xX, h aY xY)
                                                            where
                                                                (aX, xX) = (a1 - a2, x1 - x2)
                                                                (aY, xY) = (b1 - b2, y1 - y2)
                                                                d = xX * aY - xY * aX
                                                                h xy ab = ((((x1 * y2) - (y1 * x2)) * xy) - (((a1 * b2) - (b1 * a2)) * ab)) /d

-- | Carro usado para teste
c111  = Carro {posicao = (2,1.5), direcao = 0, velocidade = (0,0)}

-- | Jogo usado para teste
j111  = Jogo m1 p1 [c111,c1] [5] [[(2,1)]]