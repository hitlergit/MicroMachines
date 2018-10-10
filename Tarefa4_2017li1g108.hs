{-|
Module      : Tarefa4_2017li1g108
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g108 where

import LI11718
import Data.Maybe
import Tarefa1_2017li1g108

-- 282 testes funcionar
-- | Tipo criado para facilitar a orientacação e o controlo sobre as funções
type VelocidadePolar = (Double,Angulo)

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(1/60,j1,a1), (1/60,j1,a2), (1/60,j1,a3), (1/60,j1,a4), (1/60,j1,a5), (1/60,j1,a6), (1/60,j1,a7), (1/60,j1,a8), (1/60,j1,a9), (1/60,j1,a10), (1/60,j1,a11),
            (1/60,j2,a1), (1/60,j2,a2), (1/60,j2,a3), (1/60,j2,a4), (1/60,j2,a5), (1/60,j2,a6), (1/60,j2,a7), (1/60,j2,a8), (1/60,j2,a9), (1/60,j2,a10), (1/60,j2,a11),
            (1/60,j3,a1), (1/60,j3,a2), (1/60,j3,a3), (1/60,j3,a4), (1/60,j3,a5), (1/60,j3,a6), (1/60,j3,a7), (1/60,j3,a8), (1/60,j3,a9), (1/60,j3,a10), (1/60,j3,a11),
            (1/60,j4,a1), (1/60,j4,a2), (1/60,j4,a3), (1/60,j4,a4), (1/60,j4,a5), (1/60,j4,a6), (1/60,j4,a7), (1/60,j4,a8), (1/60,j4,a9), (1/60,j4,a10), (1/60,j4,a11),
            (1/60,j5,a1), (1/60,j5,a2), (1/60,j5,a3), (1/60,j5,a4), (1/60,j5,a5), (1/60,j5,a6), (1/60,j5,a7), (1/60,j5,a8), (1/60,j5,a9), (1/60,j5,a10), (1/60,j5,a11),
            (1/60,j6,a1), (1/60,j6,a2), (1/60,j6,a3), (1/60,j6,a4), (1/60,j6,a5), (1/60,j6,a6), (1/60,j6,a7), (1/60,j6,a8), (1/60,j6,a9), (1/60,j6,a10), (1/60,j6,a11),
            (1/60,j7,a1), (1/60,j7,a2), (1/60,j7,a3), (1/60,j7,a4), (1/60,j7,a5), (1/60,j7,a6), (1/60,j7,a7), (1/60,j7,a8), (1/60,j7,a9), (1/60,j7,a10), (1/60,j7,a11),
            (1/60,j8,a1), (1/60,j8,a2), (1/60,j8,a3), (1/60,j8,a4), (1/60,j8,a5), (1/60,j8,a6), (1/60,j8,a7), (1/60,j8,a8), (1/60,j8,a9), (1/60,j8,a10), (1/60,j8,a11),
            (1/60,j9,a1), (1/60,j9,a2), (1/60,j9,a3), (1/60,j9,a4), (1/60,j9,a5), (1/60,j9,a6), (1/60,j9,a7), (1/60,j9,a8), (1/60,j9,a9), (1/60,j9,a10), (1/60,j9,a11),
            (1/60,j10,a1),(1/60,j10,a2),(1/60,j10,a3),(1/60,j10,a4),(1/60,j10,a5),(1/60,j10,a6),(1/60,j10,a7),(1/60,j10,a8),(1/60,j10,a9),(1/60,j10,a10),(1/60,j10,a11),
            (1/60,j11,a1),(1/60,j11,a2),(1/60,j11,a3),(1/60,j11,a4),(1/60,j11,a5),(1/60,j11,a6),(1/60,j11,a7),(1/60,j11,a8),(1/60,j11,a9),(1/60,j11,a10),(1/60,j11,a11),
            (1/60,j12,a1),(1/60,j12,a2),(1/60,j12,a3),(1/60,j12,a4),(1/60,j12,a5),(1/60,j12,a6),(1/60,j12,a7),(1/60,j12,a8),(1/60,j12,a9),(1/60,j12,a10),(1/60,j12,a11),
            (1/60,j13,a1),(1/60,j13,a2),(1/60,j13,a3),(1/60,j13,a4),(1/60,j13,a5),(1/60,j13,a6),(1/60,j13,a7),(1/60,j13,a8),(1/60,j13,a9),(1/60,j13,a10),(1/60,j13,a11),
            (1/60,j14,a1),(1/60,j14,a2),(1/60,j14,a3),(1/60,j14,a4),(1/60,j14,a5),(1/60,j14,a6),(1/60,j14,a7),(1/60,j14,a8),(1/60,j14,a9),(1/60,j14,a10),(1/60,j14,a11),
            (1/60,j15,a1),(1/60,j15,a2),(1/60,j15,a3),(1/60,j15,a4),(1/60,j15,a5),(1/60,j15,a6),(1/60,j15,a7),(1/60,j15,a8),(1/60,j15,a9),(1/60,j15,a10),(1/60,j15,a11),
            (1/60,j16,a1),(1/60,j16,a2),(1/60,j16,a3),(1/60,j16,a4),(1/60,j16,a5),(1/60,j16,a6),(1/60,j16,a7),(1/60,j16,a8),(1/60,j16,a9),(1/60,j16,a10),(1/60,j16,a11),
            (1/60,j17,a1),(1/60,j17,a2),(1/60,j17,a3),(1/60,j17,a4),(1/60,j17,a5),(1/60,j17,a6),(1/60,j17,a7),(1/60,j17,a8),(1/60,j17,a9),(1/60,j17,a10),(1/60,j17,a11),
            (1/60,j18,a1),(1/60,j18,a2),(1/60,j18,a3),(1/60,j18,a4),(1/60,j18,a5),(1/60,j18,a6),(1/60,j18,a7),(1/60,j18,a8),(1/60,j18,a9),(1/60,j18,a10),(1/60,j18,a11),
            (1/60,j19,a1),(1/60,j19,a2),(1/60,j19,a3),(1/60,j19,a4),(1/60,j19,a5),(1/60,j19,a6),(1/60,j19,a7),(1/60,j19,a8),(1/60,j19,a9),(1/60,j19,a10),(1/60,j19,a11),
            (1/60,j20,a1),(1/60,j20,a2),(1/60,j20,a3),(1/60,j20,a4),(1/60,j20,a5),(1/60,j20,a6),(1/60,j20,a7),(1/60,j20,a8),(1/60,j20,a9),(1/60,j20,a10),(1/60,j20,a11),
            (1/60,j21,a1),(1/60,j21,a2),(1/60,j21,a3),(1/60,j21,a4),(1/60,j21,a5),(1/60,j21,a6),(1/60,j21,a7),(1/60,j21,a8),(1/60,j21,a9),(1/60,j21,a10),(1/60,j21,a11),
            (1/60,j22,a1),(1/60,j22,a2),(1/60,j22,a3),(1/60,j22,a4),(1/60,j22,a5),(1/60,j22,a6),(1/60,j22,a7),(1/60,j22,a8),(1/60,j22,a9),(1/60,j22,a10),(1/60,j22,a11),
            (1/60,j23,a1),(1/60,j23,a2),(1/60,j23,a3),(1/60,j23,a4),(1/60,j23,a5),(1/60,j23,a6),(1/60,j23,a7),(1/60,j23,a8),(1/60,j23,a9),(1/60,j23,a10),(1/60,j23,a11),
            (1/60,j24,a1),(1/60,j24,a2),(1/60,j24,a3),(1/60,j24,a4),(1/60,j24,a5),(1/60,j24,a6),(1/60,j24,a7),(1/60,j24,a8),(1/60,j24,a9),(1/60,j24,a10),(1/60,j24,a11),
            (1/60,j25,a1),(1/60,j25,a2),(1/60,j25,a3),(1/60,j25,a4),(1/60,j25,a5),(1/60,j25,a6),(1/60,j25,a7),(1/60,j25,a8),(1/60,j25,a9),(1/60,j25,a10),(1/60,j25,a11),
            (1/60,j26,a1),(1/60,j26,a2),(1/60,j26,a3),(1/60,j26,a4),(1/60,j26,a5),(1/60,j26,a6),(1/60,j26,a7),(1/60,j26,a8),(1/60,j26,a9),(1/60,j26,a10),(1/60,j26,a11),
            (1/60,j27,a1),(1/60,j27,a2),(1/60,j27,a3),(1/60,j27,a4),(1/60,j27,a5),(1/60,j27,a6),(1/60,j27,a7),(1/60,j27,a8),(1/60,j27,a9),(1/60,j27,a10),(1/60,j27,a11),
            (1/60,j28,a1),(1/60,j28,a2),(1/60,j28,a3),(1/60,j28,a4),(1/60,j28,a5),(1/60,j28,a6),(1/60,j28,a7),(1/60,j28,a8),(1/60,j28,a9),(1/60,j28,a10),(1/60,j28,a11),
            (1/60,j29,a1),(1/60,j29,a2),(1/60,j29,a3),(1/60,j29,a4),(1/60,j29,a5),(1/60,j29,a6),(1/60,j29,a7),(1/60,j29,a8),(1/60,j29,a9),(1/60,j29,a10),(1/60,j29,a11),
            (1/60,j30,a1),(1/60,j30,a2),(1/60,j30,a3),(1/60,j30,a4),(1/60,j30,a5),(1/60,j30,a6),(1/60,j30,a7),(1/60,j30,a8),(1/60,j30,a9),(1/60,j30,a10),(1/60,j30,a11),
            (1/60,j31,a1),(1/60,j31,a2),(1/60,j31,a3),(1/60,j31,a4),(1/60,j31,a5),(1/60,j31,a6),(1/60,j31,a7),(1/60,j31,a8),(1/60,j31,a9),(1/60,j31,a10),(1/60,j31,a11),
            (1/60,j32,a1),(1/60,j32,a2),(1/60,j32,a3),(1/60,j32,a4),(1/60,j32,a5),(1/60,j32,a6),(1/60,j32,a7),(1/60,j32,a8),(1/60,j32,a9),(1/60,j32,a10),(1/60,j32,a11),
            (2,j1,a1), (2,j1,a2), (2,j1,a3), (2,j1,a4), (2,j1,a5), (2,j1,a6), (2,j1,a7), (2,j1,a8), (2,j1,a9), (2,j1,a10), (2,j1,a11),
            (2,j2,a1), (2,j2,a2), (2,j2,a3), (2,j2,a4), (2,j2,a5), (2,j2,a6), (2,j2,a7), (2,j2,a8), (2,j2,a9), (2,j2,a10), (2,j2,a11),
            (2,j3,a1), (2,j3,a2), (2,j3,a3), (2,j3,a4), (2,j3,a5), (2,j3,a6), (2,j3,a7), (2,j3,a8), (2,j3,a9), (2,j3,a10), (2,j3,a11),
            (2,j4,a1), (2,j4,a2), (2,j4,a3), (2,j4,a4), (2,j4,a5), (2,j4,a6), (2,j4,a7), (2,j4,a8), (2,j4,a9), (2,j4,a10), (2,j4,a11),
            (2,j5,a1), (2,j5,a2), (2,j5,a3), (2,j5,a4), (2,j5,a5), (2,j5,a6), (2,j5,a7), (2,j5,a8), (2,j5,a9), (2,j5,a10), (2,j5,a11),
            (2,j6,a1), (2,j6,a2), (2,j6,a3), (2,j6,a4), (2,j6,a5), (2,j6,a6), (2,j6,a7), (2,j6,a8), (2,j6,a9), (2,j6,a10), (2,j6,a11),
            (2,j7,a1), (2,j7,a2), (2,j7,a3), (2,j7,a4), (2,j7,a5), (2,j7,a6), (2,j7,a7), (2,j7,a8), (2,j7,a9), (2,j7,a10), (2,j7,a11),
            (2,j8,a1), (2,j8,a2), (2,j8,a3), (2,j8,a4), (2,j8,a5), (2,j8,a6), (2,j8,a7), (2,j8,a8), (2,j8,a9), (2,j8,a10), (2,j8,a11),
            (2,j9,a1), (2,j9,a2), (2,j9,a3), (2,j9,a4), (2,j9,a5), (2,j9,a6), (2,j9,a7), (2,j9,a8), (2,j9,a9), (2,j9,a10), (2,j9,a11),
            (2,j10,a1),(2,j10,a2),(2,j10,a3),(2,j10,a4),(2,j10,a5),(2,j10,a6),(2,j10,a7),(2,j10,a8),(2,j10,a9),(2,j10,a10),(2,j10,a11),
            (2,j11,a1),(2,j11,a2),(2,j11,a3),(2,j11,a4),(2,j11,a5),(2,j11,a6),(2,j11,a7),(2,j11,a8),(2,j11,a9),(2,j11,a10),(2,j11,a11),
            (2,j12,a1),(2,j12,a2),(2,j12,a3),(2,j12,a4),(2,j12,a5),(2,j12,a6),(2,j12,a7),(2,j12,a8),(2,j12,a9),(2,j12,a10),(2,j12,a11),
            (2,j13,a1),(2,j13,a2),(2,j13,a3),(2,j13,a4),(2,j13,a5),(2,j13,a6),(2,j13,a7),(2,j13,a8),(2,j13,a9),(2,j13,a10),(2,j13,a11),
            (2,j14,a1),(2,j14,a2),(2,j14,a3),(2,j14,a4),(2,j14,a5),(2,j14,a6),(2,j14,a7),(2,j14,a8),(2,j14,a9),(2,j14,a10),(2,j14,a11),
            (2,j15,a1),(2,j15,a2),(2,j15,a3),(2,j15,a4),(2,j15,a5),(2,j15,a6),(2,j15,a7),(2,j15,a8),(2,j15,a9),(2,j15,a10),(2,j15,a11),
            (2,j16,a1),(2,j16,a2),(2,j16,a3),(2,j16,a4),(2,j16,a5),(2,j16,a6),(2,j16,a7),(2,j16,a8),(2,j16,a9),(2,j16,a10),(2,j16,a11),
            (2,j17,a1),(2,j17,a2),(2,j17,a3),(2,j17,a4),(2,j17,a5),(2,j17,a6),(2,j17,a7),(2,j17,a8),(2,j17,a9),(2,j17,a10),(2,j17,a11),
            (2,j18,a1),(2,j18,a2),(2,j18,a3),(2,j18,a4),(2,j18,a5),(2,j18,a6),(2,j18,a7),(2,j18,a8),(2,j18,a9),(2,j18,a10),(2,j18,a11),
            (2,j19,a1),(2,j19,a2),(2,j19,a3),(2,j19,a4),(2,j19,a5),(2,j19,a6),(2,j19,a7),(2,j19,a8),(2,j19,a9),(2,j19,a10),(2,j19,a11),
            (2,j20,a1),(2,j20,a2),(2,j20,a3),(2,j20,a4),(2,j20,a5),(2,j20,a6),(2,j20,a7),(2,j20,a8),(2,j20,a9),(2,j20,a10),(2,j20,a11),
            (2,j21,a1),(2,j21,a2),(2,j21,a3),(2,j21,a4),(2,j21,a5),(2,j21,a6),(2,j21,a7),(2,j21,a8),(2,j21,a9),(2,j21,a10),(2,j21,a11),
            (2,j22,a1),(2,j22,a2),(2,j22,a3),(2,j22,a4),(2,j22,a5),(2,j22,a6),(2,j22,a7),(2,j22,a8),(2,j22,a9),(2,j22,a10),(2,j22,a11),
            (2,j23,a1),(2,j23,a2),(2,j23,a3),(2,j23,a4),(2,j23,a5),(2,j23,a6),(2,j23,a7),(2,j23,a8),(2,j23,a9),(2,j23,a10),(2,j23,a11),
            (2,j24,a1),(2,j24,a2),(2,j24,a3),(2,j24,a4),(2,j24,a5),(2,j24,a6),(2,j24,a7),(2,j24,a8),(2,j24,a9),(2,j24,a10),(2,j24,a11),
            (2,j25,a1),(2,j25,a2),(2,j25,a3),(2,j25,a4),(2,j25,a5),(2,j25,a6),(2,j25,a7),(2,j25,a8),(2,j25,a9),(2,j25,a10),(2,j25,a11),
            (2,j26,a1),(2,j26,a2),(2,j26,a3),(2,j26,a4),(2,j26,a5),(2,j26,a6),(2,j26,a7),(2,j26,a8),(2,j26,a9),(2,j26,a10),(2,j26,a11),
            (2,j27,a1),(2,j27,a2),(2,j27,a3),(2,j27,a4),(2,j27,a5),(2,j27,a6),(2,j27,a7),(2,j27,a8),(2,j27,a9),(2,j27,a10),(2,j27,a11),
            (2,j28,a1),(2,j28,a2),(2,j28,a3),(2,j28,a4),(2,j28,a5),(2,j28,a6),(2,j28,a7),(2,j28,a8),(2,j28,a9),(2,j28,a10),(2,j28,a11),
            (2,j29,a1),(2,j29,a2),(2,j29,a3),(2,j29,a4),(2,j29,a5),(2,j29,a6),(2,j29,a7),(2,j29,a8),(2,j29,a9),(2,j29,a10),(2,j29,a11),
            (2,j30,a1),(2,j30,a2),(2,j30,a3),(2,j30,a4),(2,j30,a5),(2,j30,a6),(2,j30,a7),(2,j30,a8),(2,j30,a9),(2,j30,a10),(2,j30,a11),
            (2,j31,a1),(2,j31,a2),(2,j31,a3),(2,j31,a4),(2,j31,a5),(2,j31,a6),(2,j31,a7),(2,j31,a8),(2,j31,a9),(2,j31,a10),(2,j31,a11),
            (2,j32,a1),(2,j32,a2),(2,j32,a3),(2,j32,a4),(2,j32,a5),(2,j32,a6),(2,j32,a7),(2,j32,a8),(2,j32,a9),(2,j32,a10),(2,j32,a11),
            (0.5,j1,a1), (0.5,j1,a2), (0.5,j1,a3), (0.5,j1,a4), (0.5,j1,a5), (0.5,j1,a6), (0.5,j1,a7), (0.5,j1,a8), (0.5,j1,a9), (0.5,j1,a10), (0.5,j1,a11),
            (0.5,j2,a1), (0.5,j2,a2), (0.5,j2,a3), (0.5,j2,a4), (0.5,j2,a5), (0.5,j2,a6), (0.5,j2,a7), (0.5,j2,a8), (0.5,j2,a9), (0.5,j2,a10), (0.5,j2,a11),
            (0.5,j3,a1), (0.5,j3,a2), (0.5,j3,a3), (0.5,j3,a4), (0.5,j3,a5), (0.5,j3,a6), (0.5,j3,a7), (0.5,j3,a8), (0.5,j3,a9), (0.5,j3,a10), (0.5,j3,a11),
            (0.5,j4,a1), (0.5,j4,a2), (0.5,j4,a3), (0.5,j4,a4), (0.5,j4,a5), (0.5,j4,a6), (0.5,j4,a7), (0.5,j4,a8), (0.5,j4,a9), (0.5,j4,a10), (0.5,j4,a11),
            (0.5,j5,a1), (0.5,j5,a2), (0.5,j5,a3), (0.5,j5,a4), (0.5,j5,a5), (0.5,j5,a6), (0.5,j5,a7), (0.5,j5,a8), (0.5,j5,a9), (0.5,j5,a10), (0.5,j5,a11),
            (0.5,j6,a1), (0.5,j6,a2), (0.5,j6,a3), (0.5,j6,a4), (0.5,j6,a5), (0.5,j6,a6), (0.5,j6,a7), (0.5,j6,a8), (0.5,j6,a9), (0.5,j6,a10), (0.5,j6,a11),
            (0.5,j7,a1), (0.5,j7,a2), (0.5,j7,a3), (0.5,j7,a4), (0.5,j7,a5), (0.5,j7,a6), (0.5,j7,a7), (0.5,j7,a8), (0.5,j7,a9), (0.5,j7,a10), (0.5,j7,a11),
            (0.5,j8,a1), (0.5,j8,a2), (0.5,j8,a3), (0.5,j8,a4), (0.5,j8,a5), (0.5,j8,a6), (0.5,j8,a7), (0.5,j8,a8), (0.5,j8,a9), (0.5,j8,a10), (0.5,j8,a11),
            (0.5,j9,a1), (0.5,j9,a2), (0.5,j9,a3), (0.5,j9,a4), (0.5,j9,a5), (0.5,j9,a6), (0.5,j9,a7), (0.5,j9,a8), (0.5,j9,a9), (0.5,j9,a10), (0.5,j9,a11),
            (0.5,j10,a1),(0.5,j10,a2),(0.5,j10,a3),(0.5,j10,a4),(0.5,j10,a5),(0.5,j10,a6),(0.5,j10,a7),(0.5,j10,a8),(0.5,j10,a9),(0.5,j10,a10),(0.5,j10,a11),
            (0.5,j11,a1),(0.5,j11,a2),(0.5,j11,a3),(0.5,j11,a4),(0.5,j11,a5),(0.5,j11,a6),(0.5,j11,a7),(0.5,j11,a8),(0.5,j11,a9),(0.5,j11,a10),(0.5,j11,a11),
            (0.5,j12,a1),(0.5,j12,a2),(0.5,j12,a3),(0.5,j12,a4),(0.5,j12,a5),(0.5,j12,a6),(0.5,j12,a7),(0.5,j12,a8),(0.5,j12,a9),(0.5,j12,a10),(0.5,j12,a11),
            (0.5,j13,a1),(0.5,j13,a2),(0.5,j13,a3),(0.5,j13,a4),(0.5,j13,a5),(0.5,j13,a6),(0.5,j13,a7),(0.5,j13,a8),(0.5,j13,a9),(0.5,j13,a10),(0.5,j13,a11),
            (0.5,j14,a1),(0.5,j14,a2),(0.5,j14,a3),(0.5,j14,a4),(0.5,j14,a5),(0.5,j14,a6),(0.5,j14,a7),(0.5,j14,a8),(0.5,j14,a9),(0.5,j14,a10),(0.5,j14,a11),
            (0.5,j15,a1),(0.5,j15,a2),(0.5,j15,a3),(0.5,j15,a4),(0.5,j15,a5),(0.5,j15,a6),(0.5,j15,a7),(0.5,j15,a8),(0.5,j15,a9),(0.5,j15,a10),(0.5,j15,a11),
            (0.5,j16,a1),(0.5,j16,a2),(0.5,j16,a3),(0.5,j16,a4),(0.5,j16,a5),(0.5,j16,a6),(0.5,j16,a7),(0.5,j16,a8),(0.5,j16,a9),(0.5,j16,a10),(0.5,j16,a11),
            (0.5,j17,a1),(0.5,j17,a2),(0.5,j17,a3),(0.5,j17,a4),(0.5,j17,a5),(0.5,j17,a6),(0.5,j17,a7),(0.5,j17,a8),(0.5,j17,a9),(0.5,j17,a10),(0.5,j17,a11),
            (0.5,j18,a1),(0.5,j18,a2),(0.5,j18,a3),(0.5,j18,a4),(0.5,j18,a5),(0.5,j18,a6),(0.5,j18,a7),(0.5,j18,a8),(0.5,j18,a9),(0.5,j18,a10),(0.5,j18,a11),
            (0.5,j19,a1),(0.5,j19,a2),(0.5,j19,a3),(0.5,j19,a4),(0.5,j19,a5),(0.5,j19,a6),(0.5,j19,a7),(0.5,j19,a8),(0.5,j19,a9),(0.5,j19,a10),(0.5,j19,a11),
            (0.5,j20,a1),(0.5,j20,a2),(0.5,j20,a3),(0.5,j20,a4),(0.5,j20,a5),(0.5,j20,a6),(0.5,j20,a7),(0.5,j20,a8),(0.5,j20,a9),(0.5,j20,a10),(0.5,j20,a11),
            (0.5,j21,a1),(0.5,j21,a2),(0.5,j21,a3),(0.5,j21,a4),(0.5,j21,a5),(0.5,j21,a6),(0.5,j21,a7),(0.5,j21,a8),(0.5,j21,a9),(0.5,j21,a10),(0.5,j21,a11),
            (0.5,j22,a1),(0.5,j22,a2),(0.5,j22,a3),(0.5,j22,a4),(0.5,j22,a5),(0.5,j22,a6),(0.5,j22,a7),(0.5,j22,a8),(0.5,j22,a9),(0.5,j22,a10),(0.5,j22,a11),
            (0.5,j23,a1),(0.5,j23,a2),(0.5,j23,a3),(0.5,j23,a4),(0.5,j23,a5),(0.5,j23,a6),(0.5,j23,a7),(0.5,j23,a8),(0.5,j23,a9),(0.5,j23,a10),(0.5,j23,a11),
            (0.5,j24,a1),(0.5,j24,a2),(0.5,j24,a3),(0.5,j24,a4),(0.5,j24,a5),(0.5,j24,a6),(0.5,j24,a7),(0.5,j24,a8),(0.5,j24,a9),(0.5,j24,a10),(0.5,j24,a11),
            (0.5,j25,a1),(0.5,j25,a2),(0.5,j25,a3),(0.5,j25,a4),(0.5,j25,a5),(0.5,j25,a6),(0.5,j25,a7),(0.5,j25,a8),(0.5,j25,a9),(0.5,j25,a10),(0.5,j25,a11),
            (0.5,j26,a1),(0.5,j26,a2),(0.5,j26,a3),(0.5,j26,a4),(0.5,j26,a5),(0.5,j26,a6),(0.5,j26,a7),(0.5,j26,a8),(0.5,j26,a9),(0.5,j26,a10),(0.5,j26,a11),
            (0.5,j27,a1),(0.5,j27,a2),(0.5,j27,a3),(0.5,j27,a4),(0.5,j27,a5),(0.5,j27,a6),(0.5,j27,a7),(0.5,j27,a8),(0.5,j27,a9),(0.5,j27,a10),(0.5,j27,a11),
            (0.5,j28,a1),(0.5,j28,a2),(0.5,j28,a3),(0.5,j28,a4),(0.5,j28,a5),(0.5,j28,a6),(0.5,j28,a7),(0.5,j28,a8),(0.5,j28,a9),(0.5,j28,a10),(0.5,j28,a11),
            (0.5,j29,a1),(0.5,j29,a2),(0.5,j29,a3),(0.5,j29,a4),(0.5,j29,a5),(0.5,j29,a6),(0.5,j29,a7),(0.5,j29,a8),(0.5,j29,a9),(0.5,j29,a10),(0.5,j29,a11),
            (0.5,j30,a1),(0.5,j30,a2),(0.5,j30,a3),(0.5,j30,a4),(0.5,j30,a5),(0.5,j30,a6),(0.5,j30,a7),(0.5,j30,a8),(0.5,j30,a9),(0.5,j30,a10),(0.5,j30,a11),
            (0.5,j31,a1),(0.5,j31,a2),(0.5,j31,a3),(0.5,j31,a4),(0.5,j31,a5),(0.5,j31,a6),(0.5,j31,a7),(0.5,j31,a8),(0.5,j31,a9),(0.5,j31,a10),(0.5,j31,a11),
            (0.5,j32,a1),(0.5,j32,a2),(0.5,j32,a3),(0.5,j32,a4),(0.5,j32,a5),(0.5,j32,a6),(0.5,j32,a7),(0.5,j32,a8),(0.5,j32,a9),(0.5,j32,a10),(0.5,j32,a11)]

m1 = constroi [Avanca,Avanca,CurvaDir,Sobe,CurvaDir,Avanca,Avanca,CurvaDir,Desce,CurvaDir]
m2 = constroi [CurvaEsq,Avanca,Avanca,CurvaDir,Sobe,Sobe,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Avanca,Desce,CurvaDir,CurvaEsq,CurvaDir,CurvaDir]
m3 = constroi [Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Sobe,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaDir,Avanca,CurvaEsq,CurvaEsq,Avanca,Avanca]
m4 = constroi [Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Desce,CurvaDir,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Sobe,Desce,CurvaEsq,Desce,Sobe,CurvaDir,CurvaDir,Avanca,Avanca,Avanca]

p1 = Propriedades 2 3 4 2 15 180
p2 = Propriedades 3 2 5 3 18 265
p3 = Propriedades 1 7 8 1 6  90
p4 = Propriedades 3 4 5 3 16 125

a1  = Acao {acelerar = True,  travar = True,  direita = True,  esquerda = True,  nitro = Nothing }
a2  = Acao {acelerar = True,  travar = False, direita = True,  esquerda = False, nitro = Just 1  }
a3  = Acao {acelerar = False, travar = True,  direita = True,  esquerda = True,  nitro = Just 0  }
a4  = Acao {acelerar = True,  travar = False, direita = False, esquerda = False, nitro = Nothing }
a5  = Acao {acelerar = False, travar = True,  direita = False, esquerda = False, nitro = Nothing }
a6  = Acao {acelerar = False, travar = False, direita = True,  esquerda = False, nitro = Nothing }
a7  = Acao {acelerar = False, travar = True,  direita = False, esquerda = False, nitro = Nothing }
a8  = Acao {acelerar = False, travar = False, direita = True,  esquerda = False, nitro = Nothing }
a9  = Acao {acelerar = False, travar = False, direita = False, esquerda = True,  nitro = Nothing }
a10 = Acao {acelerar = False, travar = False, direita = False, esquerda = False, nitro = Just 1  }
a11 = Acao {acelerar = False, travar = False, direita = False, esquerda = False, nitro = Nothing }

c1  = Carro {posicao = (1.5,2.5), direcao = -36.45, velocidade = (1,1)} 
c2  = Carro {posicao = (3.4,4.4), direcao = 179.12, velocidade = (0,0)}
c3  = Carro {posicao = (4.5,1.4), direcao = 568.90, velocidade = (-1.54,-1)}

n1 = [1,1,1]
n2 = [0,1,3]

h1 :: [[Posicao]]
h1 = [[(2,3)],[(3,4)],[]]

j1  = Jogo m1 p1 [c1,c2,c3] n1 h1
j2  = Jogo m2 p1 [c1,c2,c3] n1 h1
j3  = Jogo m3 p1 [c1,c2,c3] n1 h1
j4  = Jogo m4 p1 [c1,c2,c3] n1 h1
j5  = Jogo m1 p2 [c1,c2,c3] n1 h1
j6  = Jogo m2 p2 [c1,c2,c3] n1 h1
j7  = Jogo m3 p2 [c1,c2,c3] n1 h1
j8  = Jogo m4 p2 [c1,c2,c3] n1 h1
j9  = Jogo m1 p3 [c1,c2,c3] n1 h1
j10 = Jogo m2 p3 [c1,c2,c3] n1 h1
j11 = Jogo m3 p3 [c1,c2,c3] n1 h1
j12 = Jogo m4 p3 [c1,c2,c3] n1 h1
j13 = Jogo m1 p4 [c1,c2,c3] n1 h1
j14 = Jogo m2 p4 [c1,c2,c3] n1 h1
j15 = Jogo m3 p4 [c1,c2,c3] n1 h1
j16 = Jogo m4 p4 [c1,c2,c3] n1 h1
j17 = Jogo m1 p1 [c1,c2,c3] n2 h1
j18 = Jogo m2 p1 [c1,c2,c3] n2 h1
j19 = Jogo m3 p1 [c1,c2,c3] n2 h1
j20 = Jogo m4 p1 [c1,c2,c3] n2 h1
j21 = Jogo m1 p2 [c1,c2,c3] n2 h1
j22 = Jogo m2 p2 [c1,c2,c3] n2 h1
j23 = Jogo m3 p2 [c1,c2,c3] n2 h1
j24 = Jogo m4 p2 [c1,c2,c3] n2 h1
j25 = Jogo m1 p3 [c1,c2,c3] n2 h1
j26 = Jogo m2 p3 [c1,c2,c3] n2 h1
j27 = Jogo m3 p3 [c1,c2,c3] n2 h1
j28 = Jogo m4 p3 [c1,c2,c3] n2 h1
j29 = Jogo m1 p4 [c1,c2,c3] n2 h1
j30 = Jogo m2 p4 [c1,c2,c3] n2 h1
j31 = Jogo m3 p4 [c1,c2,c3] n2 h1
j32 = Jogo m4 p4 [c1,c2,c3] n2 h1


-- | Corre a função atualiza numa lista de testes, para três carros
testa [] = []
testa ((t,j,a):x) = atualiza t j 0 a : atualiza t j 1 a : atualiza t j 2 a : testa x  

{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t e j a = Jogo {mapa = mapa e, pista = pista e, carros = fCar t e j a, nitros = tNitro t e j a, historico = atHist e j}

{-|
Função usada para somar todas as componentes da velocidade do carro j,
de acordo com a ação do jogador 
-}
vFinal :: Tempo -> Jogo -> Int -> Acao -> Velocidade
vFinal t e j a  | isNothing (nitro a)       = somaPares [x, acelera t e j a, pneus t e j a, atrito t e j, peso t j e]
                | fromJust (nitro a) == j   = somaPares [x, acelera t e j a, pneus t e j a, atrito t e j, peso t j e, nitroCarro t e j a]
                | otherwise                 = somaPares [x, acelera t e j a, pneus t e j a, atrito t e j, peso t j e]
                where 
                    x   = velocidade ((carros e) !! j)

{-|
Substitui o carro do jogador j, atualizado de acordo com a ação dada, à lista de carros do jogo
-}
mCar :: Tempo -> Jogo -> Int -> Acao -> [Carro] 
mCar t e j a    = substitui j Carro{posicao = posicao z, direcao = roda t e j a, velocidade = vFinal t e j a} (carros e)
                where 
                    z   = (carros e) !! j 

{-|
Atualiza o carro onde o nitro é aplicado, 
caso este seja diferente do carro do jogador que o
aplica
-}
fCar :: Tempo -> Jogo -> Int -> Acao -> [Carro]
fCar t e j a    | isNothing (nitro a) || i == j = mCar t e j a
                | otherwise                     = substitui i Carro{posicao = posicao z, direcao = direcao z, velocidade = (x'+xn, y'+vn)} (mCar t e j a)
                where 
                    z       = (carros e) !! i
                    i       = fromJust (nitro a)
                    (x',y') = velocidade z
                    (xn,vn) = nitroCarro t e j a

{-|
Calcula o vetor nitro, de acordo com o tempo da ação e
o tempo restante de nitro do jogador que o aplica
-}
nitroCarro :: Tempo -> Jogo -> Int -> Acao -> Velocidade
nitroCarro t e j a  | t > td    = polCart (k*td,o)
                    | otherwise = polCart (k*t, o)
                    where
                        k       = k_nitro (pista e)
                        td      = (nitros e) !! j
                        o       = direcao ((carros e) !! i)
                        i       = fromJust (nitro a)

{-|
Calcula o vetor da aceleração do carro j, de acordo com a ação dada
-}
acelera :: Tempo -> Jogo -> Int -> Acao -> Velocidade
acelera t e j a | ac && z   = polCart (0,0)
                | ac        = polCart (k*t, o)
                | z         = polCart (k*t, o+180)
                | otherwise = polCart (0,0)
                where 
                    ac      = acelerar a 
                    z       = travar a
                    k       = k_acel (pista e)
                    o       = direcao ((carros e) !! j)

{-|
Calcula a direção do carro j após a aplicação da ação dada
-}
roda :: Tempo -> Jogo -> Int -> Acao -> Angulo   
roda t e j a    | d && eq   = o
                | d         = o - (o' * t)
                | eq        = o + (o' * t)
                | otherwise = o 
                where 
                    d       = direita a
                    eq      = esquerda a
                    o'      = k_roda (pista e)
                    o       = direcao ((carros e) !! j)

{-|
Calcula o vetor peso, de acordo com a peça onde o carro j se encontra
-}
peso :: Tempo -> Int -> Jogo -> Velocidade 
peso t j e  | z == Rampa Norte  = polCart (k*t, -90)
            | z == Rampa Sul    = polCart (k*t, 90)
            | z == Rampa Este   = polCart (k*t, 180)
            | z == Rampa Oeste  = polCart (k*t, 0)
            | otherwise         = polCart (0,0)
            where 
                Peca z h        = m !! truncate x2 !! truncate x1
                k               = k_peso (pista e)
                car             = (carros e) !! j
                (x1,x2)         = posicao car
                Mapa o m        = mapa e

{-|
Calcula a força dos pneus, de acordo com a direção do carro e 
e a velocidade inicial, atingindo o valor maximo quando a direção do carro
é perpendicular à velocidade inicial.
-}
pneus :: Tempo -> Jogo -> Int -> Acao -> Velocidade
pneus t e j a   | snd (cartPol (n,p)) >= o  = polCart ((sin ang)*k*t*(norma (n,p)), o-90)
                | snd (cartPol (n,p)) < o   = polCart ((sin ang)*k*t*(norma (n,p)), o+90)
                where
                    o                       = direcao ((carros e) !! j)
                    k                       = k_pneus (pista e)
                    (n,p)                   = velocidade ((carros e) !! j)
                    ang                     = angVel (polCart (1,o)) (n,p)

{-|
Atualiza o histórico do jogo para o carro j, caso este se encontre 
numa posição diferente da mais recente existente no hístorico do carro
-}
atHist :: Jogo -> Int -> [[Posicao]]
atHist e j  | a == []               = [[y]]
            | a !! j == []          = substitui j [y] a
            | head carHist == y     = a
            | otherwise             = substitui j (y:carHist) a
            where
                car                 = (carros e) !! j
                (y1,y2)             = posicao car
                y                   = (truncate y1, truncate y2)
                a                   = historico e
                carHist             = a !! j
                 
{-|
Calcula a força de atrito, de acordo com a velocidade inicial
independentemente da ação que ocorre
-}
atrito :: Tempo -> Jogo -> Int -> Velocidade
atrito t e j    = ((-h)*k*t,(-z)*k*t)
                where 
                    (h,z)   = (velocidade ((carros e) !! j))
                    k       = k_atrito (pista e)

{-
vNitro :: Tempo -> Jogo -> Int -> Velocidade
vNitro t e j    | t > td    = polCart (k*td,o)
                | otherwise = polCart (k*t, o)
                where 
                    o       = direcao ((carros e) !! j)
                    k       = k_nitro (pista e)
                    td      = (nitros e) !! j
-}

{-|
Atualiza a quantidade de nitro do carro j, de acordo com a ação dada
-}
tNitro :: Tempo -> Jogo -> Int -> Acao -> [Tempo]
tNitro t e j a  | isNothing (nitro a)                       = nitros e
                | fromJust (nitro a) > length (nitros e)-1  = nitros e
                | t > td                                    = substitui j 0 (nitros e) 
                | otherwise                                 = substitui j (td -t) (nitros e) 
                where
                    k                   = k_nitro (pista e)
                    td                  = (nitros e) !! j

-----------------------------------------Funcões Auxiliares-----------------------------------------

{-|
Substitui o elemento de indíce j de uma lista pelo elemento n,
inserindo-o na mesma posição  
-}
substitui :: Int -> a -> [a] -> [a]
substitui j n []        = []
substitui j n [a]       = [n]
substitui j n (v:x:t)   | j == 0    = n:x:t
                        | otherwise = v : substitui (j-1) n (x:t) 

{-|
Transforma coordenadas cartesianas em polares, de acordo com 
as caracteristicas do referencial utilizado no projeto
-}
cartPol :: Velocidade -> VelocidadePolar
cartPol (x,y)   | x == 0 && y >= 0  = (norma (x,y), 90)
                | x == 0 && y < 0   = (norma (x,y), -90)
                | x > 0 && y >= 0   = (norma (x,y), -radGrau(atan (y/x)))
                | x < 0 && y >= 0   = (norma (x,y), -radGrau(atan (y/x)) + 180)
                | x < 0 && y < 0    = (norma (x,y), -radGrau(atan (y/x)) + 180)
                | x > 0 && y < 0    = (norma (x,y), -radGrau(atan (y/x)) + 360)
                | otherwise         = (norma (x,y), 90)

{-|
Calcula a norma de um vetor de coordenadas cartesianas
-}
norma :: Velocidade -> Double
norma (x,y)     = sqrt (x^2 + y^2)

{-|
Transforma coordenadas polares em cartesianas, de acordo com
as caracteristicas do referencial utilizado no projeto
-}
polCart :: VelocidadePolar -> Velocidade
polCart (n,a)   = (n*(cos (grauRad a)), n*(sin (grauRad (-a)))) 

{-|
Converte ângulos em radianos para ângulos em graus
-}
radGrau :: Angulo -> Angulo
radGrau a   = a * (180/pi)

{-|
Converte ângulos em graus para ângulos em radianos
-}
grauRad :: Angulo -> Angulo
grauRad a   = a * (pi/180)

{-|
Calcula o ângulo entre dois vetores, com recurso
à formula do produto escalar de dois vetores
-}
angVel :: Velocidade -> Velocidade -> Angulo
angVel (0,0) (x,y)      = 0
angVel (x,y) (0,0)      = 0
angVel (x1,y1) (x2,y2)  = acos ((x1*x2 + y1*y2)/((norma (x1,y1)) * (norma(x2,y2))))

{-|
Calcula a soma de uma lista de velocidades 
-}
somaPares :: [Velocidade] -> Velocidade
somaPares [a]                   = a
somaPares ((x1,y1):(x2,y2):t)   = somaPares ((x1+x2,y1+y2):t)
