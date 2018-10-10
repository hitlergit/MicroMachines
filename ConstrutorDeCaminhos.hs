module Construcao where

import LI11718

type Passos1 = Passo
a :: Passos1
a = Avanca
d :: Passos1
d = CurvaDir
e :: Passos1
e = CurvaEsq
s :: Passos1
s = Sobe
r :: Passos1
r = Desce

caminhos :: [Passos1] -> Caminho 
caminhos [] = []
caminhos (h:t)  | h == a = Avanca : caminhos t
                | h == d = CurvaDir : caminhos t
                | h == e = CurvaEsq : caminhos t
                | h == s = Sobe : caminhos t
                | h == r = Desce : caminhos t


                