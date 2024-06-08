type Ejes = Int
type Altura = float
type Peso = Int

data Auto = Soloa | ConTrailer Ejes Altura deriving Show

data Rodado 
         = Automovil Auto
         | Moto
         | Bus Ejes Altura
         | Camion Ejes Altura Peso 
         deriving Show

iguales :: Rodado -> Rodado -> Bool
iguales Moto Moto = True
Iguales (Automovil _) (Automovil _) = True
iguales (Bus e1 _) (Bus e2 _) = e1 == e2
iguales (Camion e1 _ _) (Camion e2 _ _) = e1 == e2
iguales _ _= False

--Funcion para verificar  si un rodado tiene la misma cantidad de ejes que el parámetro pasado.
mismaCantEjes :: Rodado -> Ejes -> Bool
mismaCantEjes (Automovil (ConTrailer e' a)) e = ( e == e')
mismaCantEjes (Bus e' a) e = (e == e')
mismaCantEjes (Camion e' a p) e = (e == e')
mismaCantEjes (Automovil SoloA) e = (2 == e)
mismaCantEjes  Moto e = False

--Funcion para obtener la altura de un Rodado.
alt :: Rodado -> Altura
alt (Automovil (ConTrailer e a)) = a
alt (Bus e a ) = a
alt (Camion e a p) = a
alt (Automovil SoloA) = 1.8
alt Moto = 1.0

--Funcion para obtener la cantidad de ejesde un Rodado
cantEjes :: Rodado -> Int
cantEjes (Automovil (ConTrailer e' a)) = e'
cantEjes (Bus e' a) = e'
cantEjes (Camion e' a p) = e'
cantEjes (Automovil SoloA) = 2
cantEjes Moto = 0


-- Instancia Eq Rodado Where

Instance Eq Rodado Where 

Moto == Moto = True
(Automovil _ ) (Automovil _ ) = True
(Bus e1 _ ) (Bus e2 _ ) = e1 == e2
(Camion e1 _ _ ) (Camion e2 _ _ ) = e1 == e2
_ == _ = False

--Filtra los autos con trailer cuya altura sea mayor que la dada

autosConTrailer :: [Rodado] -> Altura -> [Rodado]
autosConTrailer [] _ = []
autosConTrailer (Automovil (ConTrailer e alt):rs)Altura
  |alt > Altura = Automovil (ConTrailer e alt) autosConTrailer rs Altura
  |otherwise = autosConTrailer rs Altura
  autosConTrailer(_:rs) autosConTrailer rs Altura

dosIguales :: [Rodado] -> Bool
dosIguales [] = False
dosIguales [_] = False
dosIguales (x1:x2:xs) = x1 == x2 || dosIguales(r2:rs)








