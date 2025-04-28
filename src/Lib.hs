module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    poder :: Poder,
    superPoder :: Poder,
    superActiva :: Bool,
    cantidadDeVida :: Int }deriving Show

type Poder = Personaje -> Personaje

espina :: Personaje
espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" lluviaDeTuercasSanadoras torretaCurativa False 9600

sumarVida :: Int -> Personaje -> Personaje
sumarVida vidaAgregada unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + vidaAgregada}

restarVida :: Int -> Personaje -> Personaje
restarVida vidaRestada unPersonaje = unPersonaje {cantidadDeVida = max (cantidadDeVida unPersonaje - vidaRestada) 0}

dividirVidaEntera :: Int -> Personaje -> Personaje
dividirVidaEntera divisor unPersonaje = unPersonaje {cantidadDeVida = div (cantidadDeVida unPersonaje) divisor}

multiplicarVida :: Int -> Personaje -> Personaje
multiplicarVida multiplicador unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje * multiplicador}

bolaEspinosa :: Poder
bolaEspinosa = restarVida 1000 

lluviaDeTuercasSanadoras :: Poder
lluviaDeTuercasSanadoras = sumarVida 800

lluviaDeTuercasDaninas :: Poder
lluviaDeTuercasDaninas = dividirVidaEntera 2

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radio unPersonaje
    | radio > 3 && cantidadDeVida unPersonaje < 800 = unPersonaje {nombre = nombre unPersonaje ++ " Espina estuvo aqui", cantidadDeVida = 0, superActiva = False}
    | radio > 3 = unPersonaje {nombre = nombre unPersonaje ++ " Espina estuvo aqui"}
    | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Poder
torretaCurativa unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje * 2}

{-bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).
lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y las segundas le disminuyen 
    a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al personaje.
granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su nombre “Espina estuvo aquí”. 
    Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. En otro caso, se usa una bola de espinas.
torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial. -}