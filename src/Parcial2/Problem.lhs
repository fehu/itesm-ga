

Diseñar un agente inteligente basado en un robot explorador, de tal forma que a partir de
un mapa (laberinto), el agente sea capaz de encontrar la salida utilizando un algoritmo
genético.

\medskip

Asume que el laberinto será un archivo de texto, en donde se definirán
una serie de nodos y una matriz de adyacencia.
Para ello, se utilizará el siguiente formato

\begin{enumerate}

\item Línea 1: un número $N$ que indica cuantos nodos habrá en el laberinto.

\item Siguientes $N$ líneas: $v_{i1}, v_{i2}, \dots ,v_{iN}$ (cada $v_{ij}$ es un $0$ o $1$,
    donde $0$ representa que no existe adyacencia entre el nodo de la i-ésima línea y en
    nodo de la j-ésima columna).
\item Siguiente línea: $vI$, $vF$ (dos números, donde $vI$ representa el nodo de
    partida del agente robótico y $vF$ representa el nodo al cual tiene que llegar).
\item Siguientes $N$ líneas: $x,y$ (donde $x_i,y_i$  representan las coordenadas donde
    se encuentra en nodo i-ésimo).

\end{enumerate}

\begin{code}
module Parcial2.Problem where

import Data.Graph (Graph, Vertex)
import Data.Map (Map)

\end{code}



\begin{code}
data LabyrinthNode coord = LabyrinthNode{ nodeId :: Vertex,
                                          nodePosition :: coord
                                        }
\end{code}
