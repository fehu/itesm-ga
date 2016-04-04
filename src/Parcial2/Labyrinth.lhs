\documentclass{article}

%include polycode.fmt

\usepackage[utf8]{inputenc}
\usepackage[spanish, mexico]{babel}

\begin{code}
module Parcial2.Labyrinth where
-- import Parcial2.ReadLabyrinth
\end{code}

\begin{document}

El mapa (laberinto), descrito en la tarea, se defina como un conjunto de puntos
(con posiciones correspondientes) y conecciones entre ellos.

El mapa se representa en el programa como un \emph{grafo no dirigido}
a traves de \underline{\emph{Data.Graph}} desde el paquete \underline{\emph{collections}}.

\begin{code}
import Control.Exception

import Data.Graph (Graph, Vertex, edges, buildG)
import Data.Map (Map)
import qualified Data.Map as Map

import Parcial2.ReadLabyrinth


data Labyrinth coord = Labyrinth { labyrinthGraph :: Graph
                                 , labyrinthNodes :: Map Vertex coord
                                 }
\end{code}


Se define la \emph{distancia directa} entre los nodos que están connectados por una arista.

\begin{code}
data DirectDistance coord dist = DirectDistance {
     labyrinthDist :: Labyrinth coord -> Vertex -> Vertex -> Maybe dist
    }

--mkDirectDistance :: (coord -> coord -> dist) -> DirectDistance coord dist
mkDirectDistance f = DirectDistance $
    \l v1 v2 -> do let ln = labyrinthNodes l
                       es = edges $ labyrinthGraph l
                   c1 <- Map.lookup v1 ln
                   c2 <- Map.lookup v2 ln
                   if (c1,c2) `elem` es
                    then Just $ f c1 c2
                    else Nothing
\end{code}


Se utiliza un mapa 2D:

\begin{code}
type Labyrinth2D = Labyrinth (Int, Int)
\end{code}


La lectura del archivo de mapa se encuentra en \href{src/Parcial2/ReadLabyrinth.hs}{}.
Aquí se describerá la construcción del grafo a partir del mapa leido.


\begin{code}
readLabyrinth2D :: FilePath -> IO (Either [String] Labyrinth2D)
readLabyrinth2D file = build <$> (try (readFile file) :: IO (Either SomeException String))
    where build (Left err) = Left [displayException err]
          build (Right s)  = case parseLabyrinth s of Left errS -> Left errS
                                                      Right l   -> Right $ build' l

          build' (LabyrinthDescription n conn setup coords) = undefined
--              where edges' =
--                    gr = buildG

\end{code}


\begin{code}
\end{code}

\end{document}
