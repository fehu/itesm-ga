\documentclass{article}

%include polycode.fmt

\usepackage[utf8]{inputenc}
\usepackage[spanish, mexico]{babel}

\usepackage{showframe}

\begin{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Introducción}


El mapa (laberinto), descrito en la tarea, se defina como un grafo:
nodos --- un conjunto de puntos (con posiciones correspondientes);
aristas --- la existencia de rutas directas.

\begin{code}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Parcial2.Labyrinth where

  import Control.Exception
  import Control.Arrow (first, second)

  import Data.Set (Set, member)
  import Data.Tuple (swap)
  import qualified Data.Set as Set

  import Parcial2.ReadLabyrinth
  import GeneticAlgorithm

  data Labyrinth point = Labyrinth { nodes   :: Set point
                                   , edges   :: Set (point, point)
                                   , initial :: point
                                   , target  :: point
                                   }

  edgeOf p es = any (`member` es) [p, swap p]

\end{code}


Se define la \emph{distancia directa} entre los nodos que están connectados por una arista.

\begin{code}
  data DirectDistance point dist = DirectDistance {
       labyrinthDist :: Labyrinth point -> point -> point -> Maybe dist
      }

  mkDirectDistance f = DirectDistance $ \l v1 v2 ->
    if (v1,v2) `edgeOf` edges l then Just (f v1 v2) else Nothing
\end{code}




\bigskip
\noindent
El algoritmo generico abstracto está definido en \href{src/GeneticAlgorithm.hs}{}.
Su implementación se presentará adelante.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Implementación}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Lectura de mapas}

Se utiliza un mapa 2D:

> type Point2D = (Int, Int)
> type Labyrinth2D = Labyrinth Point2D


La lectura del archivo de mapa se encuentra en \href{src/Parcial2/ReadLabyrinth.hs}{}.
Aquí se presenta la construcción del grafo a partir del mapa leido.


\begin{code}
  readLabyrinth2D :: FilePath -> IO (Either [String] Labyrinth2D)

  readLabyrinth2D file = build <$> try (readFile file)
    where
        build (Left err) = Left [displayException (err :: SomeException)]
        build (Right s)  = case parseLabyrinth s of
                                   Left errS -> Left errS
                                   Right l   -> Right (build' l)

        build' (LabyrinthDescription n conn (i,t) coords) =
          let get = (coords !!)
          in Labyrinth
                       (Set.fromList coords)
                       (Set.fromList $ map (first get . second get) coords)
                       (get i)
                       (get t)

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Algoritmo genético}


> data GA = GA Labyrinth2D

Un alias para tuple \verb|(a,a)|.

> newtype Pair a = Pair (a,a)
> unwrapPair (Pair p) = p
> pair2List (Pair (f,s)) = [f,s]

Se define la instancia de la clase \emph{GeneticAlgorithm} para \emph{GA}
empiezando con los tipos y siguiendo con los métodos.

> instance GeneticAlgorithm GA where

\begin{itemize}

\item Un \emph{gene} se define como \underline{nodo del laberinto}
y una \emph{cromosoma} como una \underline{lista de genes}.

>    type Gene GA = Point2D
>    type Chromosome GA = [Point2D]
>    -- listGenes :: Chromosome ga \rightarrow$ [Gene ga]
>    listGenes = id

\item Los valores de adaptación van a tener un tipo flotante de doble precision.

>    type Fitness GA = Double

\item Para denotar que la operación de \emph{crossover} preserva el tamaño de población,
su resultado se marca como un par de hijos.

>    type CrossoverChildren GA = Pair

\item La información de entrada para generación de la población --- el laberinto.

>    type InputData GA = Labyrinth2D

\item El resultado es la \underline{mejor chromosoma} obtenida.

>    type ResultData GA = Chromosome GA

\item Generación de población inicial.

>    -- initialPopulation :: ga \rightarrow$ IO [Chromosome ga]
>    initialPopulation (GA l) = do




>                                  undefined

\item ?

>    -- fitness :: Chromosome ga \rightarrow$ Fitness ga

\item ?

>    -- crossover :: Chromosome ga \rightarrow$ Chromosome ga
>    -- \qquad\qquad \rightarrow$ CrossoverChildren ga (Chromosome ga)

\item ?

>    -- mutate :: Chromosome ga \rightarrow$ Chromosome ga

\item ?

>    -- stopCriteria :: [Fitness ga] \rightarrow$ Bool

\item ?

>    -- newGA :: InputData ga \rightarrow$ ga


\end{itemize}









\end{document}
