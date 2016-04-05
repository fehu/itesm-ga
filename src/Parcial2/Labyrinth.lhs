\documentclass{article}

%include polycode.fmt


\usepackage[utf8]{inputenc}
\usepackage[spanish, mexico]{babel}
\usepackage{amsmath, hyperref, xcolor, tikz}

\usepackage{showframe}


% \newcommand{hs}[2]{\href{api/src/#1.html}{src/#2}}

\begin{document}

\begin{code}
{-# LANGUAGE TypeFamilies
           , UndecidableInstances
           , FlexibleContexts
       #-}

module Parcial2.Labyrinth where

  import Control.Exception
  import Control.Arrow (first, second)
  import Control.Monad.Fix

  import Data.Tuple (swap)
  import Data.Maybe (isJust, fromJust)
  import Data.Set (Set, member)
  import qualified Data.Set as Set
  import GHC.Real (infinity)

  import Parcial2.ReadLabyrinth
  import GeneticAlgorithm

  import System.Random
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Introducción}


El mapa (laberinto), descrito en la tarea, se define como un grafo:
nodos --- un conjunto de puntos (con posiciones correspondientes);
aristas --- la existencia de rutas directas.

\begin{code}
  data Labyrinth point = Labyrinth {
        nodes   :: Set point,
        edges   :: Set (point, point),
        initial :: point,
        target  :: point
      }

  edgeOf p es = any (`member` es) [p, swap p]

  mapPoints f (Labyrinth ns es i t) = Labyrinth {
        nodes = Set.map f ns,
        edges = Set.map (first f . second f) es,
        initial = f i,
        target  = f t
    }

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
El algoritmo genético abstracto está definido en
% \hs{GeneticAlgorithm}{
GeneticAlgorithm.hs
% }.
Su implementación se presentará adelante.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Implementación}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Lectura de mapas}

Se utiliza un mapa 2D:

> newtype Point2D = Point2D (Int, Int) deriving Eq
> instance Show Point2D where
>   show (Point2D (x,y)) = show x ++ "-" ++ show y

> type Labyrinth2D = Labyrinth Point2D


La lectura del archivo del mapa se encuentra en
% \hs{Parcial2-ReadLabyrinth}{
Parcial2/ReadLabyrinth.hs
% }.
Aquí se presenta la construcción del grafo a partir del mapa leido.

La instancia de \emph{Ord} usada determinará

\begin{code}
  readLabyrinth2D :: (Ord Point2D) =>
                  FilePath -> IO (Either [String] Labyrinth2D)

  readLabyrinth2D file = build <$> try (readFile file)
    where
        build (Left err) = Left [displayException (err :: SomeException)]
        build (Right s)  = case parseLabyrinth s of
                                   Left errS -> Left errS
                                   Right l   -> Right (build' l)

        build' (LabyrinthDescription n conn (i,t) coords) =
          let get = Point2D . (coords !!)
          in Labyrinth
                       (Set.fromList $ map Point2D coords)
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

Se defina el valor de aptitud como uno de los dos:
\begin{itemize}
  \item longitud de la ruta completa;
  \item grado de valides $\dfrac
    {\text{número de aristas existentes en gene}}
    {\text{número de aristas total}}$.

    \textit{aristas existentes ---
    aristas que existen entre los pares de genes ajustados.}
\end{itemize}

% > newtype CompositeFitness = CompositeFitness (Either Double Double)

> data Route = RouteLength Double | RouteValidess Double
>           deriving (Eq, Show)

\noindent Y se defina la orden sobre la aptitud de tal manera que
$$\forall x \in \textit{longitud}, y \in \textit{valides} \Rightarrow x > y$$

> instance Ord Route where
>   compare (RouteLength x)   (RouteLength y)   = compare x y
>   compare (RouteValidess x) (RouteValidess y) = compare x y
>   compare (RouteLength _)   (RouteValidess _) = GT
>   compare (RouteValidess _) (RouteLength _)   = LT

Se define el orden \textbf{ascendiente} soble los puntos,
para que los mejores cromosomas (con valores \textbf{menores})
sean en el principio de la lista, que representa la población.

> instance Ord Point2D where
>   compare (Point2D p1) (Point2D p2) = compare p1 p2

Se define el orden sobre los contenedores de aptitud.

Se define la metrica sobre los puntos del grafo:
$$
  \mathrm{dist}(p_1, p_2) = \begin{cases}
       \mathit{Just}~ d_E(p_1, p_2)
    &  \mbox{si } \exists \text{ arista, connectando } p_1 \text{ y } p_2
    \\ \mathit{Nothing}
    &  \mbox{en otro caso}
  \end{cases}
\text {, donde}

d_E \text{ --- es la distancia euclidiana entre dos puntos.}
$$

> eDist' = mkDirectDistance $
>         \(Point2D (x1,x2)) (Point2D (y1,y2)) ->
>               sqrt $ fromIntegral $
>               abs(x1-x2)^2 + abs(y1-y2)^2
> eDist  = labyrinthDist eDist'

Se define la instancia de la clase \emph{GeneticAlgorithm} para \emph{GA}
empezando con los tipos y siguiendo con los métodos.

> instance GeneticAlgorithm GA where

\begin{itemize}

\item Un \emph{gene} se define como \underline{nodo del laberinto}
y un \emph{cromosoma} como una \underline{lista de genes}.

>    type Gene GA = Point2D
>    type Chromosome GA = [Point2D]
>    -- listGenes :: Chromosome ga \rightarrow$ [Gene ga]
>    listGenes = id

\item Los valores de aptitud ya fueron descritos previamente.

>    type Fitness GA = Route

\item Para denotar que la operación de \emph{crossover} preserva el tamaño de población,
su resultado se marca como un par de hijos.

>    type CrossoverChildren GA = Pair

\item La información de entrada para generación de la población --- el laberinto.

>    type InputData GA = Labyrinth2D

\item El resultado es el \underline{mejor chromosoma} obtenida.

>    type ResultData GA = Chromosome GA



\item Generación de cromosomas aleatorios.

>    -- randomChromosome :: ga \rightarrow$ IO (Chromosome ga)
>    randomChromosome (GA l) = do

Primero se genera aleotoriamente el tamaño extra del cromosoma con valor entre $0$ y $2N$.
Dos valores mas se reservan para el punto inicial y el punto final.
El tamaño final de los cromosomas generados está entre $2$ y $2N + 2$.

>       len' <- getStdRandom $ randomR (0, 2* Set.size (nodes l))

Un punto aleatorio se selecciona entre todos los nodos del mapa, excepto la posición inicial
del agente y el punto meta.

>       let randPoint = undefined :: StdGen -> (Point2D, StdGen)

Un punto aleatorio se re-genera hasta que se encuentra uno que todavía no está en el cromosoma,
generado previamente (ulilizando el mismo generador).

>       let rand prev = fix $ \f g ->
>                        let (r, g') = randPoint g
>                        in if r `elem` prev  then f g' else (r, g')

Se genera la parte aleatoria del cromosoma.

>       rnd <- getStdGen
>       let (genes, _) = ($ ([], rnd)) . fix $
>                           \f (l,g) -> if length l == len'
>                                       then (l, g)
>                                       else first (:l) (rand l g)

Todas las rutas, encodificadas en los cromosomas, se empiezan en
el punto inicial y se terminan en el punto meta.

>       let chrom = [initial l] ++ genes ++ [target l]
>       return chrom



\item La \textbf{aptitud de adoptación} se define como:

$$ f(c) = \begin{cases}
\sum\limits_{i = 1}^{\mathrm{len}-1} \mathrm{dist}(c_{i-1}, c_i)
    & \mbox{si } \begin{tabular}{l}
             \forall i = \overline{[1,\mathrm{len}-1]} \Rightarrow \\
             \exists \text{ arista, connectando } c_{i-1} \text{ y } c_i
      \end{tabular}
\\
+\infty & \mbox{en otro caso}
\end{cases}
$$

>    -- fitness :: ga \rightarrow$ Chromosome ga \rightarrow$ Fitness ga
>    fitness (GA l) genes = let
>                               lPairs (f:s:t) = (f,s) : lPairs (s:t)
>                               lPairs _       = []
>                               dists = map (uncurry $ eDist l) (lPairs genes)
>                           in if isJust `all` dists
>                                then undefined -- sum $ map fromJust dists
>                                else undefined -- fromRational infinity

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
