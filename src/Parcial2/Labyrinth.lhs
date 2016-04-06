\documentclass{article}

%include polycode.fmt


\usepackage[utf8]{inputenc}
\usepackage[spanish, mexico]{babel}
\usepackage{amsmath, hyperref, xcolor, tikz, mdframed}
\usepackage[shortlabels, inline]{enumitem}

% \usepackage{showframe}

\newenvironment{note}
    {\begin{mdframed}[leftmargin=1cm,
                 skipabove=1em,
                 skipbelow=1em,
                 rightline=false,
                 topline=false,
                 bottomline=false,
                 linewidth=2pt]
        \textbf{Nota}\\}
    {\end{mdframed}}


\newcommand{\crule}[2][1pt] {\begin{center}\rule{#2\textwidth}{#1}\end{center}}

\newcommand{\hssrc} [2]{\href{http://./api/src/#1.html}{src/#2}}
\newcommand{\hstest}[2]{\href{http://./api/tests/src/#1.html}{test/#2}}


\begin{document}

\begin{code}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

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


Se define la \emph{distancia directa} entre los nodos que están conectados por una arista.

\begin{code}
  data DirectDistance point dist = DirectDistance {
       labyrinthDist :: Labyrinth point -> point -> point -> Maybe dist
      }

  mkDirectDistance f = DirectDistance $ \l v1 v2 ->
    if (v1,v2) `edgeOf` edges l then Just (f v1 v2) else Nothing
\end{code}




\bigskip
\noindent
El algoritmo genético abstracto está definido en \hssrc{GeneticAlgorithm}{GeneticAlgorithm.hs}.
Su implementación se presentará a continuación.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Implementación}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Lectura de mapas}

Se utiliza un mapa 2D:

> newtype Point2D = Point2D (Int, Int) deriving (Eq, Ord)
> instance Show Point2D where
>   show (Point2D (x,y)) = show x ++ "-" ++ show y

> type Labyrinth2D = Labyrinth Point2D


La lectura del archivo del mapa se encuentra en
\hssrc{Parcial2-ReadLabyrinth}{Parcial2/ReadLabyrinth.hs}.
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
          let get = Point2D . (coords !!)
          in Labyrinth
                       (Set.fromList $ map Point2D coords)
                       (Set.fromList $ map (first get . second get) coords)
                       (get i)
                       (get t)

\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Adoptación}



Se define el valor de \emph{aptitud de adoptación} como uno de los dos:
\begin{itemize}
  \item longitud de la ruta completa;
  \item grado de valides $\dfrac
    {\text{número de aristas existentes}}
    {\text{número de aristas total}}$.

    \textit{aristas existentes ---
    aristas que existen entre los pares de genes ajustados.}
\end{itemize}

> data Route (dir :: OrdDir) = RouteLength Double | RouteValidess Double
>           deriving (Eq, Show)

\noindent También tiene un parametro de tipo para establecer la dirección de busqueda,
lo que determina el orden deseado.
Se define la orden sobre la aptitud de tal manera que dependiendo en la dirección:
\begin{itemize}
  \item \emph{Min} --- $\forall x \in \textit{longitud}, y \in \textit{valides} \Rightarrow x < y$;

\begin{code}
  instance Ord (Route Min) where
    compare (RouteLength x)   (RouteLength y)   = compare x y
    compare (RouteValidess x) (RouteValidess y) = compare x y
    compare (RouteLength _)   (RouteValidess _) = LT
    compare (RouteValidess _) (RouteLength _)   = GT
\end{code}

  \item \emph{Max} --- $\forall x \in \textit{longitud}, y \in \textit{valides} \Rightarrow x > y$.

\begin{code}
  instance Ord (Route Max) where
    compare (RouteLength x)   (RouteLength y)   = compare x y
    compare (RouteValidess x) (RouteValidess y) = compare x y
    compare (RouteLength _)   (RouteValidess _) = GT
    compare (RouteValidess _) (RouteLength _)   = LT
\end{code}

\end{itemize}


Las pruebas del contenedor \emph{Route} se encuentran en \hstest{Parcial2-Route}{Parcial2/Route.hs}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Algoritmo genético}


> data GA = GA Labyrinth2D

Se usa adelante un alias de tuple \verb|(a,a)| para denotar el número de hijos de \emph{crossover}.

\begin{code}
  newtype Pair a = Pair (a,a)
  unwrapPair (Pair p) = p
  pair2List (Pair (f,s)) = [f,s]
\end{code}



\noindent
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



\crule{1}
\medskip
\noindent
Se define la instancia de la clase \emph{GeneticAlgorithm} para \emph{GA}
empezando con los tipos y siguiendo con los métodos.

> instance GeneticAlgorithm GA where

\begin{enumerate}[(1)]

\item Un \emph{gen} se define como \underline{nodo del laberinto}
      y un \emph{cromosoma} como una \underline{lista de genes}.

>    type Gene GA = Point2D
>    type Chromosome GA = [Point2D]
>    -- listGenes :: Chromosome ga \rightarrow$ [Gene ga]
>    listGenes = id

\item Los valores de aptitud ya fueron descritos previamente.

>    type Fitness GA = Route Min


\item Para denotar que la operación de \emph{crossover} preserva el tamaño de población,
su resultado se marca como un par de hijos.

>    type CrossoverChildren GA = Pair

\item La información de entrada para generación de la población --- el laberinto.

>    type InputData GA = Labyrinth2D

\item El resultado es el \underline{mejor cromosoma} obtenido.

>    type ResultData GA = Chromosome GA



\item La \textbf{aptitud de adoptación} se define como:

$$
f(c) &= \begin{cases}
  \mathit{Length} ~ \mathrm{length}
    & \mbox{si } \begin{tabular}{l}
             \forall i = \overline{[1,\mathrm{len}-1]} \Rightarrow \\
             \qquad \exists \text{ arista, connectando } c_{i-1} \text{ y } c_i \\
             \land ~\mathrm{initial} \in \{c\}\\
             \land ~\mathrm{target} \in \{c\}
      \end{tabular}
  \\
  \mathit{Validess} ~\mathrm{validess}
    & \mbox{en otro caso}
  \end{cases}
$$

\qquad\qquad  donde
\begin{align*}
\mathrm{length} &= \sum\limits_{i = 1}^{\mathrm{len}-1} \mathrm{dist}(c_{i-1}, c_i)
\\
\mathrm{validess} &= \text{grado de valides (se describe antes)}
\end{align*}


>    -- fitness :: ga \rightarrow$ Chromosome ga \rightarrow$ Fitness ga
>    fitness (GA l) genes = let
>                               lPairs (f:s:t) = (f,s) : lPairs (s:t)
>                               lPairs _       = []
>                               dists = map (uncurry $ eDist l) (lPairs genes)
>                           in if isJust `all` dists
>                                then -- is a valid route
>                                     RouteLength . sum $ map fromJust dists
>                                else -- is incomplete
>                                     RouteValidess $
>                                       fromIntegral (length $ filter isJust dists)
>                                       / fromIntegral (length dists)




\item Generación de cromosomas aleatorios.

\begin{figure}[h]
    \centering
    \input{MapExampleRaw.tikz}
    \caption{Un exemplo de mapa, inicio: 0--2, meta: 9--3.}
    \label{fig:rawMapExample}
\end{figure}

\begin{figure}[h]
    \centering
    \input{MapExampleChromosomes.tikz}
    \caption{Se presentan algunos cromosomas en el mapa.
             Los cromosomas {\color{orange} •} {\color{blue} •} {\color{green} •} están compuestas
               de pares de genes, conectados por aristas;
             mientras que los cromosomas {\color{red} •} {\color{violet} •} están compuestos
               de cadenas de genes, conectados por aristas, de longitud 3.
            \textit{\small (Son de diferente grosor para que se ven mejor
                            las conecciones que existen en varios cromosomas)}
            }
    \label{fig:chromosomesMapExample}
\end{figure}


\noindent Para mejorar las poblaciones iniciales, las cromosomas se componen de
secuencias de genes, que son sub-rutas validas de tamaños diferentes.

En la figura \ref{fig:rawMapExample} se presenta un ejemplo de un mapa y
en la figura \ref{fig:chromosomesMapExample} se presenta un exemplo de cromosomas generados.






{\Huge \color{red} TBD \dots}






>    -- randomChromosome :: ga \rightarrow$ IO (Chromosome ga)
>    randomChromosome (GA l) = undefined


\item ?

>    -- crossover :: Chromosome ga \rightarrow$ Chromosome ga
>    -- \qquad\qquad \rightarrow$ CrossoverChildren ga (Chromosome ga)

\item ?

>    -- mutate :: Chromosome ga \rightarrow$ Chromosome ga

\item ?

>    -- stopCriteria :: [Fitness ga] \rightarrow$ Bool

\item ?

>    -- newGA :: InputData ga \rightarrow$ ga


\end{enumerate}



\newpage

{\Huge \color{red} Esto es un reporte preliminar }

\begin{note}
  La intención es utilizar \emph{crossover} para:
  \begin{enumerate*}[1)]
    \item remplazar los ''hoyos'' en las rutas;
    \item extender rutas existientes.
  \end{enumerate*}
  La preferencia debe ser dada a las rutas que contienen un de los puntos de interes (inicio, meta).

  La mutación debe extender/remplacar un gen al inicio/meta si $\exists$ una ruta directa.

  \medskip
  \noindent {\Large \color{red} !} El concepto de \emph{''valides''} va ser cambiado.
\end{note}





\end{document}
