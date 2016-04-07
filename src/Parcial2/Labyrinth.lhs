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

\def\github{http://fehu.github.io/itesm-ga/}

\newcommand{\hssrc} [2]{\href{\github/api/src/#1.html}{src/#2}}
\newcommand{\hstest}[2]{\href{\github/api/tests/src/#1.html}{test/#2}}


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


El valor de \emph{aptitud de adoptación} se llamará \emph{ruta} y se define
para permitir destinguir facilmente los dos tipos de rutas
(encodificadas en los cromosomas) posibles:

\begin{itemize}
  \item \textbf{Ruta completa}: es una ruta valida
    ($\exists$ una conección entre cada par de genes adjuntos) que contiene
    en punto inicial y el punto meta.

    Se caracteriza por la \emph{longitud de la ruta}.

    El resultado, esperado del algoritmo genético es la más corta de estas rutas.

  \item \textbf{Ruta parcial}: es una ruta que
    \begin{enumerate*}[1)]
        \item contiene pares de genes adjuntos, los cuales no están conectados, o
        \item no contiene los ambos puntos: inicio y meta.
    \end{enumerate*}

    Se caracteriza por los tres valores:
    \begin{enumerate}

        \item \emph{valides} $= \dfrac
            {\text{número de aristas existentes}}
            {\text{número de aristas total}}$.

            \textit{aristas existentes ---
                    aristas que existen entre los pares de genes adjuntos.}

        \item los \emph{puntos de interés} que contiene la ruta.
        \item la longitud sumatoria de las sub-rutas en el cromosoma.

    \end{enumerate}

\end{itemize}



\begin{code}

  data POI  = POIInit | POITarget deriving (Eq, Ord, Enum, Show)
  data POIs = POINone | POISome POI | POIBoth deriving (Eq, Show)

  instance Ord POIs where
    x `compare` y = val x `compare` val y where
        val POINone = 0
        val (POISome _) = 1
        val POIBoth = 2


  data Route =
      CompleteRoute { routeLength :: Double }
    | PartialRoute  { partialValidess :: Double
                    , partialPOI      :: POIs
                    , partialLength   :: Double
                    }
    deriving (Eq, Show)
\end{code}

\noindent Para la busqueda de la ruta mas corta,
se define el orden sobre las rutas de tal manera, que
una lista de \emph{rutas}, ordenada ascendentamente,
tendrá los mejores elementos en el principio.
\begin{enumerate}

  \item Cualquiera \emph{ruta completa} \underline{es minor} que cualquiera \emph{ruta parcial}.
        $$\forall x \in \textit{ruta completa},
                  y \in \textit{ruta parcial}     \implies x < y$$

  \item Dos \emph{rutas completas} se comparan por su \underline{longitud} sin cambios de la orden.
        \begin{align*}
            \begin{tabular}{l}
                \forall x \in \textit{ruta completa}, x \sim r_1\\
                \forall y \in \textit{ruta completa}, y \sim r_2
            \end{tabular}
            & \implies
            & \begin{cases}
                x < y & \mbox{si } r_1 < r_2 \\
                x > y & \mbox{si } r_1 > r_2 \\
                x = y & \mbox{en otro caso}
              \end{cases}
        \end{align*}

  \item Dos \emph{rutas parciales} se comparan por sus \underline{tres componentes} en orden
        \underline{lexicográfico}, que quiere decir que
        primero se comparan los primeros elementes, si son igual, se comparan los segundos, etc.,
        hasta que la comparación da un resultado diferente de igualidad o se termina la lista.

        El orden de la comparación se cambia al opuesto.

        \begin{align*}
            \begin{tabular}{l}
                \forall x \in \textit{ruta parcial}, x \sim \langle v_x, i_x, l_x \rangle \\
                \forall y \in \textit{ruta parcial}, y \sim \langle v_y, i_y, l_y \rangle
            \end{tabular}
            & \implies
            & \begin{cases}
                x < y & \mbox{si } \langle v_x, i_x, l_x \rangle > \langle v_y, i_y, l_y \rangle \\
                x > y & \mbox{si } \langle v_x, i_x, l_x \rangle < \langle v_y, i_y, l_y \rangle \\
                x = y & \mbox{en otro caso}
              \end{cases}
        \end{align*}

\end{enumerate}

\begin{code}
  instance Ord Route where
    compare (CompleteRoute x)       (CompleteRoute y)       = compare x y
    compare (PartialRoute v1 i1 l1) (PartialRoute v2 i2 l2) = compare (v2,i2,l2) (v1,i1,l1)
    compare (CompleteRoute _)        PartialRoute{}         = LT
    compare  PartialRoute{}         (CompleteRoute _)       = GT
\end{code}


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

>    type Fitness GA = Route


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

\begin{code}
    -- fitness :: ga \rightarrow$ Chromosome ga \rightarrow$ Fitness ga
     fitness (GA l) genes = let
                            lPairs (f:s:t) = (f,s) : lPairs (s:t)
                            lPairs _       = []
                            dists = map (uncurry $ eDist l) (lPairs genes)
                        in if isJust `all` dists
                             then -- is a valid route
                                  CompleteRoute . sum $ map fromJust dists
                             else -- is incomplete
                                  let v = fromIntegral (length $ filter isJust dists)
                                        / fromIntegral (length dists)
                                      poi = undefined
                                      len = undefined
                                  in PartialRoute v poi len

\end{code}



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
