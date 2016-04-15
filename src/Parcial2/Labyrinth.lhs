\documentclass{article}

%include polycode.fmt


\usepackage[utf8]{inputenc}
\usepackage[spanish, mexico]{babel}
\usepackage{amsmath, hyperref, xcolor, tikz, mdframed, subcaption}
\usepackage[shortlabels, inline]{enumitem}

\usetikzlibrary{chains, fit}

\usepackage{showframe}

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

\newcommand{\resizePicture}[2][\textwidth]{\resizebox{#1}{!}{#2}}}
\newcommand{\resizeInput}[2][\textwidth]{\resizePicture[#1]{\input{#2}}}

\begin{document}

\begin{code}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module Parcial2.Labyrinth where

  import Control.Exception
  import Control.Arrow (first, second, (&&&))
  import Control.Monad.Fix

  import Data.Tuple (swap)
  import Data.List (elemIndex, sort, nub, minimumBy, maximumBy)
  import Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)
  import Data.Bits (xor)
  import Data.Either (isLeft, isRight, Either(..))
  import Data.Function (on)
  import Data.Set (Set, member, elemAt)
  import qualified Data.Set as Set
  import Data.Map (Map)
  import qualified Data.Map as Map
  import GHC.Exts (Down(..), sortWith)


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
  data Labyrinth point = Labyrinth  { nodes   :: Set point
                                    , edges   :: Set (point, point)
                                    , initial :: point
                                    , target  :: point
                                    }
        deriving (Show, Eq)

  edgeOf p l = any (`member` edges l) [p, swap p]

  mapPoints f (Labyrinth ns es i t) = Labyrinth {
        nodes = Set.map f ns,
        edges = Set.map (first f . second f) es,
        initial = f i,
        target  = f t
    }

  isPOI p l = p == initial l || p == target l

\end{code}


Se define la \emph{distancia directa} entre los nodos que están conectados por una arista.

\begin{code}
  data DirectDistance point dist = DirectDistance {
       labyrinthDist :: Labyrinth point -> point -> point -> Maybe dist
      }

  mkDirectDistance f = DirectDistance $ \l v1 v2 ->
    if (v1,v2) `edgeOf` l then Just (f v1 v2) else Nothing
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
          in Labyrinth  (Set.fromList $ map Point2D coords)
                        (Set.fromList $ map (first get . second get) coords)
                        (get i)
                        (get t)

\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Adaptación}
\label{subsec:fitness}


El valor de \emph{aptitud de adaptación} se llamará \emph{ruta} y se define
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
        \item no contiene ambos puntos: inicio y meta.
    \end{enumerate*}

    Se caracteriza por tres valores:
    \begin{enumerate}

        \item \emph{validez} $= \dfrac
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


  data RouteFitness =
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

  \item Cualquiera \emph{ruta completa} \underline{es menor} que cualquier \emph{ruta parcial}.
        $$\forall x \in \textit{ruta completa},
                  y \in \textit{ruta parcial}     \implies x < y$$

  \item Dos \emph{rutas completas} se comparan por su \underline{longitud} sin cambios en el orden.
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

  \item Dos \emph{rutas parciales} se comparan por sus \underline{tres componentes} en orden \\
        \underline{lexicográfico}, que quiere decir que
        primero se comparan los primeros elementos, si son igual, se comparan los segundos, etc.,
        hasta que la comparación de un resultado diferente de igualidad o se termine la lista.

        El orden de comparación se cambia al opuesto.

        \begin{align*}
            \begin{aligned}
                \forall ~ &x \in \textit{ruta parcial}\\
                          &x \sim \langle v_x, i_x, l_x \rangle \\
                \forall ~ &y \in \textit{ruta parcial}\\
                          &y \sim \langle v_y, i_y, l_y \rangle
            \end{aligned}
            & \quad \implies
            & \begin{cases}
                x < y & \mbox{si } \langle v_x, i_x, l_x \rangle > \langle v_y, i_y, l_y \rangle \\
                x > y & \mbox{si } \langle v_x, i_x, l_x \rangle < \langle v_y, i_y, l_y \rangle \\
                x = y & \mbox{en otro caso}
              \end{cases}
        \end{align*}

\end{enumerate}

\begin{code}
  instance Ord RouteFitness where
    compare (CompleteRoute x)       (CompleteRoute y)       = compare x y
    compare (PartialRoute v1 i1 l1) (PartialRoute v2 i2 l2) =
        compare (v2,i2,l2) (v1,i1,l1)
    compare (CompleteRoute _)        PartialRoute{}         = LT
    compare  PartialRoute{}         (CompleteRoute _)       = GT

\end{code}


\textbf{\Large No se usa}

Función de utilidad: separación de las sub-rutas que se encuentran dentro de un cromosoma.
Separa los puntos de interes como sub-rutas.

\begin{spec}
  splitRoutes :: Labyrinth2D -> Chromosome GA -> [[Gene GA]]
  splitRoutes l = reverse . map reverse . splitRoutes' [] [] l

  splitRoutes' accSplit [] _ [] = accSplit
  splitRoutes' accSplit accRoute _ [] = accRoute:accSplit
  splitRoutes' accSplit accRoute l (h:t) =
    case accRoute of
        _      | h `isPOI` l          -> splitRoutes' (addR accRoute accSplit) [h] l t
        prev:_ | prev `isPOI` l       -> splitRoutes' (addR accRoute accSplit) [h] l t
        prev:_ | (prev, h) `edgeOf` l -> splitRoutes' accSplit (h:accRoute) l t
        _                             -> splitRoutes' (addR accRoute accSplit) [h] l t
    where   addR [] s = s
            addR r s = r:s
\end{spec}


Las pruebas del contenedor \emph{Route} se encuentran en \hstest{Parcial2-Route}{Parcial2/Route.hs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Sub-Rutas}
\label{subsec:subroutes}

Se definen los contenedores de sub-rutas. Se guardan solamente los genes extremos de la ruta
y se proveen funciones de busqueda de sub-ruta en cromosoma. Se implementa asi porque las
cromosomas cambian durante operaciones geneticos, afectando las sub-rutas.

El orden sobre las sub-rutas se define en contexto de dos cromosomas: donor y recipiente.
Se comparan lexográficamente los sigientes valores:
\begin{enumerate}
  \item Número de púntos de interes que tiene el donor pero no el recipiente.
  \item Diferencia entre las longitudes de donor y recipiente.
\end{enumerate}


\begin{code}

  newtype SubRoute = SubRoute (Point2D, Point2D)
        deriving Show

  instance Eq SubRoute where
    (SubRoute p1) == (SubRoute p2) = p1 == p2 || swap p1 == p2

  type Reversed = Bool
  type SubRoutePoints = ([Point2D], Reversed)

  subseq from to = take (to - from + 1) . drop from

  findSubRoute :: SubRoute -> [Point2D] -> Maybe SubRoutePoints
  findSubRoute (SubRoute (x,y)) route =
        case (elemIndex x &&& elemIndex y) route of
                (Just xi, Just yi)  ->  let     rev     = xi > yi
                                                ids'    = (xi,yi)
                                                ids     = if rev    then swap ids'
                                                                    else ids'
                                        in Just (uncurry subseq ids route, rev)
                _                   -> Nothing

  -- Sub-routes, found in 2 points sequences.
  data SubRoutes = SubRoutes    Labyrinth2D
                                SubRoute
                                (Either SubRoutePoints SubRoutePoints)
                                (Either SubRoutePoints SubRoutePoints)
        deriving Show

  instance Eq SubRoutes where
      (SubRoutes _ _ (Left _) _)  ==  (SubRoutes _ _ (Right _) _)  = False
      l@(SubRoutes l1 sr1 _ _)    ==  r@(SubRoutes l2 sr2 _ _)     =
                l1 == l2
            &&  sr1 == sr2
            &&  same subRouteDonor
            &&  same subRouteReceiver
        where same f = ((==) `on` (fst . f)) l r

  subRouteDonor (SubRoutes _ _ donor _)     = case donor of     Left x   -> x
                                                                Right x  -> x

  subRouteReceiver (SubRoutes _ _ _ recei)  = case recei of     Left x   -> x
                                                                Right x  -> x

  subRoutesBoth = subRouteDonor &&& subRouteReceiver

  subRoutesLabyrinth (SubRoutes l _ _ _) = l

  subRoutesPts (SubRoutes _ pts _ _) = pts

  lPairs (f:s:t)    = (f,s) : lPairs (s:t)
  lPairs _          = []

  subRoutesIn   :: Labyrinth2D -> SubRoute
                -> [Point2D] -> [Point2D] -> [SubRoutes]
  subRoutesIn l subRoute pts1 pts2 = do
        route1 <- maybeToList $ findSubRoute subRoute pts1
        route2 <- maybeToList $ findSubRoute subRoute pts2

        let valid = all (`edgeOf` l) . lPairs . fst
            sRoutes = SubRoutes l subRoute
            r1 = Left route1
            r2 = Right route2
            sRoutesLeft = sRoutes r1 r2
            sRoutesRight = sRoutes r2 r1

        case (valid route1, valid route2) of
                (True, True)    -> [sRoutesLeft, sRoutesRight]
                (True, _)       -> return sRoutesLeft
                (_, True)       -> return sRoutesRight
                _               -> []


  subRoutesValue sr@(SubRoutes l _ _ _) = (pois, len)
    where   donor     = fst $ subRouteDonor sr
            receiver  = fst $ subRouteReceiver sr
            countPois = length . filter (`isPOI` l)
            pois = countPois donor - countPois receiver
            len = length receiver - length donor

  instance Ord SubRoutes where compare = compare `on` subRoutesValue

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Algoritmo genético}

\begin{code}

  data GAParams = GAParams  { gaChromGenMaxChainLen  :: Int
                            , gaChromGenMaxChains    :: Int
                            , gaPopulationSize       :: Int
    }

  data GACache = GACache {
        cacheNeighbours :: Map Point2D [Point2D]
    }

  neighboursOf cache point = fromMaybe []
                           $ Map.lookup point (cacheNeighbours cache)

  data GA = GA  { gaLabyri  :: Labyrinth2D
                , gaParams  :: GAParams
                , gaCache   :: GACache
                }
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


\begin{code}
  eDist' = mkDirectDistance $
          \(Point2D (x1,x2)) (Point2D (y1,y2)) ->
                    sqrt (fromIntegral $ abs(x1-x2)^2 + abs(y1-y2)^2)
  eDist  = labyrinthDist eDist'
\end{code}

\crule{1}
\medskip
\noindent
Se define la instancia de la clase \emph{GeneticAlgorithm} para \emph{GA}
empezando con los tipos y siguiendo con los métodos.

> instance GeneticAlgorithm GA where

\begin{enumerate}[(1)]

\item Un \emph{gen} se define como \underline{nodo del laberinto}
      y un \emph{cromosoma} como una \underline{lista de genes}.

      Los cromosomas no deben de tener repeticiones.

>    type Gene GA = Point2D
>    type Chromosome GA = [Point2D]
>    -- listGenes :: Chromosome ga \rightarrow$ [Gene ga]
>    listGenes = id

\item Los valores de aptitud ya fueron descritos previamente.

>    type Fitness GA = RouteFitness

\item Dirección de búsqueda -- minimización.

>    type Target GA = Min

\item Información de entrada para generación de la población --- el laberinto.

>    type InputData GA = Labyrinth2D

\item El resultado es el \underline{mejor cromosoma} obtenido.

>    type ResultData GA = Chromosome GA


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%

\item La \textbf{aptitud de adaptación} fue descrita en subsección \ref{subsec:fitness}.

\begin{code}
     -- fitness :: ga \rightarrow$ Chromosome ga \rightarrow$ Fitness ga
     fitness (GA l _ _) genes =
                    let dists = map (uncurry $ eDist l) (lPairs genes)
                    in if isJust `all` dists
                         then -- is a valid route
                              CompleteRoute . sum $ map fromJust dists
                         else -- is incomplete
                              let valid = filter isJust dists
                                  v = fromIntegral (length valid)
                                    / fromIntegral (length dists)
                                  hasInit = elem (initial l) genes
                                  hasFin = elem (target l) genes
                                  poi = case (hasInit, hasFin) of
                                          (True, True)  -> POIBoth
                                          (True, False) -> POISome POIInit
                                          (False, True) -> POISome POITarget
                                          _             -> POINone
                                  len = sum $ map fromJust valid
                                  in PartialRoute v poi len

\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%

\item Generación de cromosomas aleatorios.

\begin{figure}
    \centering
    \input{MapExampleRaw.tikz}
    \caption{Ejemplo de mapa, inicio: 0--2, meta: 9--3.}
    \label{fig:rawMapExample}
\end{figure}

\begin{figure}
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


\noindent Para mejorar las poblaciones iniciales, las cromosomas se componen de \emph{cadenas}
-- secuencias de genes, que son sub-rutas validas de tamaños diferentes.

En la figura \ref{fig:rawMapExample} se presenta el ejemplo de un mapa y
en la figura \ref{fig:chromosomesMapExample} se presenta un ejemplo de cromosomas generados.

\medskip

>    -- randomChromosome :: ga \rightarrow$ IO (Chromosome ga)
>    randomChromosome (GA l params cache) = do
>       let


Un gen aleatorio se selecciona entre todos los nodos del mapa, y se re-genera en caso de
que este gen ya hubiera sido generado previamente.

\begin{code}
            randPoint   = first (`elemAt` nodes l)
                        . randomR (0, length (nodes l) - 1)
            rand prev = fix $
                        \f g ->
                         let (r, g') = randPoint g
                         in if r `elem` prev  then f g' else (r, g')
\end{code}

Se empieza con la generación del primer punto

\begin{code}
            randChain :: StdGen -> Int -> [Point2D] -> [Point2D]
            randChain g' len prev = nextRand [first'] g''
                where (first', g'') = rand prev g'
\end{code}

Los demas genes se seleccionan desde los vecinos (los nodos directamente conectados)
del gen previo.

Durante la generación de cadenas se consideran las cadenas, generadas previamente,
para no permitir repeticiones de genes.

Si se encontró una repetición, se intenta
\begin{enumerate*}[1)]
  \item buscar otro vecino, que no se repita;
  \item cambiar la dirección de generación;
  \item buscar a otro vecino, con la nueva dirección.
\end{enumerate*}
En caso que todas las opciones fallen, la cadena se queda de tamaño incompleto.


\begin{code}
                      oneOf xs = first (xs !!) . randomR(0, length xs - 1)
                      nextRand = nextRand' False 0
                      nextRand' rev c chain@(h:t) g =
                          let neighbours = cache `neighboursOf` h
                              (r, g') = oneOf neighbours (g :: StdGen)
                              moreTries = c < 5 * length neighbours
                          in if r `elem` prev || r `elem` chain
                           then -- connected to some other chain
                                if moreTries then nextRand' rev (c+1) chain g' -- 1 / 3
                                             else if rev then chain -- incomplete
                                                         else nextRand' True (c+1) chain g' -- 2
                           else if length chain + 1 == len
                            then r:chain -- return
                            else nextRand' rev c (r:chain) g' -- next
\end{code}

Se selecciona aleatoriamente la longetud de \emph{cadenas}.

>       chainLen <- randomRIO (1, gaChromGenMaxChainLen params)

Se selecciona aleatoriamente el número de \emph{cadenas}.

>       chainCnt <- randomRIO (1, gaChromGenMaxChains params)

Se genera el cromosoma.

\begin{code}
        g <- getStdGen
        let f _ = randChain g chainLen
        return $ foldr f [] [1..chainCnt]
\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%


\item La \emph{recombinación} de cromosomas se enfoca en remplacar las
      malas sub-rutas o extender rutas existentes.

      Aquí solamente se define la recombinación de dos cromosomas, su selección
      será descrita en la subsección \ref{subsec:gaRun}.

      \noindent Se replazan los ``hoyos'' de la siguente manera:

      \begin{enumerate}
        \item Se seleccionan los genes $\lbrace c \rbrace$, miembros de ambos cromosomas.

        \item Para ambos cromosomas se encuentran \emph{sub-rutas intercambiables}:
              \begin{align*}
                \forall ~&x \in \lbrace c \rbrace \\
                         &y \in \lbrace c \rbrace \quad \implies \\
                         &
                \begin{cases}
                    \text{secuencia } \lbrace r_i \rbrace_{i=1}^{N_r}
                        & \mbox{si } \begin{aligned}
                            \forall &~r_{j-1}, r_j \in \lbrace r_i \rbrace_{i=1}^{N_r} \\
                            \exists & \text{ arista entre } r_{j-1} \text{ y } r_j
                          \end{alaigned} \\
                        \lbrace\rbrace & \mbox{en otro caso}
                \end{cases}
              \end{align*}

              Se guarda también la dirección de las sub-rutas para ambos cromosomas.

        \item Se aplica el remplazamiento para todas las rutas intercambiables ordenadas
                (fue descrita en subsección \ref{subsec:subroutes}).
              Se remplazan las sub-rutas no existentes por las existentes;
              y se remplazan las existentes por mas cortas.

              El remplazamiento se aplica solamente si
              \begin{enumerate*}[1)]
                \item los genes en cuestion no fueron eliminados con los remplazamientos previos;
                \item remplazamiento no creará genes duplicados;
                \item no disminuye el número de puntos de interes.
              \end{enumerate*}

         \item Se devuelve el par de cromosomas remplazados.

      \end{enumerate}

      \medskip
      \noindent Se extienden los extremos del recipiente con los del donador:
      \begin{enumerate}
        \item Se encuentran los extremos mas cortos del donador: entre todos los
          puntos $\left{ c \right}$ se seleccionan los con menor y
          mayor índices en el cromosoma donador. Si las sub-rutas entre los
          puntos extremos y los índices correspondientes están \emph{validas} -- se guardan.
        \item Se encuentran los extremos, correspondientes a los puntos, seleccionados en
          el punto previo. Se guardan si son \emph{invalidas}.
        \item Se remplazan los extremos correspondientes del recipiente por los
          del donador (si fueron guardados ambos).
      \end{enumerate}


      \begin{figure}
        \centering
        \caption{ Recombinación de cromosomas, marcados {\color{violet} •} y {\color{orange} •}
                  en la figura \ref{fig:chromosomesMapExample}.
                }

          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletOrangeSources.tikz} }
            \caption{ Los remplazamientos. }
            \label{fig:crossVOsrc}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletOrangeRoute0.tikz} }
            \caption{ Remplazamiento {\color{orange} •} $\rightarrow$ {\color{violet} •} \#1. }
            \label{fig:crossVOr0}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletOrangeRoute1.tikz} }
            \caption{ Remplazamiento {\color{violet} •} $\rightarrow$ {\color{orange} •}. }
            \label{fig:crossVOr1}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletOrangeRoute2.tikz} }
            \caption{ Remplazamiento {\color{orange} •} $\rightarrow$ {\color{violet} •} \#2. }
            \label{fig:crossVOr2}
          \end{subfigure}

        \label{fig:crossVO}
      \end{figure}


      \begin{figure}
        \centering
        \begin{subfigure}[b]{\textwidth}
            \input{CrossoverVioletOrangeChildrenFst.tikz }
            \caption{ Resultados de recombinación de sub-rutas en cromosoma {\color{violet} •} desde
                      {\color{orange} •}.
                    }
            \label{fig:crossOVchildren}
        \end{subfigure}
        \\
        \begin{subfigure}[b]{\textwidth}
            \input{CrossoverVioletOrangeChildrenSnd.tikz }
            \caption{ Resultados de recombinación de sub-rutas en cromosoma {\color{orange} •} desde
                      {\color{violet} •}.
                    }
            \label{fig:crossVOchildren}
        \end{subfigure}

        \caption{}
      \end{figure}


      \begin{figure}
        \centering
        \caption{ Recombinación de cromosomas, marcados {\color{violet} •} y {\color{blue} •}
                  en la figura \ref{fig:chromosomesMapExample}.
                }

          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueSources.tikz} }
            \caption{ Los remplazamientos. }
            \label{fig:crossVBsrc}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute0.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr0}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute1.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr1}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute2.tikz} }
            \caption{ Remplazamiento {\color{violet} •} $\rightarrow$ {\color{blue} •}. }
            \label{fig:crossVBr2}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute3.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr3}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute4.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr4}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute5.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr5}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute6.tikz} }
            \caption{ Remplazamiento {\color{violet} •} $\rightarrow$ {\color{blue} •}. }
            \label{fig:crossVBr6}
          \end{subfigure}
        \\
          \begin{subfigure}[b]{\textwidth}
            \fbox{ \resizeInput{CrossoverVioletBlueRoute7.tikz} }
            \caption{ Remplazamiento {\color{blue} •} $\rightarrow$ {\color{violet} •}. }
            \label{fig:crossVBr7}
          \end{subfigure}

        \label{fig:crossVB}
      \end{figure}


      \begin{figure}
        \centering
        \begin{subfigure}[b]{\textwidth}
            \input{CrossoverVioletBlueChildrenFst.tikz }
            \caption{ Resultados de recombinación de sub-rutas en cromosoma {\color{blue} •} desde
                      {\color{violet} •}.
                    }
            \label{fig:crossVBchildren}
        \end{subfigure}
        \\
        \begin{subfigure}[b]{\textwidth}
            \input{CrossoverVioletBlueChildrenSnd.tikz }
            \caption{ Resultados de recombinación de sub-rutas en cromosoma {\color{violet} •} desde
                      {\color{blue} •}.
                    }
            \label{fig:crossBVchildren}
        \end{subfigure}

        \caption{}
      \end{figure}

\medskip

\begin{code}

     type CrossoverDebug GA =  (  [SubRoutes]
                               ,  (  [(SubRoutes, Maybe [Point2D])]
                                  ,  (  (Maybe [Point2D], Maybe [Point2D])
                                     ,  (Maybe [Point2D], Maybe [Point2D])
                                     )
                                  )
                               )

     -- crossover' :: ga \rightarrow$ Chromosome ga \rightarrow$ Chromosome ga
     -- \rightarrow$ ((Chromosome ga, Chromosome ga), CrossoverDebug ga)

     crossover' (GA l _ _) ch1 ch2 = (extended, debugs)
        where   debugs = (subRoutes, (debugAcc, extDebug))

                samePoints x y = let  set1 = Set.fromList x
                                      set2 = Set.fromList y
                                in Set.toList $  Set.intersection set1 set2

                cs = samePoints ch1 ch2
                sRoutes' = do   x <- cs
                                y <- cs
                                let sr = SubRoute (x,y)
                                if x == y then []
                                else subRoutesIn l sr ch1 ch2

                subRoutes = sortWith Down . nub $ sRoutes'


                replaceList what with l =
                        let (Just il, Just ir) = (  (head what `elemIndex`) &&&
                                                    (last what `elemIndex`)) l
                            (left, _)   = splitAt il l
                            (_, right)  = splitAt (ir+1) l
                        in left ++ with ++ right

                replaceSafe lab what with l =
                        let candidate = replaceList what with l
                            poisTarget = length $ filter (`isPOI` lab) what
                            poisSrc    = length $ filter (`isPOI` lab) with
                        in  if poisSrc < poisTarget || candidate /= nub candidate
                            then Nothing else Just candidate

                tryReplace chrom _ _ [] debugAcc = (chrom, debugAcc)
                tryReplace chrom thisSide getThatSide (sr:srs) debugAcc =
                    tryReplace res thisSide getThatSide srs acc'
                    where  acc' =  if mbRes == Just chrom then debugAcc
                                   else (debug,mbRes):debugAcc
                           (mbRes, debug) = fromMaybe (Just chrom, sr) res'
                           res = fromMaybe chrom mbRes
                           res' = case sr of
                             SubRoutes l pts src' target' | thisSide target' ->
                                    do  target <- findSubRoute pts chrom
                                        let  src = getThatSide src'
                                             rev = snd src `xor` snd target

                                             debug = SubRoutes l pts
                                                        (fmap (const src) src')
                                                        (fmap (const target) target')

                                        return  ( replaceSafe l  (fst target)
                                                                 (fst src)
                                                                 chrom
                                                , debug )
                             _ -> Nothing

                (replaced1, debugAcc') = tryReplace  ch1
                                                     isLeft
                                                     (\(Right x) -> x)
                                                     subRoutes
                                                     []
                (replaced2, debugAcc)  = tryReplace  ch2
                                                     isRight
                                                     (\(Left x) -> x)
                                                     subRoutes
                                                     debugAcc'

                tryExtend donor receiver = (res, deb)
                    where  cmp x   = compare `on` (`elemIndex` x)
                           valid = all (`edgeOf` (l :: Labyrinth2D)) . lPairs
                           inCase x c = if c x then Just x else Nothing

                           cs = samePoints donor receiver


                           left   = minimumBy (cmp donor) cs
                           right  = maximumBy (cmp donor) cs

                           Just diLeft   = left `elemIndex` donor
                           Just diRight  = right `elemIndex` donor

                           drLeft   = subseq 0 diLeft donor
                           drRight  = subseq diRight (length donor -1) donor

                           mbdRLeft   = drLeft `inCase` valid
                           mbdRRight  = drRight `inCase` valid

                           Just riLeft   = left `elemIndex` receiver
                           Just riRight  = right `elemIndex` receiver

                           rrLeft   = subseq 0 riLeft receiver
                           rrRight  = subseq riRight (length receiver -1) receiver

                           mbrRLeft   = rrLeft `inCase` (not . valid)
                           mbrRRight  = rrRight `inCase` (not . valid)

                           extend (Just src) (Just target) chrom =
                                replaceSafe l target src chrom
                           extend _ _ _ = Nothing

                           extLeft   = extend mbdRLeft mbrRLeft receiver
                           extRight  = extend mbdRLeft mbrRLeft
                                     $ fromMaybe receiver extLeft

                           debug = (extLeft, extRight)
                           result =  (receiver `fromMaybe` extLeft)
                                     `fromMaybe` extRight

                           res = if null cs then receiver else result
                           deb = if null cs then (Nothing, Nothing) else debug

                (extended1, extDebug1) = tryExtend replaced2 replaced1
                (extended2, extDebug2) = tryExtend replaced1 replaced2

                extDebug = (extDebug1, extDebug2)
                extended = (extended1, extended2)

\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%


\item ?

>    -- mutate :: Chromosome ga \rightarrow$ Chromosome ga

\item ?

>    -- stopCriteria :: [Fitness ga] \rightarrow$ Bool

\item ?

>    -- newGA :: InputData ga \rightarrow$ ga


\end{enumerate}




\subsection{??? gaRun ???}
\label{subsec:gaRun}

\begin{code}

  instance RunGA GA [Point2D] [Point2D] RouteFitness Min where
    type DebugData GA = ()


\end{code}


\newpage

{\Huge \color{red} Esto es un reporte preliminar }

\begin{note}
  La preferencia debe ser dada a las rutas que contienen un de los puntos de interes (inicio, meta).

  La mutación debe extender/remplacar un gen al inicio/meta si $\exists$ una ruta directa.

\end{note}





\end{document}

