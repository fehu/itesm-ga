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
  import Control.Monad (replicateM)
  import Control.Monad.Fix

  import Data.IORef
  import Data.Tuple (swap)
  import Data.List (elemIndex, sort, nub, minimumBy, maximumBy)
  import Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)
  import Data.Bits (xor)
  import Data.Either (isLeft, isRight, Either(..))
  import Data.Function (on)
  import Data.Ratio
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

  labyrinthPOIs l = map ($ l) [initial, target]

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

\section{Implementación I}

Misceláneo.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Lectura de mapas}

Se utiliza un mapa 2D:

> newtype Point2D = Point2D (Int, Int) deriving (Eq, Ord)
> instance Show Point2D where
>   show (Point2D (x,y)) = show x ++ "-" ++ show y

> type Labyrinth2D = Labyrinth Point2D


La lectura del archivo del mapa se encuentra en
\hssrc{Parcial2-ReadLabyrinth}{Parcial2/ReadLabyrinth.hs}.
Aquí se presenta la construcción del grafo a partir del mapa leído.


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
                        (Set.fromList $ map (first get . second get) conn)
                        (get i)
                        (get t)

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Implementación II}

Se definan las operaciones \emph{atómicas} -- sobre genes y cromosomas,
y los conceptos relacionados.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Adaptación}
\label{subsec:fitness}


El valor de \emph{aptitud de adaptación} se llamará \emph{ruta} y se define
para permitir distinguir fácilmente los dos tipos de rutas
(encodificadas en los cromosomas) posibles:

\begin{itemize}
  \item \textbf{Ruta completa}: es una ruta valida
    ($\exists$ una conexión entre cada par de genes adjuntos) que contiene
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
            {\text{número de aristas total}}$;

            \textit{aristas existentes ---
                    aristas que existen entre los pares de genes adjuntos;}

        \item los \emph{puntos de interés} que contiene la ruta;
        \item el número de genes;
        \item la longitud sumatoria de las sub-rutas en el cromosoma.

    \end{enumerate}

\end{itemize}



\begin{code}

  data POI  = POIInit | POITarget deriving (Eq, Ord, Enum, Show)
  data POIs = POINone | POISome POI | POIBoth deriving (Eq, Show)

  instance Ord POIs where
    x `compare` y = poisIntVal x `compare` poisIntVal y

  poisIntVal POINone = 0
  poisIntVal (POISome _) = 1
  poisIntVal POIBoth = 2


  data RouteFitness =
      CompleteRoute { routeLength :: Double }
    | PartialRoute  { partialValidess :: Double
                    , partialPOI      :: POIs
                    , partialPaths    :: Int
                    , partialLength   :: Double
                    }
    deriving (Eq, Show)

\end{code}

\noindent Para la busqueda de la ruta mas corta,
se define el orden sobre las rutas de tal manera, que
una lista de \emph{rutas}, ordenada ascendentemente,
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

  \item Dos \emph{rutas parciales} se comparan por los valores,
        producidos desde sus \underline{4 componentes}.
        $ \langle v, i, p, l \rangle \Rightarrow  p \times v \times (\texttt{int } i + 1) - l$


\end{enumerate}

\begin{code}
  instance Ord RouteFitness where
    compare (CompleteRoute x)       (CompleteRoute y)       = compare x y
    compare (PartialRoute v1 i1 p1 l1) (PartialRoute v2 i2 p2 l2) =
        compare (fromIntegral p1 * v1 * (poisIntVal i2 + 1) - l1)
                (fromIntegral p2 * v2 * (poisIntVal i2 + 1) - l2)
    compare (CompleteRoute _)        PartialRoute{}         = LT
    compare  PartialRoute{}         (CompleteRoute _)       = GT

\end{code}


Función de utilidad: separación de las sub-rutas que se encuentran dentro de un cromosoma.
Separa los puntos de interes como sub-rutas.

\begin{code}
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
\end{code}


La función misma se definirá en subsección \ref{subsec:ga}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Cromosomas aleatorios}
\label{subsec:random}

Un gen aleatorio se selecciona entre todos los nodos del mapa, y se re-genera en caso de
que este gen ya hubiera sido generado previamente.

\begin{code}
  randPoint l  = first (`elemAt` nodes l)
               . randomR (0, length (nodes l) - 1)
  randUnique l prev = fix $
              \f g ->
               let (r, g') = randPoint l g
               in if r `elem` prev  then f g' else (r, g')
\end{code}

Se empieza con la generación del primer punto

\begin{code}
  randChain :: GA -> StdGen -> Int -> [Point2D] -> [Point2D]
  randChain ga g' len prev = nextRand ga [first'] g''
      where (first', g'') = randUnique (gaLabyri ga) prev g'
\end{code}

Los demás genes se seleccionan desde los vecinos (los nodos directamente conectados)
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
            nextRand ga = nextRand' ga False 0
            nextRand' ga rev c chain@(h:t) g =
                let neighbours = gaCache ga `neighboursOf` h
                    (r, g') = oneOf neighbours (g :: StdGen)
                    moreTries = c < 5 * length neighbours
                in if r `elem` prev || r `elem` chain
                 then -- connected to some other chain
                      if moreTries then nextRand' ga rev (c+1) chain g' -- 1 / 3
                                   else if rev then chain -- incomplete
                                               else nextRand' ga True (c+1) chain g' -- 2
                 else if length chain + 1 == len
                  then r:chain -- return
                  else nextRand' ga rev c (r:chain) g' -- next
\end{code}


\noindent La generación de cromosoma completa se presentará en subsección \ref{subsec:ga}.

\crule{0.75}

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
                            las conexiones que existen en varios cromosomas)}
            }
    \label{fig:chromosomesMapExample}
\end{figure}


\noindent Para mejorar las poblaciones iniciales, las cromosomas se componen de \emph{cadenas}
-- secuencias de genes, que son sub-rutas validas de tamaños diferentes.

En la figura \ref{fig:rawMapExample} se presenta el ejemplo de un mapa y
en la figura \ref{fig:chromosomesMapExample} se presenta un ejemplo de cromosomas generados.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Sub-Rutas}
\label{subsec:subroutes}

Se definen los contenedores de sub-rutas. Se guardan solamente los genes extremos de la ruta
y se proveen funciones de búsqueda de sub-ruta en cromosoma. Se implementa así porque las
cromosomas cambian durante operaciones genéticos, afectando las sub-rutas.

El orden sobre las sub-rutas se define en contexto de dos cromosomas: donador y recipiente.
Se comparan lexográficamente los siguientes valores:
\begin{enumerate}
  \item Número de puntos de interés que tiene el donador pero no el recipiente.
  \item Diferencia entre las longitudes de donador y recipiente.
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

\subsection{Recombinación de cromosomas}
\label{subsec:crossover}

Aquí solamente se define la recombinación de dos cromosomas, su selección
será descrita en la subsección \ref{subsec:gaSelect}.


\subsubsection{Remplazamiento}

Se remplazan los ``hoyos'' de la siguiente manera:

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
        \item los genes en cuestión no fueron eliminados con los remplazamientos previos;
        \item remplazamiento no creará genes duplicados;
        \item no disminuye el número de puntos de interés.
      \end{enumerate*}

 \item Se devuelve el par de cromosomas remplazados.

\end{enumerate}

\subsubsection{Extensión de extremos}

Se extienden los extremos del recipiente con los del donador:
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


\crule{1}

\noindent  Se definen unas funciones de utilidad; la definición de `'crossover'' se encuentra en
    subsección \ref{subsec:ga}.

\begin{code}
  replaceList :: (Eq a) => [a] -> [a] -> [a] -> Maybe [a]
  replaceList what with l =
          let ids = (  (head what `elemIndex`) &&&
                       (last what `elemIndex`)) l
          in case ids of
                (Just il, Just ir) ->
                     let  (left, _)   = splitAt il l
                          (_, right)  = splitAt (ir+1) l
                     in Just $ left ++ with ++ right
                _ -> Nothing

\end{code}
\begin{code}

  replaceSafe  :: Labyrinth2D -> [Point2D] -> [Point2D] -> [Point2D]
               -> Maybe [Point2D]
  replaceSafe lab what with l =
          let candidate = replaceList what with l
              poisTarget = length $ filter (`isPOI` lab) what
              poisSrc    = length $ filter (`isPOI` lab) with
          in do  c <- candidate
                 if poisSrc < poisTarget || c /= nub c
                 then Nothing else Just c

\end{code}
\begin{code}

  type ReplaceDebug = [(SubRoutes, Maybe [Point2D])]

  tryReplace  :: [Point2D]
              -> (Either SubRoutePoints SubRoutePoints -> Bool)
              -> (Either SubRoutePoints SubRoutePoints -> SubRoutePoints)
              -> [SubRoutes]
              -> ReplaceDebug
              -> ([Point2D], ReplaceDebug)
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

\end{code}
\begin{code}

  samePoints x y = let  set1 = Set.fromList x
                        set2 = Set.fromList y
                  in Set.toList $  Set.intersection set1 set2



  type ExtendDebug' = (Maybe [Point2D], Maybe [Point2D])
  type ExtendDebug = (ExtendDebug', ExtendDebug')

  tryExtend  :: Labyrinth2D -> [Point2D] -> [Point2D]
             -> ([Point2D], ExtendDebug')
  tryExtend l donor receiver = (res, deb)
    where  cmp x   = compare `on` (`elemIndex` x)
           valid = all (`edgeOf` l) . lPairs
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

\end{code}


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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Mutación de cromosomas}
\label{subsec:mutation}


La mutación de cromosomas consiste de varios operaciones,
que se dividen en los que cambian un gen o una sub-ruta.

\begin{code}

  type MutateSubRoute  = [Point2D] -> IO [Point2D]
  type MutateGene      = [Point2D] -> Point2D -> IO Point2D


  -- Choose randomly an element from a list.
  randChoice = fmap fst . randChoice'
  randChoice' xs = do ind <- randomRIO (0, length xs - 1)
                      return $ (xs !!) &&& id $ ind

  randChoiceSafe [] = return Nothing
  randChoiceSafe xs = Just <$> randChoice xs

  randRange xs = do
        (_,i1) <- randChoice' xs
        (_,i2) <- randChoice' xs

        return $ if i2 > i1 then (i1,i2) else (i2,i1)

\end{code}

Se definen las siguientes \emph{operaciones sobre sub-rutas}: {\color{red} \Large desactivados}
\begin{itemize}
  \item Cambia una sub-ruta valida por otra aleatoria (valida),
        con misma longitud.

\begin{code}
  mutSubRouteSame :: GA -> MutateSubRoute
  mutSubRouteSame ga ch = do
      print "mutSubRouteSame"
      let srs = splitRoutes (gaLabyri ga) ch
      (sr, sri) <- randChoice' srs
      gen <- getStdGen
      let len = length sr
          rChain = randChain ga gen len []

      return . concat  $   subseq 0 (sri-1) srs
                       ++  rChain
                       :   subseq (sri+1) len srs
\end{code}

  \item Cambia una sub-ruta, aleatoriamente seleccionada, por otra(s) aleatoria(s).

\begin{code}
  mutSubRouteAny :: GA -> MutateSubRoute
  mutSubRouteAny ga ch = do
    print "mutSubRouteAny"
    let maxGen  = gaMutateMaxChainsGen $ gaParams ga
    let maxLen  = gaMutateMaxChainLen $ gaParams ga
    n <- randomRIO (1, maxGen)

    cut <- flip (uncurry subseq) ch <$> randRange ch

    paste <- sequence $ do  _ <- [1..n]
                            return $ do  len <- randomRIO (1, maxLen)
                                         gen <- getStdGen
                                         return $ randChain ga gen len []
    return . fromJust $ replaceList cut (concat paste) ch

\end{code}

\end{itemize}

Se definen las siguientes \emph{operaciones sobre genes} con la probabilidad de aplicación:
\begin{itemize}[leftmargin=2.5cm]
  \item[$P=0.01$ ---] Cambia un gen a un de los puntos de interés (inicio/meta),
    si todavía no existe en el cromosoma.

\begin{code}

  mutGenePOI = (0.01, mutGenePOI')
  mutGenePOI' :: GA -> MutateGene
  mutGenePOI' ga ch gene  =    fromMaybe gene
                          <$>  randChoiceSafe notFound
        where  l = gaLabyri ga
               notFound = filter (not . (`elem` ch)) $ labyrinthPOIs l

\end{code}

  \item[$P=0.005$ ---] Cambia un gen a un aleatorio.

\begin{code}

  mutGeneAny = (0.005, mutGeneAny')
  mutGeneAny' :: GA -> MutateGene
  mutGeneAny' ga chrom gene = do
        gen <- getStdGen
        let (gene', _) = randUnique (gaLabyri ga) chrom gen
        return gene'

\end{code}

\end{itemize}

\noindent La aplicación de las mutaciones se encuentra en subsección \ref{subsec:ga}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Algoritmo genético}
\label{subsec:ga}

\begin{code}

  data GAParams = GAParams  { gaChromGenMaxChainLen  :: Int
                            , gaChromGenMaxChains    :: Int

                            , gaMutateMaxChainsGen   :: Int
                            , gaMutateMaxChainLen    :: Int

                            , gaMaxUnchangedIter     :: Int
                            , gaMaxIters             :: Int

                            , gaSelIntactFrac        :: Rational
                            , gaSelCrossoverFrac     :: Rational
                            , gaSelMutateFrac        :: Rational
    }
    deriving Show

  data GACache = GACache {
        cacheNeighbours    :: Map Point2D [Point2D]
      , cacheBestRepeats   :: IORef Int
      , cacheBestFitness   :: IORef (Maybe RouteFitness)
      , cacheIter          :: IORef Int
      , cacheSelIndexGen   :: IORef ([Int] -> IO Int)
    }

  cachedBestFit :: GACache -> IO (Maybe RouteFitness)
  cachedBestFit = readIORef . cacheBestFitness

  setCachedBestFit = writeIORef . cacheBestFitness

  cachedRepeat = readIORef . cacheBestRepeats
  affectCachedRepeat = modifyIORef . cacheBestRepeats

  cachedIter = readIORef . cacheIter
  affectCachedIter = modifyIORef . cacheIter

  cachedIdxGen = readIORef . cacheSelIndexGen
  setCachedIdxGen = writeIORef . cacheSelIndexGen

  neighboursOf cache point = fromMaybe []
                           $ Map.lookup point (cacheNeighbours cache)

  data GA = GA  { gaLabyri  :: Labyrinth2D
                , gaParams  :: GAParams
                , gaCache   :: GACache
                }
\end{code}



\noindent
Se define la métrica sobre los puntos del grafo:
$$
  \mathrm{dist}(p_1, p_2) = \begin{cases}
       \mathit{Just}~ d_E(p_1, p_2)
    &  \mbox{si } \exists \text{ arista, conectando } p_1 \text{ y } p_2
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

\item Información de entrada para generación de la población --- el laberinto y los parámetros.

>    type InputData GA = (Labyrinth2D, GAParams)

\item El resultado es el \underline{mejor cromosoma} obtenido.

>    type ResultData GA = Chromosome GA


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%

\item La \textbf{aptitud de adaptación} fue descrita en subsección \ref{subsec:fitness}.

\begin{code}
     -- fitness :: ga \rightarrow$ Chromosome ga \rightarrow$ Fitness ga
     fitness (GA l _ _) genes =
                    let dists = map (uncurry $ eDist l) (lPairs genes)
                    in if isJust `all` dists  && initial l  `elem` genes
                                              && target  l  `elem` genes
                         then -- is a valid route
                              CompleteRoute . sum $ map fromJust dists
                         else -- is incomplete
                              let valid = filter isJust dists
                                  plen = length dists
                                  v  = fromIntegral (length valid)
                                     / fromIntegral plen
                                  hasInit = elem (initial l) genes
                                  hasFin = elem (target l) genes
                                  poi = case (hasInit, hasFin) of
                                          (True, True)  -> POIBoth
                                          (True, False) -> POISome POIInit
                                          (False, True) -> POISome POITarget
                                          _             -> POINone
                                  len = sum $ map fromJust valid
                                  v' = if isNaN v then 1 else v
                                  in PartialRoute v' poi plen len

\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%

\item Generación de cromosomas aleatorios.

>    -- randomChromosome :: ga \rightarrow$ IO (Chromosome ga)
>    randomChromosome ga@(GA _ params _) = do

Se selecciona aleatoriamente la longetud de \emph{cadenas}.

>       chainLen <- randomRIO (1, gaChromGenMaxChainLen params)

Se selecciona aleatoriamente el número de \emph{cadenas}.

>       chainCnt <- randomRIO (1, gaChromGenMaxChains params)

Se genera el cromosoma.

\begin{code}
        g <- getStdGen
        let f _ = randChain ga g chainLen
        return $ foldr f [] [1..chainCnt]
\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%


\item La \emph{recombinación} de cromosomas se enfoca en remplazar las
      malas sub-rutas o extender rutas existentes.



\begin{code}

     type CrossoverDebug GA =  (  [SubRoutes]
                               ,  (ReplaceDebug,  ExtendDebug)
                               )

     -- crossover' :: ga \rightarrow$ Chromosome ga \rightarrow$ Chromosome ga
     -- \rightarrow$ ((Chromosome ga, Chromosome ga), CrossoverDebug ga)

     crossover' (GA l _ _) ch1 ch2 = (extended, debugs)
        where   debugs = (subRoutes, (debugAcc, extDebug))

                cs = samePoints ch1 ch2
                sRoutes' = do   x <- cs
                                y <- cs
                                let sr = SubRoute (x,y)
                                if x == y then []
                                else subRoutesIn l sr ch1 ch2

                subRoutes = sortWith Down . nub $ sRoutes'

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

                (extended1, extDebug1) = tryExtend l replaced2 replaced1
                (extended2, extDebug2) = tryExtend l replaced1 replaced2

                extDebug = (extDebug1, extDebug2)
                extended = (extended1, extended2)

\end{code}


%%%%%%%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%


\item La \emph{mutación} funciona en la siguiente manera:
      \begin{enumerate}
        \item Se escoge y se aplique una de las \emph{operaciones sobre sub-rutas}.
        \item Para cada gen del resultado del punto previo,
              se aplican (todas) las \emph{operaciones sobre genes},
              con su probabilidad asignada.
      \end{enumerate}

\begin{code}

     -- mutate :: ga \rightarrow$ Chromosome ga \rightarrow$ IO (Chromosome ga)
     mutate ga chrom = do
        mutateGenes chrom geneMuts

        where  subRouteMuts  = [] -- TODO: disabled
               geneMuts      = [ mutGenePOI, mutGeneAny ]

               mutateGenes ch = mutateGenes' ch []
               mutateGenes' [] mutated _ = return mutated
               mutateGenes' (gene:t) mutated muts =
                    do  g <- gene'
                        mutateGenes' t (mutated ++ [g]) muts

                    where  -- chrom g = mutated ++ g:t
                           mutF (p, mut) g' = do
                                 d <- randomIO :: IO Double
                                 g <- g'
                                 if p < d then mut ga mutated g else return g
                           gene' = foldr mutF (return gene) muts



\end{code}


\item \emph{Criterio de parada} se selecciona, considerando que no
  se conoce la longitud de ruta aceptable.
  Esto no permita establecer un criterio exacto.

  Es porque el criterio se establece sobre el
  \emph{cambio del mejor valor de adaptación en las últimas iteraciones}.

  \noindent También se utiliza como criterio adicional el \emph{número máximo de iteraciones}.

\begin{code}
     -- stopCriteria :: ga \rightarrow$ [Fitness ga] \rightarrow$ IO Bool
     stopCriteria ga fitness = do
            let  cache  = gaCache ga
                 raiseCount = cache `affectCachedRepeat` (+1)

                 best   = head fitness
            best' <- cachedBestFit cache

            if Just best == best' then raiseCount
            else cache `setCachedBestFit` Just best

            repCount <- cachedRepeat cache
            iter <- cachedIter cache

            return  $   repCount  >= gaMaxUnchangedIter (gaParams ga)
                    ||  iter      >= gaMaxIters (gaParams ga)

\end{code}


\item Creación de instancia de \emph{GA}. Se crea el cache.

\begin{code}

     -- newGA :: InputData ga \rightarrow$ IO ga
     newGA (labyrinth, params) = do
        let  nodes' = Set.toList $ nodes labyrinth

             neighbours = Map.fromList $ do
                node <- nodes'
                let connected = filter ((`edgeOf` labyrinth) . (,) node) nodes'
                return (node, connected)

        bestRepeats  <- newIORef 0
        bestFitness  <- newIORef Nothing
        iter         <- newIORef 0
        selIndexGen  <- newIORef (const $ return (-1))

        let cache = GACache  neighbours
                             bestRepeats
                             bestFitness
                             iter
                             selIndexGen

        return $ GA labyrinth params cache

\end{code}

\end{enumerate}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Implementación III}

El proyecto separa las operaciones genéticos \emph{atómicos}, definidas en clase \emph{GA},
de las \emph{masivas}, definidas en clase \emph{RunGA}.

Las operaciones \emph{masivas} -- son las que trabajar con entera población:
\begin{enumerate*}[1)]
  \item selección de cromosomas para operaciones genéticos, generación de población inicial;
  \item generación de población inicial;
  \item ejecución de las iteraciones.
\end{enumerate*}

Las últimas dos están definidas en \hssrc{GeneticAlgorithm}{GeneticAlgorithm},
pero su código se presentara incluso en subsección \ref{subsec:gaRun}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{ Operaciones de selección }
\label{subsec:gaSelect}

Para preservación del tamaño de población, la unión de los tres
siguientes selecciones debe siempre ser de mismo tamaño que la población,
de la cual hubieron sidos seleccionados.

Se uso un concepto \emph{Assessed}, el cual encapsula una lista de
cromosomas con sus correspondientes valores de adaptación.
Está siempre ordenada ascendentemente, para que los mejores
cromosomas (con menor valor de adaptación) estén en el principio.

Se intente usar \emph{RouteFitness} sin transformarlos en un valor numérico;
para esto se defina la función de densidad de probabilidad de selección
de un cromosoma, dependiendo de su índice en la lista.

\begin{align*}
  P'_i &= \dfrac{0.1}{i} \\
  P_i  &= \dfrac{P'_i}{ \sum\limits_j P'_j }
\end{align*}

\begin{tikzpicture}
  \edef\N{200}

  \draw[->] (0,0) -- (10,0) node[right] {$i$};
  \draw[->] (0,0) -- (0,10) node[above] {$P'_i$};

  \foreach \x   [evaluate=\x as \i using \x*\N*0.1] in {1,2,...,10}
	\draw (\x,1pt) -- (\x,-3pt)
	node[anchor=north] {\pgfmathprintnumber[fixed,precision=0]{\i}};

  \foreach \y [evaluate=\y as \i using \y*0.01] in {0,...,10}
	\draw (1pt,\y) -- (-3pt,\y)
		node[anchor=east] {\pgfmathprintnumber[fixed,precision=3]{\i}};

  \draw[xscale=10/\N, yscale=100, domain=1:\N,variable=\x,blue]
    plot ({\x},{0.1 / \x});
\end{tikzpicture}

\bigskip

\begin{code}

  assessedProb' i  = 0.1 / fromInteger i

  assessedProbs n  = map (/ isum) is
        where  is = assessedProb' <$> [1..n]
               isum = sum is


  assessedRandIndexGen  :: Int    -- Population size.
                        -> [Int]  -- Previous indices.
                        -> IO Int -- Random index.
  assessedRandIndexGen n = randIdx
    where  probs    = assessedProbs $ toInteger n
           -- accumulated probabilities
           accProbs = snd $ foldr  (\p (p', acc) -> (p'+p, p'+p : acc))
                                   (0,[]) probs
           -- select id, given a number in [0,1]
           selIdx :: Double -> Int
           selIdx d =  let less = (< d) `filter` accProbs
                       in n - length less -1
           -- Generate a random assessed index,
           -- without repeating elems of `prev`.
           randIdx prev = do
                r <- selIdx <$> randomIO
                if r `elem` prev  then randIdx prev
                                  else return r

  replicateRandIndices prev' n igen = foldr f (return prev') [1..n]
        where f _ prev = do p <- prev
                            i <- igen p
                            return $ i:p

  assessedRand selFrac ga assessed = do
        iGen <- cachedIdxGen $ gaCache ga
        let pSize = fromIntegral $ popSize assessed
            frac  = selFrac (gaParams ga)
            count = round $ pSize * frac

        ids <- replicateRandIndices [] count iGen --replicateM count iGen

        let ua = map fst $ unwrapAssessed assessed
        return $ map (ua !!) ids

\end{code}

\crule{0.75}
\bigskip

\begin{code}

  instance RunGA GA [Point2D] [Point2D] RouteFitness Min where
    type DebugData GA = Assessed [Point2D] RouteFitness


\end{code}

\noindent Se define \underline{selección de cromosomas}:
\begin{enumerate}[(a)]

  \item Que \underline{pasan al siguiente generación intactos}.

    La fracción establecida de la población previa se escoge
    aleatoriamente (con repeticiones).

\begin{code}

    selectIntact = assessedRand gaSelIntactFrac

\end{code}

  \item Para la \underline{recombinación}.

        Se escogen aleatoriamente una fraccione establecida de la población previa
        y se divida en dos partes iguales.

\begin{code}

    selectCrossover ga assessed = zip <$> rand <*> rand
        where rand = assessedRand  (flip (/) 2 . gaSelCrossoverFrac)
                                   ga assessed

\end{code}


  \item Para la \underline{mutación}.

        Se escoge aleatoriamente una fracción establecida de la población previa.

\begin{code}

    selectMutate = assessedRand gaSelMutateFrac

\end{code}

\end{enumerate}


\medskip
\noindent El resultado es el cromosoma con mejor valor de adaptación.

\begin{code}

    selectResult _ a@(Assessed (h:_)) = (fst h, a)

\end{code}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Ejecución de algoritmo genético}
\label{subsec:gaRun}

\noindent Se implementan los actualizaciones de cache.

\begin{code}

    initHook ga pop = setCachedIdxGen (gaCache ga) (assessedRandIndexGen pop)

    iterationHook ga = do  affectCachedIter (gaCache ga) (+1)
                           i <- cachedIter (gaCache ga)
                           putStrLn $ "iteration #" ++ show i

\end{code}


\noindent Se presenta aquí parte de código desde \hssrc{GeneticAlgorithm}{GeneticAlgorithm}.

\begin{itemize}

\item Generación de población inicial.

\begin{spec}
    initialPopulation ga pop = sequence $  do _ <- [1..pop]
                                           return $ randomChromosome ga
\end{spec}

\item  Ejecución de las iteraciones del algoritmo.

\begin{spec}

runGA' ga pop = do
    let fit = assessed $ map (id &&& fitness ga) pop

    iterationHook ga

    stop <- stopCriteria ga . map snd $ unwrapAssessed fit

    intact  <- selectIntact ga fit
    cross   <- selectCrossover ga fit
    mut     <- selectMutate ga fit


    mutated <- mapM (mutate ga) mut

    let  pairToList (x,y) = [x,y]
         newPop  =  intact
                 ++ concatMap (pairToList . uncurry (crossover ga)) cross
                 ++ mutated

    if stop  then return $ selectResult ga fit
             else runGA' ga newPop

\end{spec}

\end{itemize}






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Ejecución y Pruebas}

La aplicación está definida en \hssrc{Parcial2--App}{Parcial2/App}
y la ejecutable actual en \hssrc{Parcial2--App}{Parcial2/App}.

\noindent El modo de uso se describe en Anexo \ref{sec:A1}.


El proyecto también contiene aplicación de ejemplo \texttt{ga-labyrinth-example-1}
y otras ejecutables que se usan para generación del documento.


\subsection{Pruebas}

\begin{note}
    El algoritmo considera todas las rutas que contienen inicio y meta, no solamente
    si son los extremos.
\end{note}

\subsubsection{Ejemplo de tarea}

El ejemplo se lee desde archivo \emph{laby.txt}.

\begin{verbatim}
dist/build/ga-labyrinth/ga-labyrinth\
    laby.txt 200\
    --gen-max-chain-len 5\
    --gen-max-chains 1\
    -I 100 -U 20
\end{verbatim}

El resultado (sin debug):

\begin{verbatim}
  [0-0,4-3,6-2,7-21]
\end{verbatim}

\subsubsection{Ejemplo de proyecto}
El laberinto se presenta en figura \ref{fig:rawMapExample};

\begin{verbatim}
dist/build/ga-labyrinth-example-1/ga-labyrinth-example-1\
    200\
    --gen-max-chain-len 10\
    --gen-max-chains 10\
    -I 100 -U 20
\end{verbatim}

\begin{verbatim}
[9--3,8--3,8-0,4-0,3-0,1-0,1--2,0--2]
\end{verbatim}


\subsection{Cambios necesarios}

\begin{enumerate}
  \item Seleccionar al remplazamiento en la recombinación de cromosomas aleatoriamente.
        Al momento el proceso de ``crossover'' es \emph{puro} -- no causa efectos secundarios,
        a los cuales pertenece lo ``aleatorio''. Eso también quiere decir que el resultado
        siempre es el mismo para los mismos padres. Se necesita introducir mas variedad.

  \item Al momento están desactivados los {color{red} \emph{mutaciones sobre rutas completas}},
        porque las implementaciones quiebran la política de no-repetición de genes.
        Deben ser reescritos.

\end{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Proyecto}

El proyecto requiere GHC y \texttt{cabal} para construcción.

Hay una dependencia, que no está disponible públicamente, por esto se necesita instalarla a mano:
\href{https://github.com/fehu/CommandArgs}{CommandArgs}.

\crule{1}


El proyecto usa la paradigma de \emph{programación literaria} y el reporte se genera desde el código.

Los ejemplos de laberinto y cromosomas se generan usando los mecanismos del proyecto;
los de cromosomas usan función \emph{crossover} de \emph{GA}.


\noindent El reporte en pdf se genera con script \emph{makeReport} proveído.
Requiere instalación de \texttt{lhs2TeX} desde \texttt{cabal}.


\section*{Anexo I}
\label{sec:A1}

\begin{verbatim}

Searches the shortest path in a labyrinth with Genetic Algorithm.

ga-labyrinth <Labyrinth File> <Population Size> [gen-max-chains]
                                                [gen-max-chain-len]
                                                [mut-max-chains]
                                                [mut-max-chain-len]
                                                [max-unchanged]
                                                [max-iter]
                                                [frac-intact]
                                                [frac-crossover]
                                                [frac-mutation]
                                                [fracs]
                                                [help]

Positional:
  Labyrinth File :: Text 	--  path to labyrinth file
  Population Size :: Int 	--

Optional:

  gen-max-chains <value>
     --gen-max-chains
     Chromosome Generation: maximum chain number.
        value :: Int 	--

  gen-max-chain-len <value>
     --gen-max-chain-len
     Chromosome Generation: maximum chain length.
        value :: Int 	--

  mut-max-chains <value>
     --mut-max-chains
     Chromosome Mutation: maximum chains to insert.
        value :: Int 	--

  mut-max-chain-len <value>
     --mut-max-chain-len
     Chromosome Mutation: maximum insert chain length.
        value :: Int 	--

  max-unchanged <value>
     -U --max-unchanged
     Maximum number of iterations without best fitness
     change before stopping.
        value :: Int 	--

  max-iter <value>
     -I --max-iter
     Maximum number of iterations.
        value :: Int 	--

  frac-intact <value>
     --frac-intact
     Fraction of population left intact.
        value :: Float 	--

  frac-crossover <value>
     --frac-crossover
     Fraction of population used for crossover.
        value :: Float 	--

  frac-mutation <value>
     --frac-mutation
     Fraction of population used for mutation.
        value :: Float 	--

  fracs <intact> <crossover> <mutation>
     -f --fracs
     Set all the fractions at once.
        intact :: Float 	--  intact fraction
        crossover :: Float 	--  crossover fraction
        mutation :: Float 	--  mutation fraction

  help <cmd...>
     -h --help
     Show help
        cmd... :: Text 	--  Commands to show the help for


\end{verbatim}

\end{document}

