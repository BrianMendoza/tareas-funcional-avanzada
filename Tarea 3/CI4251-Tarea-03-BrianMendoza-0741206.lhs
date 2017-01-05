\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey},
   literate=
     {á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
     {Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Entrega Tarea 3}

\author{Brian-Marcial Mendoza\\
07-41206\\
\href{mailto:07-41206@usb.ve}{<07-41206@usb.ve>}}

\date{Junio 07, 2015}

\maketitle

\pagebreak


\section*{Eat all the empanadas!}

\noindent
Rafita prepara empanadas en una recóndita playa del oriente del
país. La calidad de sus empanadas de cazón es legendaria, tanto
que la contratan para servir los pasapalos de las fiestas playeras
organizadas por la banda usual de desadaptados que allí pulula en
cualquier puente o feriado largo.
\\

\noindent
Para esos eventos, Rafita se presenta con su gran paila,
que tiene capacidad para freír $m$ empanadas. Una vez fritas,
echa \emph{todas} las empanadas, simultáneamente, en un gran colador
plástico de color indeterminado, raído por el tiempo, los elementos
y el manoseo. Hecho esto, y consecuencia del calor por la paila y el
inclemente sol, Rafita se sienta a tomar cerveza.
\\

\noindent
Cuando un parroquiano está hambriento, se acerca a buscar empanadas.
Los parroquianos de la fiesta no hacen cola, sino que meten mano
y se sirven del colador, tomando y comiendo \textbf{una} empanada
a la vez, siempre y cuando haya empanadas disponibles. Cuando el
parroquiano come, vuelve a tomar cerveza por un rato, hasta que
tenga hambre nuevamente.
\\

\noindent
Si un parroquiano tiene hambre, pero no hay empanadas, le avisa a
Rafita para que prepare más.
\\

\noindent
Se desea que Ud. construya una simulación de este comportamiento
usando Haskell. En este sentido, tome en cuenta lo siguiente:

\begin{itemize}
\item
  Rafita es la única que prepara empanadas y siempre prepara
  \emph{exactamente} $m$ empanadas en lote. Naturalmente,
  Rafita será modelada con un hilo de ejecución.
\item 
  Rafita tarda un tiempo al azar en preparar las empanadas.
  A efectos de esta simulación, considere un tiempo al azar
  entre 3 y 5 segundos. La simulación debe indicar claramente
  \texttt{Rafita está cocinando} y pasado el intervalo
  \texttt{Rafita sirvió las empanadas}.
\item
  En la fiesta puede haber un número arbitrario de parroquianos.
  Su simulación debe estar parametrizada para $n > 0$ parroquianos,
  y cada parroquiano debe estar representado por un hilo
  independiente.
\item
  Los hábitos de bebida y comida de los parroquianos son muy
  variables, así que debe considerar que transcurre un tiempo
  al azar entre 1 y 7 segundos desde el momento en que empuña
  su empanada, la consume, vuelve a su bebida, y tiene hambre
  de nuevo. La simulación debe indicar claramente
  \texttt{Parroquiano N come empanada} cuando comienza a comer
  y \texttt{Parroquiano N tiene hambre} cuando vuelve a buscar
  una empanada.
\item
  Rafita tiene ingredientes infinitos para preparar las empanadas,
  y los parroquianos no tienen nada más productivo que hacer,
  de manera que una vez que comienza la fiesta, sólo termina
  cuando se interrumpe la simulación con \texttt{Ctrl-C}.
\end{itemize}


\noindent
Presente \textbf{dos} soluciones para este problema: una empleando
técnicas clásicas de concurrencia (sincronización con \texttt{MVar})
y otra empleando Memoria Transaccional (\texttt{STM}).

\noindent
En ambos casos, cuando se interrumpa la simulación, presente
un resultado sumario indicando:

\begin{verbatim}
Rafita preparó R empanadas.

Parroquiano 1:    P1
Parroquiano 2:    P2
...
Parroquiano N:    PN
Total:            T
\end{verbatim}

\noindent
Donde \texttt{R} es el total de empanadas, necesariamente
múltiplo de $m$; \texttt{P1} hasta \texttt{PN} corresponden
a la cantidad de empanadas que comió cada parroquiano,
respectivamente, y \texttt{T} resulta de la suma de esos
consumos.
\\

\noindent
Finalmente, para poder comprobar la fidelidad de su simulación
es necesario que use números pseudo-aleatorios, como los que
se proveen en \texttt{System.Random} \emph{fuera} del
monad \texttt{IO}, usando \texttt{randomSeed} como semilla.

\pagebreak

\section*{Librerías Utilizadas}

\noindent
Para la resolución del problema se utilizaron las siguientes librerías.
\\

\begin{lstlisting}

> {-# LANGUAGE ScopedTypeVariables #-}
>
> import Control.Applicative (pure, (<$>))
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Exception
> import Control.Monad (forever, replicateM)
> import Data.Foldable (for_, traverse_)
> import Data.Traversable (for, traverse)
> import Data.Tuple (swap)
> import System.Random

\end{lstlisting}

\section{Solución Clásica}

\subsection{Funciones Auxiliares}

\noindent
Las funciones auxiliares utilizadas para la solución clásica del
problema se presentan a continuación y serán explicados
brevemente.
\\

\begin{lstlisting}

> randomSeed :: Int
> randomSeed = 42

\end{lstlisting}

\noindent
Función que proveerá la semilla para el generador de números
aleatorios.
\\

\begin{lstlisting}

> printer :: Chan String -> IO ()
> printer spool
>   = do
>     xs <- getChanContents spool
>     mapM_ putStrLn xs

\end{lstlisting}

\noindent
Función que será ejecutada en la solución clásica por el hilo
encargado de imprimir en pantalla los mensajes de los hilos de
la simulación. Recibe como entrada un canal y va leyendo
perezosamente de dicho canal e imprimiendo cada mensaje encontrado
en pantalla.
\\

\begin{lstlisting}

> randomDelayC :: RandomGen a => Int -> Int -> MVar a -> IO ()
> randomDelayC lo hi gen
>   = do
>     time <- modifyMVar gen
>             $ pure . swap . randomR (lo, hi)
>     threadDelay time

\end{lstlisting}

\noindent
Función que será ejecutada en la solución clásica por los
hilos encargados de simular el comportamiento de Rafita y
los parroquianos. El invocarlo obtiene un tiempo aleatorio
entre los valores lo y hi que son pasados como parámetros.
El MVar que contiene el generador del próximo número aleatorio
es actualizado y posteriormente se invoca la función threadDelay
para simular los tiempos de espera.
\\

\begin{lstlisting}

> rafaC :: RandomGen a =>
>           MVar a -> MVar Int -> MVar Bool ->
>             MVar Bool -> MVar Int -> Chan String ->
>               Int -> IO b
> rafaC gen empanadas needMore served total spool m
>   = forever
>   $ do
>     _ <- takeMVar needMore
>     writeChan spool "Rafita esta cocinando"
>     randomDelayC (3 * 10^6)  (5 * 10^6) gen
>     writeChan spool "Rafita sirvió las empanadas"
>     putMVar served True
>     putMVar empanadas (m-1)
>     modifyMVar_ total (pure . (+m))

\end{lstlisting}

\noindent
Función que será ejecutada en la solución clásica por el hilo
encargado de simular el comportamiento de Rafita. Primero se
queda esperando hasta que el MVar que actúa como un flag de
que se necesitan más empanadas tenga un valor. Luego, escribe al canal
para que el hilo de impresión escriba en pantalla que Rafita
está cocinando. Después de esto se invoca la función \texttt{randomDelay}
para simular el tiempo que se tarda en cocinar. Por último,
manda un mensaje al hilo de impresión para que escriba en pantalla
que Rafita sirvió las empanadas, se llena el MVar que actúa como flag
de que están servidas con el valor True, se llena el MVar que
simula la cantidad de empanadas en el colador con \texttt{m-1} empanadas
y se incrementa su contador de empanadas servidas en \texttt{m}.
\\

\noindent
La razón por la cual el MVar del colador se llena con \texttt{m-1} en vez
de \texttt{m} se debe a que el hilo que notó que hacían falta y avisó a 
Rafita que cocinara tiene garantizado una empanada, por ende solo hay 
\texttt{m-1} empanadas disponibles para los demás que vengan después.
\\

\begin{lstlisting}

> parroquianoC :: RandomGen a =>
>                   MVar Int -> MVar a -> MVar Int ->
>                     MVar Bool -> MVar Bool -> Chan String ->
>                       Int -> IO b
> parroquianoC counter gen empanadas needMore served spool id
>   = forever
>   $ do
>     randomDelayC (1 * 10^6) (7 * 10^6) gen
>     writeChan spool $ "Parroquiano "
>                         ++ show id
>                         ++ " tiene hambre"
>     takeEmpanada
>     writeChan spool $ "Parroquiano "
>                         ++ show id
>                         ++ " come empanada"
>   where
>     takeEmpanada :: IO ()
>     takeEmpanada
>       = do
>         empanada <- takeMVar empanadas
>         if empanada > 0
>           then putMVar empanadas (empanada - 1)
>           else (do
>                 putMVar needMore True
>                 _ <- takeMVar served
>                 return ()
>                )
>         modifyMVar_ counter (pure . (+1))

\end{lstlisting}

\noindent
Función que será ejecutada en la solución clásica por los hilos
encargados de simular el comportamiento de los parroquianos.
En primer lugar se ejecuta un \texttt{randomDelay} para simular
el tiempo que transcurre para que al parroquiano le dé hambre.
Luego, se manda un mensaje al hilo encargado de impresión mediante
un canal para que se imprima en pantalla el mensaje de que el
parroquiano $P_i$ tiene hambre. Posteriormente, trata de agarrar
una empanada.
\\

\noindent
Esto consiste en tomar la cantidad de empanadas del MVar que simula
la cantidad de empanadas en el colador. Si esta cantidad es mayor que
cero, entonces se llena dicho MVar con el valor disminuido en 1. Si
la cantidad no es mayor que cero, entonces se acabaron las empanadas
y el parroquiano le avisa a Rafita que se neccesitan más. Esto se logra
primero llenando el MVar que actúa como un flag de que se necesitan más
empanadas con el valor True, luego se queda esperando hasta que el MVar
que actúa como un flag de que se sirvieron más empanadas contenga un valor.
Por la forma que se estructura el programa, se garantiza que el que pidió
que se hicieran más tenga una empanada garantizada. Por último, se
incrementa el contador de cuantas empanadas ha comido el parroquiano
$P_i$ en 1.
\\

\noindent
El último paso para cada ciclo de simulación es mandar un mensaje al
hilo encargado de impresión para que imprima por pantalla un aviso que
diga que el parroquiano $P_i$ comió una empanada.
\\

\subsection{Solución}

\noindent
A continuación se presenta el cuerpo principal de ejecución de la
solución transaccional junto con una explicación de su ejecución.
\\

\begin{lstlisting}

> classic :: Int -> Int -> IO ()
> classic m n
>   = do
>     gen <- newMVar $ mkStdGen randomSeed
>     empanadas <- newMVar m
>     needMore <- newEmptyMVar
>     served <- newEmptyMVar
>     totalRafa <- newMVar m
>     spool <- newChan
>     counters <- replicateM n $ newMVar 0
>     printerID <- forkIO $ printer spool
>     rafaID <- forkIO
>                 $ rafaC
>                     gen
>                     empanadas
>                     needMore
>                     served
>                     totalRafa
>                     spool
>                     m
>     threads <- for [1..n] $ \id ->
>                   forkIO
>                     $ parroquianoC
>                         (counters !! (id-1))
>                         gen
>                         empanadas
>                         needMore
>                         served
>                         spool
>                         id
>     forever (threadDelay (10^6))
>      `catch`
>      (\ (e :: SomeException) -> cleanup
>                                  (printerID : rafaID : threads)
>                                  counters
>                                  totalRafa)
>   where
>     cleanup :: [ThreadId] -> [MVar Int] -> MVar Int -> IO ()
>     cleanup threads counters totalRafa
>       = do
>         for_ threads killThread
>         results <- traverse takeMVar counters
>         totalPreparados <- takeMVar totalRafa
>         putStrLn "\n"
>         putStrLn $ "Rafita preparó "
>                       ++ show totalPreparados
>                       ++ " empanadas."
>         traverse_ imprimir $ zip [1..n] results
>         putStrLn $ "Total:\t\t" ++ show (sum results)
>       where
>         imprimir :: (Int,Int) -> IO ()
>         imprimir (id,num)
>           = putStrLn $ "Parroquiano "
>                           ++ show id
>                           ++ ":\t"
>                           ++ show num

\end{lstlisting}

\noindent
El orden de ejecución de la solución clásica es el siguiente:

\begin{itemize}
\item
  Se generan los MVars y el Chan que serán usados durante la ejecución:
  \begin{itemize}
  \item[gen:]
    El MVar que contiene el generador del próximo número aleatorio.
    Se inicializa con la semilla provista por \texttt{randomSeed}.
  \item[empanadas:]
    El MVar que simula la cantidad de empanadas dentro del colador.
  \item[needMore:]
    El MVar que actúa como flag para indicar si se necesitan cocinar
    más empanadas.
  \item[served:]
    El MVar que actúa como flag para indicar que se sirvieron
    más empanadas.
  \item[totalRafa:]
    El MVar que actúa como contador para llevar la cuenta de cuantas empanadas
    ha cocinado Rafita.
  \item[spool:]
    El Chan que será utilizado como canal de los hilos de simulación al hilo
    encargado de la impresión de mensajes.
  \item[counters:]
    Una lista de MVars que actúan como contadores para cada hilo que simula
    un parroquiano para llevar la cuenta de cuantas empanadas ha comido cada
    parroquiano.
  \end{itemize}
  \item
    Se ejecuta un \texttt{forkIO} para crear el hilo encargado de la impresión
    por pantalla, recibe como argumento el Chan \emph{spool}.
  \item
    Se ejecuta un \texttt{forkIO} para crear el hilo encargado de simular el
    comportamiento de Rafita. Recibe como argumentos los MVars
    \emph{gen, empanadas, needMore, served, totalRafa}, el Chan \emph{spool}
    y el número de empanadas a crear cada vez que cocina \emph{m}.
  \item
    Se ejecutan \emph{n} \texttt{forkIO}'s para crear los hilos encargados de simular el
    comportamiento de un parroquiano $P_i$. Cada \texttt{forkIO} recibe como argumentos los MVars
    \emph{counter, gen, empanadas, needMore, served, spool} y el número \emph{i} que identifica a cada
    parroquiano. El MVar \emph{counter} es uno de los MVars de la lista de MVars \emph{counters} que le
    corresponde a ese hilo en particular.
  \item
    El hilo principal ejecuta \texttt{threadDelay} de 1 segundo indefinidamente hasta que atrape
    una señal de interrupción de la ejecución. Cuando esto suceda se invoca la función \texttt{cleanup}
    pasándole como argumentos la lista de los id's de los hilos creados, la lista \emph{counters} y el 
    MVar \emph{totalRafa}.
  \item
    Cuando se ejecuta \texttt{cleanup}, primero se matan a todos los hilos creados con la función
    \texttt{killThread}. Luego, se ejecuta \texttt{takeMVar} sobre todos los elementos de la lista
    \emph{counters} y sobre el MVar \emph{totalRafa}. Posteriormente se imprimen en pantalla los 
    resultados de cuantas empanadas preparó Rafita, cuantas empanadas comieron cada uno de los
    parroquianos y la suma total de las empanadas consumidas.
\end{itemize}

\pagebreak

\section{Solución Transaccional}

\subsection{Funciones Auxiliares}

\noindent
Las funciones auxiliares utilizadas para la solución transaccional del
problema se presentan a continuación y serán explicados
brevemente. La función auxiliar \texttt{randomSeed} de la solución
clásica es utilizada de nuevo en esta solución.
\\

\begin{lstlisting}

> type Counter = TVar Int
> 
> newCount :: Int -> STM Counter
> newCount n = newTVar n

\end{lstlisting}

\noindent
Nuevo tipo que será utilizado en la solución transaccional
para almacenar las empanadas consumidas por cada parroquiano
y el total de empanadas hechas por Rafita.
\\

\begin{lstlisting}

> output :: TChan String -> IO a
> output spool
>   = forever
>   $ atomically (readTChan spool)
>     >>=
>     putStrLn

\end{lstlisting}

\noindent
Función que será invocada infinitamente en la solución
transaccional para leer del TChan e imprimir cada mensaje
obtenido por pantalla.
\\

\begin{lstlisting}

> randomDelayT :: RandomGen a => Int -> Int -> TVar a -> IO ()
> randomDelayT lo hi gen
>   = do
>     time
>       <- atomically
>       $ do
>         (time, newGen) <- randomR (lo, hi) <$> readTVar gen
>         writeTVar gen newGen
>         pure time
>     threadDelay time

\end{lstlisting}

\noindent
Función que será invocada durante la solución transaccional
para poner a un hilo a dormir por una cantidad de tiempo aleatoria
entre los parámetros \emph{lo} y \emph{hi}. El nuevo generador aleatorio es almacenado
en su TVar y se invoca la función \texttt{threadDelay} para dormir al hilo
por esa cantidad de tiempo.
\\

\begin{lstlisting}

> rafaT :: RandomGen a =>
>           Counter -> Counter -> Int ->
>             TChan String -> TVar a -> IO b
> rafaT totalRafa empanadas m out gen
>   = forever
>   $ do
>     atomically
>       $ do
>         checkEmpanadas
>         writeTChan out "Rafita esta cocinando"
>     randomDelayT (3 * 10^6)  (5 * 10^6) gen
>     atomically
>       $ do
>         modifyTVar' totalRafa (+m)
>         writeTChan out "Rafita sirvió las empanadas"
>         writeTVar empanadas m
>   where
>     checkEmpanadas :: STM ()
>     checkEmpanadas
>       = do
>         numEmpanadas <- readTVar empanadas
>         if (numEmpanadas > 0)
>           then retry
>           else return ()

\end{lstlisting}

\noindent
Función que será invocada por un hilo que se dedica a
simular las acciones de Rafita en la solución transaccional.
Primero revisa si todavía hay empanadas. Si hay al menos uno, el hilo
se duerme hasta que ocurra un cambio en el número de empanadas
y vuelve a revisar. Si ya no hay empanadas (es decir, cero empanadas)
entonces manda un mensaje por el TChan para que sea impreso por pantalla,
avisando que Rafita empieza a cocinar. Luego, el hilo se duerme por un
tiempo aleatorio para simular el tiempo de cocinar las empanadas. Por último,
incrementa el total de empanadas que cocinó en \emph{m} de forma estricta
(para evitar tener un thunk muy grande en el TVar), manda un mensaje por
el TChan para avisar que ya sirvió las empanadas y actualiza el número
de empanadas disponibles a \emph{m}.
\\

\pagebreak

\begin{lstlisting}

> parroquianoT :: RandomGen a =>
>                 Counter -> Counter -> TChan String->
>                   TVar a -> Int -> IO b
> parroquianoT counter empanadas out gen id
>   = forever
>   $ do
>     randomDelayT (1 * 10^6)  (7 * 10^6) gen
>     atomically $ writeTChan out $ "Parroquiano "
>                                     ++ show id
>                                     ++ " tiene hambre"
>     atomically
>       $ do
>         checkEmpanadas
>         modifyTVar' counter (+1)
>         writeTChan out $ "Parroquiano "
>                             ++ show id
>                             ++ " come empanada"
>   where
>     checkEmpanadas :: STM ()
>     checkEmpanadas
>       = do
>         numEmpanadas <- readTVar empanadas
>         if (numEmpanadas <= 0)
>           then retry
>           else writeTVar empanadas (numEmpanadas-1)

\end{lstlisting}

\noindent
Función que será invocada por cada hilo que simula las
acciones de un parroquiano $P_i$ en la solución transaccional.
Transcurre su tiempo de espera aleatorio para simular el
tiempo que se tarda en tener hambre y manda un mensaje por
el TChan para que sea impreso en pantalla, avisando que tiene
hambre. Posteriormente revisa si hay empanadas, si no hay se
duerme el hilo  hasta que ocurra algún cambio en la cantidad de empanadas.
Si habían empanadas, decrementa el número de empanadas en 1,
incrementan estrictamente su contador de empanadas consumidas en 1
(para evitar tener un thunk muy grande en el TVar) y mandan otro
mensaje por el TChan para que se imprima en pantalla, avisando que
están comiendo una empanada.
\\

\pagebreak

\subsection{Solución}

\noindent
A continuación se presenta el cuerpo principal de ejecución de la
solución transaccional junto con una explicación de su ejecución.
\\

\begin{lstlisting}

> transactional :: Int -> Int -> IO ()
> transactional m n
>   = do
>     counters <- atomically $ replicateM n (newCount 0)
>     totalRafa <- atomically $ newCount m
>     empanadas <- atomically $ newCount m
>     gen <- (atomically . newTVar) $ mkStdGen randomSeed
>     spool <- atomically newTChan
>     rafaID <- forkIO
>                 $ rafaT totalRafa empanadas m spool gen
>     threads <- for [1..n] $ \id ->
>                   forkIO
>                     $ parroquianoT
>                         (counters !! (id-1))
>                         empanadas
>                         spool
>                         gen
>                         id
>     output spool
>       `catch`
>       (\ (e :: SomeException) -> cleanup
>                                     (rafaID : threads)
>                                     counters
>                                     totalRafa)
>   where
>     cleanup :: [ThreadId] -> [Counter] -> Counter -> IO ()
>     cleanup threads counters totalRafa
>       = do
>         for_ threads $ killThread
>         results <- atomically $ traverse readTVar counters
>         totalPreparados <- atomically $ readTVar totalRafa
>         putStrLn "\n"
>         putStrLn $ "Rafita preparó "
>                       ++ show totalPreparados
>                       ++ " empanadas."
>         traverse_ imprimir $ zip [1..n] results
>         putStrLn $ "Total:\t\t" ++ show (sum results)
>       where
>         imprimir :: (Int,Int) -> IO ()
>         imprimir (id,num)
>           = putStrLn $ "Parroquiano "
>                           ++ show id
>                           ++ ":\t"
>                           ++ show num

\end{lstlisting}

\noindent
El orden de ejecución de la solución transaccional es el siguiente:

\begin{itemize}
\item
  Se generan los TVars y el TChan que serán usados durante la ejecución:
  \begin{itemize}
  \item[gen:]
    El TVar que contiene el generador del próximo número aleatorio.
    Se inicializa con la semilla provista por \texttt{randomSeed}.
  \item[empanadas:]
    El TVar que simula la cantidad de empanadas dentro del colador.
  \item[totalRafa:]
    El TVar que actúa como contador para llevar la cuenta de cuantas empanadas
    ha cocinado Rafita.
  \item[spool:]
    El TChan que será utilizado como canal de los hilos de simulación al hilo
    encargado de la impresión de mensajes.
  \item[counters:]
    Una lista de TVars que actúan como contadores para cada hilo que simula
    un parroquiano para llevar la cuenta de cuantas empanadas ha comido cada
    parroquiano.
  \end{itemize}
  \item
    Se ejecuta un \texttt{forkIO} para crear el hilo encargado de simular el
    comportamiento de Rafita. Recibe como argumentos los TVars
    \emph{gen, empanadas, totalRafa}, el TChan \emph{spool}
    y el número de empanadas a crear cada vez que cocina \emph{m}.
  \item
    Se ejecutan \emph{n} \texttt{forkIO}'s para crear los hilos encargados de simular el
    comportamiento de un parroquiano $P_i$. Cada \texttt{forkIO} recibe como argumentos los TVars
    \emph{counter, gen, empanadas}, el TChan \emph{spool} y el número \emph{i} que identifica a cada
    parroquiano. El TVar \emph{counter} es uno de los TVars de la lista de TVars \emph{counters} que le
    corresponde a ese hilo en particular.
  \item
    El hilo principal ejecuta \texttt{output} con \emph{spool} como parámetro indefinidamente hasta que atrape
    una señal de interrupción de la ejecución. Cuando esto suceda se invoca la función \texttt{cleanup}
    pasándole como argumentos la lista de los id's de los hilos creados, la lista \emph{counters} y el 
    TVar \emph{totalRafa}.
  \item
    Cuando se ejecuta \texttt{cleanup}, primero se matan a todos los hilo creados con la función
    \texttt{killThread}. Luego, se ejecuta \texttt{readTVar} sobre todos los elementos de la lista
    \emph{counters} y sobre el TVar \emph{totalRafa}. Posteriormente se imprimen en pantalla los 
    resultados de cuantas empanadas preparó Rafita, cuantas empanadas comieron cada uno de los
    parroquianos y la suma total de las empanadas consumidas.
\end{itemize}

\end{document}
