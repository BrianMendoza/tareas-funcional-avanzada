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
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Entrega Tarea 1}

\author{Brian-Marcial Mendoza\\
07-41206\\
\href{mailto:07-41206@usb.ve}{<07-41206@usb.ve>}}

\date{Mayo 01, 2015}

\maketitle

\pagebreak

\section{Librerías Utilizadas}

\begin{minipage}{\linewidth}
\begin{lstlisting}

> import Control.Applicative ((<$>),(<*>), pure)
> import Control.Monad ((>=>))
> import Data.List
> import Data.Functor
> import Data.Maybe
> import Data.Monoid
> import Data.Foldable (foldMap)
> import Data.Tree

\end{lstlisting}
\end{minipage}

Las librerías ya suministradas no fueron alteradas. Solamente
fueron removidas los imports referentes a librerías de elaboración de gráficos
por problemas de instalación y no eran requisitos para la correcta ejecución del
ejercicio. Adicionalmente se importaron tres librerías, Data.Maybe, Control.Applicative y
Control.Monad para la elaboración del segundo ejercicio y la prueba del tercer ejercicio.

\section{Machine Learning}

Se empieza trabajando con estos datos ya suministrados por el
enunciado.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> data Sample a = Sample { x :: [a], y :: a }
>      deriving (Show)
>
> data Hypothesis a = Hypothesis { c :: [a] }
>      deriving (Show)

\end{lstlisting}
\end{minipage}

Adicionalmente, se tienen estos valores por defecto que se emplearán
a lo largo de la resolución del problema. El contenido de \emph{training}
no se muestra en este documento por razones de legibilidad, pero al cargarlo
en ghci se evidencia como es una lista con varios Samples ya predeterminados.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> alpha :: Double
> alpha = 0.03
>
> epsilon :: Double
> epsilon = 0.0000001
>
> guess :: Hypothesis Double
> guess = Hypothesis { c = [0.0, 0.0, 0.0] }
>
> training :: [Sample Double]

\end{lstlisting}
\end{minipage}

\ignore{

> training = [
>  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
>  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
>  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
>  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
>  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
>  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
>  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
>  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
>  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
>  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
>  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
>  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
>  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
>  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
>  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
>  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
>  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
>  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
>  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
>  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
>  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
>  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
>  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
>  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
>  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
>  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
>  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
>  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
>  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
>  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
>  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
>  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
>  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
>  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
>  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
>  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
>  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
>  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
>  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
>  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
>  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
>  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
>  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
>  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
>  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
>  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
>  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]

}

\subsection{Comparar en punto flotante}

Para esta sección la función es bastante sencilla. Para ver
si la diferencia entre dos numeros es $\epsilon$-despreciable
se necesita simplemente restar el mayor de los dos menos el menor,
y consecuentemente verificar que ese resultado sea menor al $\epsilon$
preestablecido en la sección anterior.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> veryClose :: Double -> Double -> Bool
> veryClose v0 v1 = max v0 v1 - min v0 v1 < epsilon

\end{lstlisting}
\end{minipage}

\subsection{Congruencia dimensional}

En esta sección, ateniendose al requisito de que la función sea \emph{point-free}
y que cada Sample en la lista tenga un 1 al comienzo de su lista de X, se utiliza
la función map para alterar cada Sample junto con una función anónima que aprovecha
la funcionalidad de Record Update junto con un cons para agregar el 1 de forma rápida
y eficiente.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> addOnes :: [Sample Double] -> [Sample Double]
> addOnes = map (\s -> s {x = 1 : x s})

\end{lstlisting}
\end{minipage}

\subsection{Evaluando Hipótesis}

En esta sección se desarrollan dos funciones, \texttt{theta} y \texttt{cost}.

Para la función \texttt{theta}, como se puede asumir que ambos vectores tienen las mismas dimensiones,
se pueden usar las funciones de la familia zip, en particular zipWith, sin riesgo de perder algún dato.

La función \texttt{theta} es equivalente al producto punto, es decir, multiplicar cada $x_i$ por cada
$h_i$ y cada multiplicación es el nuevo $\theta_i$, y posteriormente se suman todos los
$\theta_i$ para obtener el valor final $\theta$, es análogo a usar las funciones predefinidas
de Haskell de \emph{zipWith (*)} y posteriormente \emph{sum} de dicho zip.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> theta :: Hypothesis Double -> Sample Double -> Double
> theta h s = sum $ zipWith (*) (x s) (c h)

\end{lstlisting}
\end{minipage}

Para la función \texttt{cost}, como se se exige que el resultado sea calculado en una sola pasada y sin
asumir una longitud para la lista de Samples, se emplea el uso de un \emph{fold} que utiliza una función
auxiliar que permite realizar la sumatoria necesaria. Adicionalmente, como se hace énfasis en una sola
pasada, y se necesita la longitud de la lista de Samples, utilizar la función predefinida \emph{length} no
es viable, ya que esto involucraría otra pasada por la lista. Por esto, la función auxiliar tiene como
acumulador una tupla, con el primer elemento siendo el resultado de la sumatoria y el segundo elemento
un contador que al final del \emph{fold} será igual a la longitud de la lista de Samples.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss
>   = fst res / (2 * snd res)
>   where
>     aux (acum,len) next
>      = (acum + (theta h next - y next)^2, len+1)
>     res
>      = foldl' aux (0,0) ss

\end{lstlisting}
\end{minipage}

\subsection{Bajando por el gradiente}

En esta sección se desarrollan dos funciones, \texttt{descend} y \texttt{gd}.

Para la función \texttt{descend}, que debe ejecutar un cálculo para reemplazar
cada $\theta_i$ por un $\theta'_i$, es una situación ideal para utilizar la
función preestablecida \emph{map} de Haskell. Como los cálculos requieren 
que se sepa el índice \emph{i} del $\theta_i$ que se esta reemplazando primero
se ejecuta un \emph{zip} con la lista [0..] y la lista de thetas de la hipótesis, para
así tener acceso rápido a dicho número.

La función que ejecutará el \emph{map} es una función auxiliar \texttt{newtheta} que toma
una tupla ($\theta$,i) que devuelve el nuevo $\theta'$ según la fórmula matemática. Dicha
función hace una llamada a otra función auxiliar la cual es un \emph{fold} que
va generando una tupla que al final contiene el resultado de la fórmula matemática y
la longitud de la lista de Samples.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double]
>         -> Hypothesis Double
> descend alpha h ss
>   = h { c = map newtheta $ zip (c h) [0..] }
>   where
>     newtheta (theta,index)
>       = theta - (alpha/snd (res index))*(fst (res index))
>     res index
>       = foldl' (aux index) (0,0) ss
>     aux index (acum,len) next
>       = (acum + (theta h next - y next)*((x next) !! index),
>                                                         len+1)

\end{lstlisting}
\end{minipage}

Para la función \texttt{gd}, se implementa mediante un \emph{unfoldr} que utiliza
una función auxiliar \texttt{expandir} que va devolviendo una 3-tupla

\begin{center}
(iteración, próxima hipótesis, costo de dicha hipótesis)
\end{center}

Dicho \emph{unfoldr} termina cuando la diferencia entre los costos entre
una hipótesis y la siguiente es $\epsilon$-despreciable. Adicionalmente, se agregan
unos al principio de cada vector X de cada Sample utilizando la función \texttt{addOnes}.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss
>   = unfoldr expandir (0,h,cost h ssOnes)
>   where
>     expandir (iter,hipotesis,costo)
>       = case veryClose costo (nextCosto hipotesis) of
>           True  -> Nothing
>           False -> Just ( ( iter,
>                             hipotesis,
>                             costo
>                           ), 
>                           ( iter+1,
>                             nextHipotesis hipotesis,
>                             nextCosto hipotesis
>                           )
>                         )
>     nextHipotesis oldHipotesis
>       = descend alpha oldHipotesis ssOnes
>     nextCosto oldHipotesis
>       = cost (nextHipotesis oldHipotesis) ssOnes
>     ssOnes
>       = addOnes ss

\end{lstlisting}
\end{minipage}

\section{Monoids}

En esta sección se debe construir una instancia Monoid polimórfica 
para cualquier tipo comparable tal que se extraiga de cualquier Foldable
conteniendo elementos de dicho tipo el máximo valor asignado. Como puede ser
para cualquier tipo de datos, Num por ejemplo, se emplea el método de envolverlos
con el tipo de data \texttt{Max a}.

Para proporcionar el requisito de que el Monoid opere con seguridad se utiliza
el monad \emph{Maybe} para envolver el resultado, de esta forma si el Foldable
se encuentra vacío simplemente se devuelve un Nothing. Si hay un resultado se
devuelve envuelto en un Just.

Como se indica en la instancia del Monoid, el tipo polimórfico debe ser un
\emph{Ord a}, asi satisfaciendo el requerimiento de que sea sobre cualquier tipo
comparable.

La definición de \emph{mempty} es directo, es simplemente Max Nothing, ya que si es vacío
devuelve nada, como indican los ejemplos proporcionados en el enunciado. La definición
de \emph{mappend} emplea el uso de los operadores <\$> y <*> de Control.Applicative y la función
\emph{maybe} de Data.Maybe para obtener el resultado con la función \emph{max}.

La lógica que se sigue es la siguiente:

\begin{itemize}
  \item La función max recibe dos parámetros del tipo a.
  \item Los elementos del Max estan en el contexto Maybe.
  \item Luego, se ejecuta \emph{max <\$> param1 <*> param2}, lo cual saca a param1 y param2
de su contexto Maybe, los pasa como parámetros al \emph{max} y los vuelve a poner en su 
contexto Maybe.
  \item Para evitar cláusulas de pattern matching se emplean las funciones:
        \newline \newline
        \hspace*{\fill}
        {maybe x pure y} \hfill {maybe y pure x}
        \hspace*{\fill}
        \newline
  \item Esto impide si se da la situación de:
        \begin{itemize}
          \item x = Nothing
          \item y = Just valor
        \end{itemize}
        Que el Nothing absorba el Just valor, ya que dado lo anterior se tendría
        Just valor <*> Just valor, y el valor es preservado.
\end{itemize}

\begin{minipage}{\linewidth}
\begin{lstlisting}

> data Max a = Max { getMax :: Maybe a }
>      deriving (Eq, Ord, Read, Show)

> instance (Ord a) => Monoid (Max a) where
>   mempty
>     = Max Nothing
>   Max x `mappend` Max y
>     = Max $ max <$> maybe x pure y <*> maybe y pure x

\end{lstlisting}
\end{minipage}

\section{Zippers}

En esta sección se implementará un Zipper para el tipo de dato suministrado
por el enunciado que se presenta a continuación.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> data Filesystem a = File a | Directory a [Filesystem a]
>   deriving (Show)

\end{lstlisting}
\end{minipage}

Empleando el método de traducir \texttt{Filesystem} a su tipo algebráico
y consecuentemente aplicando la derivada parcial, se obtiene que el tipo de
dato necesario para representar las "migajas" del recorrido es el siguiente:

\begin{minipage}{\linewidth}
\begin{lstlisting}

> data Crumbs a = Crumbs a [Filesystem a] [Filesystem a]
>   deriving (Show)

\end{lstlisting}
\end{minipage}

La interpretación que se da a este tipo de dato es:

\begin{center}
  Crumbs x l r
\end{center}

Donde:
\begin{itemize}
  \item x = Nombre del directorio padre
  \item l = Elementos a la izquierda de elemento enfocado en directorio actual
  \item r = Elementos a la derecha de elemento enfocado en directorio actual
\end{itemize}

Posterior a esto se declara el tipo \texttt{Breadcrumbs} que no es más que una lista de
\texttt{Crumbs} y el tipo \texttt{Zipper}, que es la tupla que representa el \texttt{Filesystem}
actualmente en foco y la lista del recorrido hasta ahora realizado.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> type Breadcrumbs a = [Crumbs a]
>
> type Zipper a = (Filesystem a, Breadcrumbs a)

\end{lstlisting}
\end{minipage}

Para la función \texttt{goDown} se tienen dos casos:

\begin{itemize}
  \item Si se está enfocado en un \texttt{File}, luego no se puede bajar.
  \item Si se está enfocado en un \texttt{Directory}, luego se baja, agregando
un Crumb con el nombre del Directorio, una lista vacía para elementos a la izquierda,
una lista de todos los elementos en el directorio en la lista para elementos a la derecha y
por último enfocando el primer elemento del directorio al cual se bajó.
\end{itemize}

Como se requiere que el \texttt{Zipper} sea seguro, tratar de bajar por un archivo devuelve
Nothing, bajar por un directorio devuelve Just nuevoZipper.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> goDown :: Zipper a -> Maybe (Zipper a)
> goDown (File fl, bs)
>   = Nothing
> goDown (Directory d (f:fs), bs)
>   = Just (f, Crumbs d [] fs : bs)

\end{lstlisting}
\end{minipage}

Para la función \texttt{goRight} se tienen dos casos:

\begin{itemize}
  \item Si no hay más elementos a la derecha, no se puede mover.
  \item Si hay elementos a la derecha, luego el Crumb más reciente es
modificado por uno con la lista de elementos de cada lado actualizados
para representar el cambio y se enfoca en el elemento inmediatamente a 
la derecha.
\end{itemize}

Como se requiere que el \texttt{Zipper} sea seguro, tratar de moverse a la derecha
cuando esto no es posible devuelve Nothing, cuando si es posible devuelve Just nuevoZipper.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> goRight :: Zipper a -> Maybe (Zipper a)
> goRight (fs, Crumbs _ _ []:bs)
>   = Nothing
> goRight (fs, Crumbs d ls (r:rs):bs)
>   = Just (r, Crumbs d (fs:ls) rs:bs)

\end{lstlisting}
\end{minipage}

Para la función \texttt{goLeft} se tienen dos casos:

\begin{itemize}
  \item Si no hay más elementos a la izquierda, no se puede mover.
  \item Si hay elementos a la izquierda, luego el Crumb más reciente es
modificado por uno con la lista de elementos de cada lado actualizados
para representar el cambio y se enfoca en el elemento inmediatamente a 
la izquierda.
\end{itemize}

Como se requiere que el \texttt{Zipper} sea seguro, tratar de moverse a la izquierda
cuando esto no es posible devuelve Nothing, cuando si es posible devuelve Just nuevoZipper.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> goLeft :: Zipper a -> Maybe (Zipper a)
> goLeft (fs, Crumbs _ [] _:bs)
>   = Nothing
> goLeft (fs, Crumbs d (l:ls) rs:bs)
>   = Just (l, Crumbs d ls (fs:rs):bs)

\end{lstlisting}
\end{minipage}

Para la función \texttt{goBack} se tienen dos casos:

\begin{itemize}
  \item No hay más \texttt{Crumbs}, es decir, se esta en el directorio raíz,
luego no se puede subir más.
  \item Si hay \texttt{Crumbs} restantes, es decir, hay al menos un directorio que
contiene al \texttt{Filesystem} actualmente en foco. Luego, se reconstruye el directorio
padre con su nombre almacenado en el \texttt{Crumb} y reconstruyendo la lista de los \texttt{Filesystem}
contenidos en él concatenando la lista de elementos a la izquierda del \texttt{Filesystem} en foco, el 
\texttt{Filesystem} en foco y la lista de elementos a la derecha del \texttt{Filesystem} en foco. Adicionalmente,
el \texttt{Crumb} donde estaba almacenado esta información es desechado y se guardan las que venían antes que él.
\end{itemize}

Como se requiere que el \texttt{Zipper} sea seguro, tratar de subir
cuando esto no es posible devuelve Nothing, cuando si es posible
devuelve Just nuevoZipper.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> goBack :: Zipper a -> Maybe (Zipper a)
> goBack (fs, [])
>   = Nothing
> goBack (fs, Crumbs d ls rs:bs)
>   = Just (Directory d $ reverse ls ++ [fs] ++ rs,bs)

\end{lstlisting}
\end{minipage}

La función \texttt{tothetop} es una aplicación recursiva de goBack, para
subir por los directorios hasta llegar al directorio raíz y se devuelve el \texttt{Zipper}
resultante. Cuando la lista de \texttt{Crumbs} está vacía ya se ha llegado al directorio
raíz. Si hay por lo menos uno, entonces se invoca \texttt{goBack} y se vuelve a aplicar
\texttt{tothetop} sobre este nuevo \texttt{Zipper}.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> tothetop :: Zipper a -> Zipper a
> tothetop (fs, [])
>   = (fs, [])
> tothetop z
>   = tothetop $ fromJust $ goBack z

\end{lstlisting}
\end{minipage}

La función \texttt{modify} aplica una función al \texttt{Filesystem} actualmente
en foco y devuelve un Zipper con dicho \texttt{Filesystem} modificado. El elemento
modificado de un \texttt{File} es el único que posee, el elemento modificado de un 
\texttt{Directory} es el primero.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> modify :: (a -> a) -> Zipper a -> Zipper a
> modify f (Directory d fs, bs)
>   = (Directory (f d) fs, bs)
> modify f (File fl, bs)
>   = (File (f fl), bs)

\end{lstlisting}
\end{minipage}

La función \texttt{focus} recibe como entrada un \texttt{Filesystem} y devuelve
un nuevo \texttt{Zipper} con dicho \texttt{Filesystem} en foco y sin \texttt{Crumbs}.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> focus :: Filesystem a -> Zipper a
> focus fs = (fs, [])

\end{lstlisting}
\end{minipage}

La función \texttt{defocus} recibe como su entrada un \texttt{Zipper} y devuelve
el \texttt{Filesystem} en foco y se descartan los \texttt{Crumbs} si todavía hay.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> defocus :: Zipper a -> Filesystem a
> defocus (fs, _) = fs

\end{lstlisting}
\end{minipage}

Para demostrar la ejecución de estos comandos, se parte de una estructura hecha
de un directorio que contiene otros directorios y archivos. Se navegará hasta
el archivo "bash.sh" para cambiar su nombre a "newbash.sh". Luego se irá hasta
el directorio padre y se desenfocará para ver si se preserva el cambio. Por legibilidad
se utiliza el operador \texttt{(>=>)} de Control.Monad para concatenar fácilmente las
operaciones de movimiento.

\begin{minipage}{\linewidth}
\begin{lstlisting}

> folders :: Filesystem String
> folders
>   = Directory "/root" [
>                        File "img01.jpeg",
>                        File "img02.jpeg",
>                        Directory "/scripts" [
>                                              File "bash.sh",
>                                              File "test.sh",
>                                              File "hax.sh"
>                                             ],
>                        File "to-do-list.txt"
>                       ]

> zipper :: Zipper String
> zipper = focus folders

\end{lstlisting}
\end{minipage}

Viendo el output de ghci, se evidencia que el cambio se realizó
de forma exitosa:

\begin{verbatim}
*Main> folders
Directory "/root" [File "img01.jpeg",
                   File "img02.jpeg",
                   Directory "/scripts" [File "bash.sh",
                                         File "test.sh",
                                         File "hax.sh"],
                   File "to-do-list.txt"]

*Main> defocus $ tothetop $ modify (\_ -> "newbash.sh") $
         fromJust $
           (goDown >=> goRight >=> goRight >=>
              goLeft >=> goRight >=> goDown) zipper

Directory "/root" [File "img01.jpeg",
                   File "img02.jpeg",
                   Directory "/scripts" [File "newbash.sh",
                                         File "test.sh",
                                         File "hax.sh"],
                   File "to-do-list.txt"]
\end{verbatim}

\end{document}
