## 22/02/2017 (Introducción1.pdf)

Aprendizaje supervisado tiene dos grandes técnicas, se diferencian en lo que yo quiero sacar de los propios datos. Los problemas reales no pertenecen a ninguna categoría como esta pero los estudiamos en base a estos dos:

  - Regresión: Es cuando la variable de salida es un número real, un vector tambien podría ser. Pj.: Predecir el valor de la temperatura mañana en base a los datos.

  - Calificación: No se da lo de arriba y además la salida serán dos o más variables. Si se superan entorno a 30 variables tal vez no se hable de un problema de clasificación. Ej.: ¿Esto es un gato o un perro?

#### Elementos del problema dar un credito
Cada caso tiene que estar definidor en un cojunto finito de variables.

  - input: vector de variables x c R^d = X
  - output: y c {-1,1} = Y
  - Target function: desconocidad a priori lo que buscamos.
  - D es el conjunto de datos que nos dan para aprender. data set D = (x1, y1),...,(xn,yn) -> yn = f(xn)
    - Data Sample: Para las x tiene que haber una distribución de probabilidad para cada una la misma o no. La idea es, que si tengo 10 personas con una situación economica parecida no puedo aprender lo suficiente porque en el fondo es el mismo ejemplo, por ello tiene que ser ejemplos distintos. -> independientes identicamente distribuidos

#### Formalización

  1. Suponemos unos buenos ejemplos, que sean i.i.d
  2. Nuestra f siempre es una aproximación. H es como notamos las clases de funciones, si elegimos una clase muy lejana o no contiene a nada que aproxime a f cometeremos un error. Lo que haremos es: de ir menos a más complegidad de la clase. Este es el mayor problema en los problemas prácticos.
  3. El algoritmo nos lleva de una función a otra pues, entonces, necesitamos un criterio para saber si hemos mejorado o no.

#### Ejemplos de formalización:

  1. Uso de datos economicos de las familias en disferentes varrios y las ultimas ventas de casas.
    - X: los datos de las personas que vivan, como es la casa, plantas, muebles etc.
    - Y: valor de la vivienda
  2. Clasificador de vinos.
    - X: propiedades fisicas del líquido
    - Y: todos los tipos de marcas, cosechas de vinos.

## 22/02/2017 (Introducción2.pdf)

#### Modelos lineales
H es la clase de todos los modelos, es una clase simple pero enorme. Cada h c H y es h(x) = w0 + w1*x1 ... todas la combinaciones lineales de las variables en esa clase.

  - X^T = (1, x1 .. xd) variables de entrada
  - W^T = (w0 .. wd) pesos de las Y
  - Entonces h(x) = w^T*x

Las clases de funciones se pueden en problemas para:
  - Regresión
  - Clasificación
  - Estimación de probailidad

#### Usando Regresión (Lineal)
Para el problema de dar un credito o no darlo (ver transparencia para ver los datos)

  - X = {1} * R^d
  - Y = R
  - h: X -> Y
  - Eout(h): Media del error de todos los datos que están fuera del los ejemplos. Es decir minimizar el error en los nuevos datos que vamos a analizar, los que vengan al banco a pedir un credito.
  - Ein(w): Minimizar el error empírico
  - Buscamos los w_lin = min E_in(w)

#### Minimos cuadrados
Los errores que afectan a la etiqueta de salida la y_i tienen errores que vienen definida por e_i pero es ruido ya que ha habido que medirlos. Los errores están acotados. No debería de haber dependencia entre los errores E(ei,ej), la media de
los errores E(ei) es igual a 0.

  - X: sería una matriz con los valores de entrada y dentría N+1 componentes
  - Y: vector con N numeros reales.
  - ||(..)|| es la norma, que es distnacia euclidia por así decirlo ||x||² = X^t*X

## 08/03/2017

#### Clasificación (Binaria)
Tenemos un cojunto de numeros en imagenes que debemos de clasificar. Poseen dos
caracteristicas que son: intensidad y simetría. Estas son sacadas por estudio del problema y de forma "personal", podría haver sido pj. la cantidad de negros, el 8 tiene más que el 1.

#### Let’s start with a simpler case
Un clasificador será encontrar el w's que cambién el signo. La distancia a la recta, usamos el punto en la función y vemos el signo.

#### The Perceptron hypothesis set
Tenemos el vector de entrada x = (intensidad, simetria), si encontramos los valoes:
w1*x1+w2*x2 >= b -> 1, < b -> 5.

#### Perceptron Learning Algoritm (PLA)
Algoritmo historico. W será: si se cumple la condición, si es así el w nuevo será el x*y + w actual, esto lo repite con todos los datos y cuando no coincide con la etiqueta. 


















-
