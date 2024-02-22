# Epilog
## TP Final ALP Juan Segundo Valero
## Introducción
Trabajo práctico final de la materia Análisis de Lenguajes de Programación.
Se creó un lenguaje de programación lógica, inicialmente basado en Prolog, aunque con varios cambios en el funcionamiento, que se van a especificar en este documento.

### Stack
Para este TP vamos a usar [**Stack**](https://docs.haskellstack.org/), una herramienta sencilla para desarrollar poryectos en Haskell. Stack tiene muchas utilidades, pero ahora nos vamos a concentrar sus funciones básicas.

Antes que nada, puede que tengas que instalarlo. En [1](https://docs.haskellstack.org/en/stable/README/#how-to-install) hay guías de instalación para distintas plataformas.

Stack se encarga de instalar la versión correcta de GHC, instalar los paquetes necesarios y compilar el proyecto. Para las primeras dos, basta con abrir una terminal en el directorio `TPFinal` y ejecutar:
```
stack setup
```
Esto puede demorar un rato porque se encarga de descargar e instalar la verisón correcta de GHC. Este comando solo se debería tener que ejecutar una única vez. Al terminar esto, está todo listo para compilar el proyecto, que se hace con:
```
stack build
```
Este es el comando que van a tener que usar para compilar el proyecto cada vez que lo modifiquen.

### Estructura del código
La estructura del proyecto es la siguiente:
```
.
├── app
│   └── Main.hs
├── src
│   ├── Common.hs
│   ├── Equations.hs
│   ├── PrettyPrinter.hs
│   ├── Parse.y  
│   ├── Monads.hs  
│   ├── VarMatch.hs  
│   ├── Helpers.hs  
│   └── Logic.hs
├── Ejemplos
│   ├── exassgn.txt
│   ├── factorial.txt
│   ├── fib.txt
│   └── len.txt
├── README.md
├── Setup.hs
├── package.yaml
├── stack.yaml
└── stack.yaml.lock
```
* En el directorio `app` se define el módulo `Main`, que implementa el ejecutable final. 

* En el directorio `src` se encuentra la fuente del programa principal:
  - `Common` define los tipos de términos y valores necesarios para el funcionamiento del programa
  - `PrettyPrinter` tiene el Pretty Printer del lenguaje. 
  - `Parse.y` define el parser. Para ello, este archivo especifica la gramática en BNF y provee el lexer. El módulo `Parse.hs` es generado por la herramienta `Happy` al hacer `stack build`, y se guarda en un directorio oculto.
  - `Equations` contiene las funciones que se encargan de hacer las operaciones numericas.
  - `Monads` contiene las definiciones de las monadas.
  - `VarMatch` contiene la lógica para la busqueda de funciones y matcheo de variables.
  - `Helpers` contiene funciones auxiliares.
  - `Logic` contiene el evaluador del programa.

* En el directorio `Ejemplos` se encuentran algunos programas para demostrar el funcionamiento del lenguaje.


### Ejecución

Primero se compila el proyecto, utilizando
```
stack build
``` 

Una vez compilado, se puede correr el ejecutable haciendo:
```
stack exec TPFinal-exe 
```

Una vez ejecutado, se solicitará el ingreso de un archivo que contenga el programa a ejecutar.

## Reglas del lenguaje
Este lenguaje se basa en la asignación de valores a funciones con variables.
* Hay cuatro tipos de variables
  - `Valores` se definen entre '' y se interpretan como valores atómicos, ej: 'var', 'arg'.
  - `Genericas` se escriben sin '' y como indica su nombre son variables genéricas, pueden ser reemplazadas por valores, ej: A, B.
  - `Ecuaciones` son ecuaciones matemáticas y números, se escriben entre {}, ej: {1 + 2}, {1}, {A - 1}.
  - `Listas` listas de valores, se pueden pasar como argumentos o separar en head y tail como en Haskell (x:xs), ej: ['a', 'b', 'c'], [], ['a'].

Todas las instrucciones tienen que tener un terminador, que es el carácter `.`

Para asignar un valor a una función se hace de la siguiente manera:
```
function('arg', 'arg2') := true.
```
Para hacer una query a una función con sus argumentos se hace de la siguiente manera:
```
function('arg', 'arg2').
````
Esto va a devolver, o bien un valor de retorno, o un error.

### Operaciones lógicas disponibles
  - `&&` And 
  - `||` Or 
  - `==` Igualdad 
  - `!=` Negación 

### Queries genéricas
Al igual que en Prolog, se pueden hacer queries de funciones con variables genéricas y se devuelven los posibles valores que pueden tomar esas variables para que devuelva true la función. Por ejemplo:
```
fun('b', 'c') := true.
fun('d', 'e') := true.
fun('x', 'y') := true.
fun(A, B).
```
Resultado:
```
A='b','d','x'
B='c','e','y'
```
En el caso de tener más de una variable genérica, como en el ejemplo anterior, las columnas del resultado representan el conjunto de valores que tienen que tener de manera simultánea, en este ejemplo, fun('b', 'c'), fun('d', 'e') y fun('x', 'y') son true.

Como en Prolog, esto solo funciona para variables y funciones que son directamente true, es decir si se da el caso:
```
fun('b', 'c') := true.
fun(A, B) := fun(B, A).
fun(A, B).
```
El resultado solo va a tener en cuenta a fun('b', 'c').

Prolog también permite hacer operaciones con números utilizando este sistema de variables genéricas, en este lenguaje esto es reemplazado por las ecuaciones que se verán en el apartado siguiente.

### Ecuaciones
A diferencia de Prolog, este lenguaje permite devolver valores númericos, esto da la posibilidad de escribir funciones con enteros de la siguiente manera:
```
factorial({0}) := {1}.
factorial({1}) := {1}.
factorial(A) := {A} * factorial({A - 1}).
factorial({5}).
```
Como se puede observar, todo lo que tiene que ser interpretado como una ecuación aritmética va entre `{}` para separarlo de las variables booleanas.
* Las operaciones aritméticas permitidas son:
    - `+` Suma
    - `-` Resta
    - `*` Multiplicación
    - `/` División

Con el orden precedencia estándar.

### Listas
Se pueden crear programas que trabajen con listas de la siguiente manera:
```
len([]) := {0}.
len(x:xs) := {1} + len(xs).
len(['a', 'b', 'c']).
```
Las listas se definen entre `[]` y pueden ser pasadas como argumento para hacer una consulta.

A la hora de definir funciones, las listas pueden ser pasadas como argumento o se puede separar el head del tail como en Haskell con `:`.

### Errores
* Puede haber tres tipos de errores en la ejecución del programa
  - `Error de Parseo` Como indica el texto, si hubo algun error en el parseo del programa.
  - `UndefVar` Cuando se está queriendo hacer una consulta de una función que no existe para el tipo de datos solicitado.
  - `InvalidOp` Se esta queriendo hacer una operación inválida, esto pueden ser operaciones lógicas con valores aritméticos (o viceversa) u operaciones que combinan ambos tipos de dato