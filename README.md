# Epilog
## TP Final ALP Juan Segundo Valero
## Introducción
Trabajo práctico final de la materia Análisis de Lenguajes de Programación.
Se creó un lenguaje de programación lógica, inicialmente basado en Prolog, aunque con varios cambios en el funcionamiento, que se van a especificar en este documento.

### Stack
Para este TP vamos a usar [**Stack**](https://docs.haskellstack.org/), una herramienta sencilla para desarrollar poryectos en Haskell. Stack tiene muchas utilidades, pero ahora nos vamos a concentrar sus funciones básicas.
```
stack setup
```

Luego:

```
stack build
```

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
│   ├── ej.txt
│   ├── pertenece.txt
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
