Para instalar

1. Abrir R o RStudio
2. Pegar este comando.
``
source("https://raw.githubusercontent.com/olitroski/Actigrafia/master/exe_control.R")
``


# Actigrafía
 Programa para analizar actividad motora en relación al ciclo de sueño y vigilia.

> Ojo que uso funciones del olitos package dentro de las funciones.



## 1. Data Source

Los datos necesarios provienen de un actígrafo (Actiwatch) en forma de archivo **AWD** el cual tiene un encabezado y sucesivos valores de pasadas de movimiento por el acelerómetro por unidad de tiempo. En este caso el file de ejemplo se llama `BenjamonVenegas.awd`

El encabezado tiene 7 líneas con información

```
BenjaminVenegas
10-dic-2009
16:00
 4 
16
V964291
M
```

La información más relevante de esta parte es la fecha y hora de inicio que permiten aplicar temporalidad de la actividad y el valor 4° que indica la cantidad de mediciones por minuto.

## 2. Archivos de salida Actividorm

Esta aplicación se inspira en el software *Actividorm* de fabricación propia del laboratorio y escrito en C++. Mediante ingeniería inversa se determinó el funcionamiento y se replicó desde cero en R, principalmente por la falta de documentación y desorden en el código fuente, además R se usa más en ambientes académicos y de data science. No se hizo en Python porque no quise...

Los principales archivos de salida de Actividorm y con los cuales se pueden construir todas las estadísticas e indicadores para hacer análisis de sueño o actividad motora son:

### acv01

Este archivo es el más importante porque contienen la detección de sueño con el algoritmo de *Mini Mitter* que es por lejos el más usado y la determinación del cambio de estado con la regla de minutos mínimos.

```
Nombre Sujeto	Hora Registro	Estado actigrafico	Estado filtrado	Cantidad de movimiento
BenjaminVenegas	10-12-2009 16:00	S	NC	4
BenjaminVenegas	10-12-2009 16:01	W	W	0
BenjaminVenegas	10-12-2009 16:02	W	W	230
BenjaminVenegas	10-12-2009 16:03	W	W	402
BenjaminVenegas	10-12-2009 16:04	W	W	171
```

### epi

Este tipo de archivo corresponde a la asignación de noches, identificación de cada estado de sueño o vigilia detectado con los antecedentes del settings. Permite la segmentación del análisis y salidas de estadísticas generales.

```
Nombre Sujeto;Periodo del registro;Hora;Estado;Actividad;Duracion (min);Promedio actividad por minuto;Numero Episodio;Episodio del estado
BenjaminVenegas;Noche 01;10-12-2009 20:40:00;S;0;  56,0;   0,0;001;S001
BenjaminVenegas;Noche 01;10-12-2009 21:36:00;W;58021; 218,0; 266,2;002;W001
BenjaminVenegas;Noche 01;11-12-2009 01:14:00;S;1042; 151,0;   6,9;003;S002
BenjaminVenegas;Noche 01;11-12-2009 03:45:00;W;715;   5,0; 143,0;004;W002
```

### arq

Corresponde a las estadísticas que se usaban para hacer los análisis, si un registro actigráfico estuviera perfecto este archivo reflejaría las estadísticas de sueño perfectas para el sujeto, sin embargo, existen diversas y extensivas omisiones en el uso del actígrafo lo cual repercute en que este archivo que es la fuente de las estadísticas de sueño y vigilia deba ser corregido manualmente.

cada corrección depende de un protocolo, sin embargo lo más complejo es que no es replicable o automatizable ya que completamente manual. En el fondo es corregir un sistema que fue mal calculado.

```
Nombre Sujeto;Periodo del registro;Jornada;Hora Inicio;
Hora Fin;Numero Episodios W;Numero Episodios S;Duracion Total W;Duracion Total S;Movimiento Total W;Movimiento Total S;Duracion Promedio Episodios W(min);Duracion Promedio Episodios S(min);% de Tiempo de los Episodios W;% de Tiempo de los Episodios S;Promedio de actividad para episodios de W;Promedio de actividad para episodios de S;Promedio de actividad por minuto para W;Promedio de actividad por minuto para S;Promedio de actividad por hora para W;Promedio de actividad por hora para S
```

La idea principal de esta aplicación es dejar un registro escrito de las ediciones y que estas sean realizadas en la primera etapa de modo que no solo quede un registro, sino que se puedena mover parámetros y reprocesar registros sin tener que realizar todo el proceso desde cero, como es actualmente.

**Por ejemplo**. Si se decide cambiar el tiempo mínimo para considerar un estado de sueño o vigilia como consolidado desde 5 minutos a 10 minutos.

**Con el sistema actual**: Habría que reprocesar y se perderán la ediciones hechas en el setting de 5 mintuos, por lo cual habría que realizar las correcciones desde cero.

**Con el nuevo sistema**: Solo habría que modificar el setting a 10 minutos, ya que la selección y edición ya estaría realizada.

### Manuales

Los manuales originales a partir del cual se hizo la ingeniería inversa se encuentran en la carpeta **Archivos**

## 3. Main Script

Como la aplicación estará implementada con programa web mediante Shiny de RStudio, no hay ejecutable, y se depende de un Script principal que carga el ambiente de trabajo, determina algunos parámetros y carga la aplicación.

1. Borra el *environment*
2. Carga todas las funciones que se utilizarán 
3. Carga las settings en forma de **lista** 
4. Ejecuta la aplicación.

El control del resto de los procesos está depositado en la aplicación.

### Archivo de Settings

Mediante un archivo de texto de nombre `parametros.set` se guardan y cargan los parámetros principales para el análisis actigráfico. Se cargan mediante la función **`function_loader()`**

```
Valores de configuración del sistema, modificar la linea correspondiente.

---- Parametros de la Sueño ---------------------
Inicio noche: Hora a partir del cual comienza a buscar noche en formato "hh:mm"
ininoc = 20:00

Primer sueño: Tiempo minimo de sueño para determinar que comienza la noche en formato "hh:mm". Para replicar resultados de actividorm deber ser el (minutos - 1) porque ese programa tiene un error.
dursleep = 00:30

---- Parametros de la Vigilia -------------------
Inicio día: Hora a partir de la cual comienza a buscar el inicio del día en formato "hh:mm"
inidia = 06:00

Primera vigilia: Tiempo minimo de vigilia para determinar que comienza el día en formato "hh:mm".
durawake = 00:30


---- Parametros Adicionales ---------------------
Cambio de estado: Cantidad mínima de minutos para determinar un cambio de estado de sueño o vigilia. Formato "hh:mm"
statedur = 00:05

Sensibilidad: Valor para la detección de sueño o vigilia, puede ser: 20, 40, 80.
sensi = 40
```

Simplemente hay que modificarlo según se necesite.

> A futuro se debe permitir utilizar varios archivos de ajustes.



# 4. Aplicación Shiny



# 1) Pestaña de los Archivos

En esta pestaña se puede chequear y determinar el directorio de trabajo, que entrega el status de los archivos y detalles de la configuración de detección.

## Cuadro directorio

Al cargar la aplicación lo primero que hace la aplicación es:

> Buscar el archivo **savedir** que se guarda en el `USERPROFILE` de Windows y contiene la carpeta en donde se está trabajando, la idea es que quede guardada para la próxima sesión.
>
> * Si no existe crea un nuevo archivo y deja el working directory en la carpeta `USERPROFILE` de Windows
> * Si existe el archivo carga como directorio de trabajo el contenido guardado en la última sesión

```R
# El savedir se guarda en el reactivo
path$ruta
```

El directorio se muestra en un cuadro de tipo `verbatimTextOutput` y corresponde a `path$ruta`

## Botón buscar

Como `path$ruta` es un valor reactivo si lo cambio se actualiza todo, este botón lo que hace es mostrar un cuadro de diálogo para buscar un directorio que contenga archivos AWD para trabajar, modifica el **savedir** y al hacer el cambio actualiza todo.

Al usar el comando de R `choose.dir()` se producen varias limitaciones, hasta el momento he detectado las siguiente:

* Sólo funciona en Windows
* De momento sólo he hecho pruebas en Windows 10
* No funciona en Firefox

## Cargar el directorio

Ya determinado el directorio donde se trabajará la aplicación lee el contenido del directorio buscando archivos AWD para poder cargar, chequea continuamente (cada 0.1 segundo) si hay cambios en los archivos, de ser así el **reactivo** `my_files()` se actualiza y al hacerlo afecta al **reactivo** `subjectDF()` que depende de la función `load.awdfolder()` que controla la lectura de archivos.

> Con el reactivePoll y un par de funciones se lee continuamente el directorio de trabajo

### `subjectDF()`

```R
# Uno de los reactivos más importantes el data.frame de los subjects.
subjectDF()
# Depende de la función
load.awdfolder()
```

## Archivo de Settings

Si el directorio seleccionado no contiene archivos AWD válidos resultará en un data.frame de `[1,1]` con la instrucción **"Directorio sin archivos AWD"** si ocurre esto asignará todas las settings como valor ausente `NA`

Si por lo contrario existen archivos para trabajar se ejecuta la función `getset()`

### `getset(awdpath())`

Esta función revisa si en el `awdpath()` existe o no el archivo **settings.sleep** de ser así lo lee y carga los settings, si no existe creará uno nuevo con una configuración tipo que debe ser adaptada a cada caso.

> **La primera vez que se carga un directorio con archivos AWD y se crea el settings.sleep se debe revisar y adaptar al tipo de registro**

Para modificar el archivo de settings basta con abrirlo en un editor de texto y guardarlo así tal cual, sin extensión.

> Los settings cargados al sistema determinarán la detección de episodios de sueño y vigilia, razón por la cual es muy importante revisar que estén bien configurados.

Los valores de detección ser guardan en objetos estándar.

## Procesar

Este botón es el que procesa los archivos AWD en originales con el formato clásico de `Minimitter`.

> Este proceso de detección tienen 

El detalle de como se hace está en el PowerPoint



# 2) Pestaña del Actograma

Esta pestaña sus datos desde el data frame `subjectDF()`, selecciona los sujetos disponibles para edición y los lista en esta pestaña. Por defecto se selecciona el primero.

> Cada vez que se selecciona un sujeto se actualiza el valor de `awdfile()`, esto provoca que se seleccione un sujeto. Esto a su vez activa un `reactivePoll` 

### `reactivePoll`

Un `reactivePoll` en Shiny es una herramienta que cada cierto tiempo (250ms en este caso) chequea una función y si el resultado cambia ejecuta una segunda función y así al infinito.

Si a un sujeto cualquiera se le hace la detección se le crean 2 archivos, el data frame de la detección y la lista de filtros, entonces si se selecciona un sujeto y se le asigna un valor a `awdfile()` el `reactivePoll` lee la información de ambos archivos y si la fecha|hora de modificación del archivo cambia, se vuelven a cargar, de esta forma se mantienen siempre actualizados a los últimos cambios. 

Por lo tanto mientras exista un sujeto seleccionado mediante el listado `subjectDF()` y  `awdfile()` existirán 2 archivos reactivos cargados en memoria que se actualizan con un `reactivePoll`.

1. `filterRDS()` que **es la lista de filtros**. La lista contiene un header con las fecha|hora de inicio y fin del registro y un data frame con el detalle de los filtros.
2. `acveditRDS()` que carga las detecciones del sujeto que a su vez contienen los marcadores de filtros se crea mediante la aplicación de la función `check.acvfilter` al `awdfile()` y la lista settings. Por lo tanto **es el data frame de la detección (+ filtros) separados por días y contenido en una lista**.

### `check.acvfilter()`

Esta función mantiene sincronizado el filtro con el archivo de detección, de forma que si se hacen cambios en los filtros en el proceso de edición y se modifica el archivo de filtro, esto generará una modificación en la propiedades del archivo, las cuales serán detectadas por el `reactivePoll` y se ejecutará esta función para re cargar el archivo de detecciones.

> Para el funcionamiento de esta función el paso a destacar es **la aplicación de la función `create.semiper()`** que particiona el archivo en periodos (día y noche) que componen el registro del sujeto.

## Listado sujetos

Estos sujetos disponibles se muestran mediante un **renderUI** que carga los sujetos con **Status** igual a "**En edicion**" que quedan cargados al leer el directorio de trabajo en el `subjectDF()`. 

De esta forma si se termina o quita un archivo de AWD esta lista se actualizará en línea porque es un reactivo. 

> La selección se guarda en el reactivo `awdfile()`

## Botón editar

Este botón sólo envía al usuario a la pestaña de edición.

## Actograma

El actograma es un constructo algo complicado porque es la mezcla de varios elementos gráficos del `Base` de **R** que se apilan y combinan en un sólo elemento. Todo está controlado mediante la función `create.actogram()` y `create.plotActo()`

> Lo primero es el tamaño del actograma que resultó ser un problema porque generaba mucho error y cada sujeto tenía un número variable de días en sus registros.

Lo diseñé de esta forma porque resultó ser lo más rápido, `ggplot` resultó ser un fiasco de velocidad aunque era ideal porque quedaba muy bonito. En su lugar y abusando de la construcción gráfica base del R se apilan todos los elementos que dibujan el actograma.

### `output$actoUI`

El actograma no es un elemento gráfico estándar porque la cantidad de días de cada sujeto es diferente y no es posible hacer flexible este parámetro en un elemento `plotOutput`. Para ello se crea el contenedor en un render.

Este render UI crea un output tipo plot con una altura igual a la **cantidad de días del registro x 120 pixeles**. La cantidad de días se extrae del reactivo `acveditRDS()` que la detección particionada en días.

Este contenedor recibe el resultado del `output$actograma`

### `output$actograma`

Este output fabrica el actograma y su resultado se envía como al contenedor del gráfico. **Su funcionamiento es la aplicación de la función `create.actogram()`**

Si un sujeto no tiene datos crea un gráfico en blanco.

### `create.actogram()`

Esta función define la escala a mostrar desde los settings. Lo que hace es básicamente compilar los gráficos de días individuales.

1. Crea la escala superior
2. Determina la cantidad de días disponibles a graficar
3. Para cada día crea un gráfico utilizando la función `create.plotActo()` que es un plot individual. 
4. Crea el marco de abajo

Entonces es un apilado de gráficos dependientes de la función `create.plotActo()`

### `create.plotActo()`

Esta función grafica un día del registro de detección de un sujeto. Tiene varios componentes:

1. Calcula las fechas de inicio y final del registro
2. Revisa si hay un filtro **Tipo 4** que es **"Mover la noche"** y si se encuentran en el período que se está analizando. No debieran haber más de 2, no está ex
3. Datos para los ejes: X escala y etiquetas, Y altura del gráfico **La función tiene un multiplicador para modificar el Y**
4. Coordenadas de sueño y vigilia que se superponen como cuadrados en el gráfico son 2 data frame.
5. Coordenadas de filtro automático **Tipo 1**
6. Coordenadas de filtro para semi períodos completos (día o noche) **Tipo 2**
7. Coordenadas para modificador de actividad **Tipo 3**

Luego construye el gráfico apilando elementos.

* Sueño: `col = rgb(0.4235,0.6510,0.8039,0.5), border = "skyblue3")`
* Vigilia: `col = rgb(0.9333,0.7882,0.0000,0.5), border = "gold2")`
* (1) Filtro automático: `col = rgb(1, 0, 0, 0.3), border = "red"`
* (2) Filtro manual: `col = rgb(0, 0.3921, 0, 0.3)`
* (3) Modificador de actividad: `col = "red", border = "red"` esta es una marca en el eje superior
* Agrega un gráfico nuevo encima con los datos de actividad en `col='grey20'`
* Agrega las líneas verticales.
  * El de la mitad del período `red`
  * Mover la noche `green`
  * Inicio y fin del registro `magenta`
* Y las fechas a los costados

Finalmente el `box()` para mostrar el gráfico (creo, lo hice hace tiempo).

> Ojo que los RGB en R son partidos por 255. Para replicar hacer un gráfico en blanco y ponerle el color.

### Filtros

Hay varios tipos de filtros que salen en el actograma, son los modificadores a la detección que permiten excluir períodos, mover el inicio de la noche o el día, agregar actividad ficticia cuando se cambia un sueño a vigilia.

**Tipo 1 - Filtro automático**: Es el filtro inicial que se calcula en la detección, es automático

**Tipo 2 - Filtro manual**: Es similar al anterior, sólo que se determina a mano en la pestaña de edición

**Tipo 3 - Modificar actividad**: Cuando un sueño se pasa a vigilia, se marca como una franja roja en la parte superior

**Tipo 4 - Mover periodo**: Cuando se necesita sobre escribir las reglas de asignación de inicio de noche o día.

## Botón finalizar

Para finalizar un sujeto se crea un archivo de texto que contiene la fecha de finalización del registro. Se debe marcar cuando toda la edición a la detección está lista.

> Este botón solo crea un archivo 

El archivo se usa para determina lo que sirve para editar y quedan como terminados en el reactivo `subjectDF()` y por lo tanto no se muestran en la pestaña del actograma.

> Si se necesita revisar el sujeto bastaría con borrar el archivo de termino, lo cual activa el proceso reactivo y vuelve a ponerlo en línea como editable.

Si el archivo de filtros tienen los valores de inicio y fin se muestra un modal para continuar de lo contrario advierte que no se han configurado y muestra un modal (ventanita) que impide finalizar al suejto.



# 3) Pestaña de Edición

En la pestaña de edición se hace los retoques a la detección automática y cuyos antecedentes determinan el resultado final cuando se aplique la función para calcular el archivo EPI.

Desde la selección de sujeto que se hace en la pestaña del actograma se carga todo, es decir, la variable reactiva `awdfile()` da inicio a todo el proceso.

## Sujeto y Período a editar

Lo primero que se muestra es el sujeto que está seleccionado `awdfile()` y sirve para asegurarse que se está editando el sujeto correcto.

Lo siguiente es es desglosar los datos del sujeto mostrando los periodos posibles para editar, estos se sacan el objeto lista  `acveditRDS()` específicamente desde el data frame `timelist`.

Entonces se muestran en un elemento **renderUI** el listado de períodos disponibles. Los cuales se deben seleccionar. Esto provocará que se cree un gráfico del segmento seleccionado, con algunos controles para agrandar, resetear y modificar el tamaño.

## Tabla de estados

Esta tabla es importante porque toma todo el período seleccionado y crea una tabla con los estados de sueño y vigilia que permite hacer todos los indicadores para mostrar en la edición. Se crea usando la función `stagesTable()`

### `stagesTable()`

Esta función toma como antecedente el `acveditRDS()` del sujeto, filtra el período seleccionado. Luego hace una tabla para los periodos de sueño y vigilia consecutivos, su duración y si hay o no filtros.

## Detalle del filtro

Esta sección carga el archivo `filterRDS()` con los filtros, muestra el encabezado y el data frame asociado. Esta tabla se actualiza cada vez que se hace un cambio.

### Inicio y fin

Este cuadro muestra el inicio y fin del registro que por defecto viene en blanco y se debe determinar manualmente. Sirve para establecer los límites que se leerán al crear el archivo EPI.

