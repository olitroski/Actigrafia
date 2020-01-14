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









---------

## 5. Procesado raw data (func_awd)

Las funciones para procesar los AWD son a grandes rasgos las siguientes y arrojan los siguientes resultados

### Create acv



### Create epi



### Create actogram



## 6. Actividad por hora (func_acti)

### Create actdata 

Esta función crea a partir de un awd datos para el análisis por actividad.

1. Tomar el acv y combinar con el epi para que tenga la info de periodos dia y noche
2. 





