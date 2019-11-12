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

## 2. Settings

Hay un script de R que contiene los parámetros iniciales `settings.R` de momento tiene las siguientes variables.





## 3. Main Script

El Script principal de momento lo tengo en 1 archivo de R, luego separo y ordeno, tengo que traer los script para las stats.

## 4. Varios

La carpeta de antecedentes tiene las salidas de archivo del programa actividorm de fabricación propia del Lab. este programa se construyó con ingeniería inversa porque el original en C++ es un desastre, sin documentación, un spagetti code horroroso y como no sé mucho de C++ aborté misión y lo hice desde cero en R.

Hay otros archivos pero estos son los importantes en términos de datos, los otros eran de transición imagino. Yo no hago eso y dejo todo en las funciones.

### acv01

Este archivo es el más importante porque contienen la detección de sueño con el algoritmo de mini mitter y la determinación del cambio de estado con la regla de minutos mínimos

```
Nombre Sujeto	Hora Registro	Estado actigrafico	Estado filtrado	Cantidad de movimiento
BenjaminVenegas	10-12-2009 16:00	S	NC	4
BenjaminVenegas	10-12-2009 16:01	W	W	0
BenjaminVenegas	10-12-2009 16:02	W	W	230
BenjaminVenegas	10-12-2009 16:03	W	W	402
BenjaminVenegas	10-12-2009 16:04	W	W	171
```

### epi

Este tipo de archivo es la identificación de cada stage de sueño o vigilia detectado con los antecedentes del settings, se usa mucho para el actograma y posteriores estadísticas.

```
Nombre Sujeto;Periodo del registro;Hora;Estado;Actividad;Duracion (min);Promedio actividad por minuto;Numero Episodio;Episodio del estado
BenjaminVenegas;Noche 01;10-12-2009 20:40:00;S;0;  56,0;   0,0;001;S001
BenjaminVenegas;Noche 01;10-12-2009 21:36:00;W;58021; 218,0; 266,2;002;W001
BenjaminVenegas;Noche 01;11-12-2009 01:14:00;S;1042; 151,0;   6,9;003;S002
BenjaminVenegas;Noche 01;11-12-2009 03:45:00;W;715;   5,0; 143,0;004;W002
```

### arq

Corresponde a las estadísticas que se usaban para hacer los análisis, este es el archivo que posteriormente se le hacen correcciones manuales, la idea de todo esto es no llegar a hacer este tipo de correcciones y que este tipo de archivos sea final.

```
Nombre Sujeto;Periodo del registro;Jornada;Hora Inicio;Hora Fin;Numero Episodios W;Numero Episodios S;Duracion Total W;Duracion Total S;Movimiento Total W;Movimiento Total S;Duracion Promedio Episodios W(min);Duracion Promedio Episodios S(min);% de Tiempo de los Episodios W;% de Tiempo de los Episodios S;Promedio de actividad para episodios de W;Promedio de actividad para episodios de S;Promedio de actividad por minuto para W;Promedio de actividad por minuto para S;Promedio de actividad por hora para W;Promedio de actividad por hora para S
```

### Manuales

Los manuales originales a partir del cual se hizo la ingeniería inversa









