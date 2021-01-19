# Olito Sleep

Aplicación para analizar sueño y vigilia a partir de archivos AWD de Actigrafía.



## Instalar la aplicación

Seguir al pie de la letra las instrucciones de instalación. La aplicación fue diseñada con la librería Shiny de R, por lo cual necesitaremos varias cosas instaladas antes de utilizarla.

### Sistema operativo

Lo primero es decir que esta aplicación solo ha sido probada en **Windows 10**, puede generar errores en otros sistemas porque la función de R para buscar directorio y que está incluida en la app podría no funcionar, al igual que la ruta al directorio del usuario.

### Instalar R

Lo siguiente será instalar una copia de R. Recientemente se liberó la versión 4 de R la cual tiene una modificación a la función `data.frame` y puede provocar problemas según las últimas pruebas. Por lo tanto se debe instalar una versión anterior.

[R 3.6.3](https://cran.r-project.org/bin/windows/base/old/3.6.3) (February, 2020) - [Descargar](https://cran.r-project.org/bin/windows/base/old/3.6.3/R-3.6.3-win.exe)

### Instalar RStudio

Luego se necesita una copia de RStudio, que se puede encontrar en este enlace [rstudio.com/products/rstudio/download](https://rstudio.com/products/rstudio/download/#download), ya que desde este programa se iniciará la aplicación.

### Instalar la librería devtools

Esta librería permite instalar la aplicación directamente desde el repositorio de GitHub [github.com/olitroski/Actigrafia](https://github.com/olitroski/Actigrafia). Para esto se debe escribir lo siguiente en la consola.

![](https://raw.githubusercontent.com/olitroski/Actigrafia/master/varios/consola.png)

Deviera lucir más o menos así

```R
# Devtools
install.packages("devtools")
```

### Instalar la aplicación

Para instalar la aplicación lo primero será cargar la librería `devtools` e instalar con las siguientes  instrucciones

```R
# Cargar libreria
library(devtools)

# Instalar la aplicacion
devtools::install_github("olitroski/Actigrafia/package")
```

Pudiera ser que tome un tiempo en ejecutar, lo importante es que debiera lucir así:

![](https://raw.githubusercontent.com/olitroski/Actigrafia/master/varios/install_update.png)

Se nos ofrecerá la opción de actualizar todas las librerías del sistema, como no es necesario en la mayoría de los casos basta con seleccionar **escribir la opción 3 y apretar ENTER**.

```
Enter one or more numbers, or an empty line to skip updates: 3
```

Luego de esto se instalará la aplicación. Las últimas líneas debieran decir lo siguiente.

```
** testing if installed package keeps a record of temporary installation path
* DONE (olitosleep)
```

Y con esto ya estaríamos listos. Si surgen errores de algún tipo se deben evaluar y corregir. El más recurrente es que no puede remover una librería antigua, en este caso `digest` si se borra la carpeta de la librería y se reinstala funciona ok.

## Reinstalar la aplicación

Instalar una nueva versión sobre una versión previa provocar problemas. Algo así:  

```
Installing package into ‘D:/Documentos/R/win-library/3.6’
(as ‘lib’ is unspecified)
* installing *source* package 'olitosleep' ...
ERROR: cannot remove earlier installation, is it in use?
* removing 'D:/Documentos/R/win-library/3.6/olitosleep'
* restoring previous 'D:/Documentos/R/win-library/3.6/olitosleep'
```

Para evitar esto lo mejor será ejecutar el RStudio como administrador, para ello basta con hacer clic derecho en el icono del programa y buscar la opción **Ejecutar como administrador**.

![](https://raw.githubusercontent.com/olitroski/Actigrafia/master/varios/asadmin.png)

Luego se debe instalar como ya se indicó.

## Ejecutar la aplicación

Esta aplicación es en el fondo una librería de R, por lo cual se debe cargar y luego ejecutar la función `olitosleep()`. Lo anterior de la instalación ya no se debe hacer y de aquí en adelante se deben ejecutar estas dos lineas.

```R
# Cargar librería
library(olitosleep)

# Cargar aplicación
olitosleep()
```

Cuando se abre la aplicación se abrirá cargará todo lo necesario y se abrirá una nueva ventana.

![](https://raw.githubusercontent.com/olitroski/Actigrafia/master/varios/app.png)

No se debe cerrar RStudio porque se cerrará la aplicación, si se desea salir simplemente basta cerrarla, no pasa nada si se corta la luz o algo así.

# Anexo

## Detalle de las estadísticas

El detalle de las estadísticas se puede encontrar acá. [Procesamiento de datos](https://github.com/olitroski/Actigrafia/blob/master/Procesamiento%20de%20datos.md)

## Archivos de prueba

Se pueden descargar archivos de prueba para dejarlos en alguna carpeta y probar el programa, es un archivo comprimido en zip. 

