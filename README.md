# Olito Sleep

Aplicación para analizar sueño y vigilia a partir de archivos AWD de Actigrafía.



## Instalar la aplicación

Seguir al pie de la letra las instrucciones de instalación.

### Sistema operativo

Lo primero es decir que esta aplicación solo ha sido probada en **Windows 10**, puede generar errores en otros sistemas porque la función de R para buscar directorio y que está incluida en la app podría no funcionar, al igual que la ruta al directorio del usuario.

Instalar 





### Instalar la librería devtools

Esta librería permite instalar la aplicación directamente desde el repositorio de GitHub [github.com/olitroski/Actigrafia](https://github.com/olitroski/Actigrafia). Para esto 





```R
# Devtools
install.packages("devtools")
library(devtools)

# Instalar la aplicacion
devtools::install_github("olitroski/Actigrafia/package", update = "never")

```

