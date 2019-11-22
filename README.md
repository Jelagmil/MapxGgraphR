MapxGgraphR
================
Jesús Lagos @Vdot\_spain <jelagmil@gmail.com>
2019-11-21

<!-- README.md is generated from README.Rmd. Please edit that file -->
La libreria MapxGgraphR está pensada para poder hacer visualizaciones de mapas de disparos y agregados de xG a partir de datos de entrada de disparos con los que se calcula el xG del disparo con un modelo propio.

Versiones
=========

Versión 0.1

Instalación y Ayuda
-------------------

Para instalar el paquete desde Github:

``` r
devtools::install_github('jelagmil/MapxGgraphR', build_opts = c("--no-resave-data", "--no-manual"))
library(MapxGgraphR)
```

Una vez instalado puedes leer la explicación de la libreria ejecutando:

``` r
#Para que se abra en el navegador:
browseVignettes("MapxGgraphR")

#Para que se abra en la pestaña de Help de RStudio:
vignette("MapxGgraphR")
```

Para conocer cada función y como se usa cada una también puedes usar "?" seguido de la función para mostrar la ayuda en la pestaña de Help de RStudio. Si apretas F1 con el cursor puesto en la función también se despliega en la ayuda:

``` r
?xGgraph
```

xGgraph
-------

Con esta función `xGgraph()` creamos el mapa con los datos de los tiros de dos equipos, calculando para cada disparo el xG y calculando el resultado a partir de los xG. La función en la consola te va pidiendo los datos para construir el campo con los tiros.

En la consola te preguntará: - identificador de partido. Para que se guarde un fichero con los datos que vas a ir introduciendo - Equipo Local para que introduzas el literal del equipo - Número de disparos que vas a introducir - Coordenada x e y. Para ello debes usar la función OptaMAPcampofutbol2() para que puedas ubicar el disparo - El minuto en el que se produce el disparo - Parte del cuerpo con la que ha disparado - Bajo que siutación ha sido - Si ha sido de tiro de penalty - Si ha sido un cara a cara contra el portero - Si ha acabado en gol - Quién ha metido el gol

``` r
#Si la invocamos direcamente con 1 o 2 en función de los equipos.
xGgraph()
```

quedaría:

<img src="vignettes/imagen1.png" alt="Resultado de la entrada de datos" width="100%" />

FUTURAS MEJORAS
---------------

-   Todo se puede mejorar
