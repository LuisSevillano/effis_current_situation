# Descarga y Vectorización de Datos de Incendios EFFIS

Este proyecto permite descargar datos de incendios desde el servicio WMS de **Copernicus EFFIS (European Forest Fire Information System)**, dividiendo España en varios cuadrantes para cubrir toda la península, Baleares y Canarias.
Cada cuadrante se procesa para extraer los polígonos de áreas quemadas en el último día, generando un fichero llamado **GeoJSON** `spain_fires.geojson` listos para usar en QGIS u otros SIG.

---

## 📦 Requisitos

Este script está desarrollado en **R** y utiliza las siguientes librerías:

- [`sf`](https://r-spatial.github.io/sf/)
- [`terra`](https://rspatial.org/terra/)
- [`dplyr`](https://dplyr.tidyverse.org/)

Instalación en R:

```r
install.packages(c("sf", "terra", "dplyr"))


Correr el script `get_data.R`.