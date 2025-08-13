# Descarga y Vectorización de Datos de Incendios EFFIS

Este proyecto permite descargar datos de incendios desde el servicio WMS de **Copernicus EFFIS (European Forest Fire Information System)**, dividiendo España en varios cuadrantes para cubrir toda la península, Baleares y Canarias.
Cada cuadrante se procesa para extraer los polígonos de áreas quemadas en los últimos 1, 2 y 3 días, generando ficheros **GeoJSON** listos para usar en QGIS u otros SIG.

El script descarga desde el visor del EFFIS https://forest-fire.emergency.copernicus.eu/apps/effis_current_situation/index.html. Los polígonos que generan no tienen ningún tipo de información como nombre de incendio, hectáreas afectadas, etc.

---

## Requisitos

Este script está desarrollado en **R** y utiliza las siguientes librerías:

- [`sf`](https://r-spatial.github.io/sf/)
- [`terra`](https://rspatial.org/terra/)
- [`dplyr`](https://dplyr.tidyverse.org/)

Instalación en R:

```r
install.packages(c("sf", "terra", "dplyr"))
```


Correr el script `effis_ba_tiles.R`.

Puedes echar un ojo a los archivos en mapshaper.org a través del siguiente enlace:

https://mapshaper.org/?q&files=https://raw.githubusercontent.com/LuisSevillano/effis_current_situation/refs/heads/main/out/modis.ba/2025-08-12_to_2025-08-13/spain_fires_modis_ba.geojson,https://raw.githubusercontent.com/LuisSevillano/effis_current_situation/refs/heads/main/out/nrt.ba/2025-08-12_to_2025-08-13/spain_fires_nrt_ba.geojson