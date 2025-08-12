library(sf)
library(terra)
library(dplyr)

# ============================================================
# Script para descargar y vectorizar fuegos EFFIS por "tiles"
# - Divide península+Baleares y Canarias en cuadrantes (EPSG:3857)
# - Descarga WMS por tile para 3 ventanas temporales (last1, last2, last3)
# - Genera PNG + PGW, umbraliza (max-30), vectoriza y guarda GeoJSON
# - Hace un dissolve final por rango y lo deja en out/<rango>/fuegos_espania.geojson
# ============================================================


# ---------------- CONFIG ----------------
ncol <- 6              # nº de columnas (Península+Baleares)
nrow <- 6              # nº de filas (Península+Baleares)

options(timeout = 60)  # timeout base; la función de descarga puede elevarlo puntualmente

crs_epsg <- "EPSG:3857"            # todo el flujo inicial trabaja en 3857 (Web Mercator)
dir.create("tiles", showWarnings = FALSE)  # carpeta temporal si se usa fuera de 'out/'

# Servicio WMS base (capa nrt.ba de EFFIS)
wms_base <- "https://maps.effis.emergency.copernicus.eu/gwis?service=WMS&request=GetMap&layers=nrt.ba&styles=&format=image/png&transparent=true&version=1.1.1&singletile=false"

# Resolución de solicitud. 'factor' permite subir detalle sin tocar el resto del código.
factor <- 2
width  <- 2048 * factor
height <- 2048 * factor

# Archivos de entrada con los bboxes en EPSG:3857
peninbal_path <- "data/peninbal.geojson"  # bbox península + Baleares
canary_path   <- "data/canary.geojson"    # bbox Canarias
# ----------------------------------------


# ------------------------------------------------------------
# Utilidad: descarga con reintentos y timeout elevable.
# - tries: nº de intentos
# - timeout_sec: timeout por intento (puede ser mayor que el global de options())
# - sleep_sec: pausa entre intentos (evita machacar al servidor)
# Devuelve TRUE si descarga válida (archivo existe y tamaño > 0); si no, FALSE.
# ------------------------------------------------------------
safe_download <- function(url, destfile, tries = 3, timeout_sec = 120, sleep_sec = 2) {
  for (t in seq_len(tries)) {
    cat("  Descarga intento", t, "...\n")
    ok <- try({
      options(timeout = timeout_sec)
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      file.exists(destfile) && file.size(destfile) > 0
    }, silent = TRUE)
    if (!inherits(ok, "try-error") && isTRUE(ok)) return(TRUE)
    Sys.sleep(sleep_sec)
  }
  FALSE
}

# ------------------------------------------------------------
# Utilidad: crear una malla regular (grid) sobre un bbox (sfc)
# - id_start controla el primer ID para este grid; útil para que
#   las IDs de Canarias no pisen a las de la península.
# ------------------------------------------------------------
make_grid <- function(bbox, ncol, nrow, id_start = 1) {
  g <- st_make_grid(st_as_sfc(bbox), n = c(ncol, nrow))
  st_sf(geometry = g) %>%
    mutate(id = id_start + row_number() - 1)
}


# ---- Tiles (lectura de bboxes) ----
# Cargamos los bboxes reales en 3857. 
# Nota: estos geojsons pueden ser un rectángulo o un polígono; st_bbox extrae la caja mínima.
pen <- st_read(peninbal_path, quiet = TRUE)
can <- st_read(canary_path,   quiet = TRUE)

pen_bbox <- st_bbox(pen)  # península + Baleares
can_bbox <- st_bbox(can)  # Canarias

# Creamos la malla para península+Baleares con 6x6.
pen_grid <- make_grid(pen_bbox, ncol, nrow, id_start = 1)

# Creamos una "malla" 1x1 para Canarias, con IDs a continuación de la península.
can_grid <- make_grid(can_bbox, 1, 1, id_start = max(pen_grid$id) + 1)

# Unimos todos los tiles en un único sf. Esta capa se usará para iterar.
all_tiles <- bind_rows(pen_grid, can_grid)

# Export de referencia (en 4326) para inspección rápida en QGIS si hace falta.
st_write(st_transform(all_tiles, 4326), "all_tiles.geojson", delete_dsn = TRUE, quiet = TRUE)


# ---- Ventanas temporales dinámicas ----
# 'today' controla los 3 rangos. Esta variable marca el "hasta" del WMS.
# La lógica aquí es: [today-1, today], [today-2, today], [today-3, today].
today <- Sys.Date()

# Esta lista define los tres rangos que se van a procesar.
# Se usa más abajo para construir el parámetro 'time' y el nombre de carpeta de salida.
ranges <- list(
  list(label = "last1", start = today - 1, end = today),
  list(label = "last2", start = today - 2, end = today),
  list(label = "last3", start = today - 3, end = today)
)

fmt <- function(d) format(d, "%Y-%m-%d")  # formateo YYYY-MM-DD para el WMS y para las carpetas

# ============================================================
# Bucle principal por rango temporal
# - Cada iteración crea su carpeta de salida
# - Descarga, umbraliza, vectoriza por tile
# - Guarda los GeoJSON por tile y luego hace el dissolve final
# ============================================================
for (rg in ranges) {
  # time_param se utiliza en la URL del WMS; out_dir se reutiliza para escribir todo lo del rango.
  time_param <- paste0(fmt(rg$start), "/", fmt(rg$end))
  out_dir <- file.path("out", paste0(fmt(rg$start), "_to_", fmt(rg$end)))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "tiles"), showWarnings = FALSE)
  
  cat("\n=== Rango temporal:", time_param, "===\n")
  
  vect_list <- list()  # aquí acumularemos las geometrías de todos los tiles válidos de este rango
  
  # -----------------------------------------
  # Bucle por tile
  # -----------------------------------------
  for (i in seq_len(nrow(all_tiles))) {
    bb <- st_bbox(all_tiles[i, ])
    bbox_str <- paste(bb, collapse = ",")  # se inyecta tal cual en la URL del WMS
    
    # Construcción de la URL de descarga WMS para este tile.
    url <- paste0(
      wms_base,
      "&time=", time_param,
      "&width=", width,
      "&height=", height,
      "&srs=", crs_epsg,
      "&bbox=", bbox_str
    )
    
    tile_id <- all_tiles$id[i]
    # Las rutas dependen de out_dir: esto separa claramente los resultados por rango temporal.
    png_path <- file.path(out_dir, "tiles", sprintf("tile_%d.png", tile_id))
    pgw_path <- sub("\\.png$", ".pgw", png_path)
    geojson_path <- file.path(out_dir, "tiles", sprintf("tile_%d.geojson", tile_id))
    
    cat("Tile", tile_id, "→\n  URL:", url, "\n")
    
    # ---------- Descarga con reintentos ----------
    ok <- safe_download(url, png_path, tries = 3, timeout_sec = 120)
    if (!ok) {
      warning("  No se pudo descargar tile ", tile_id, ". Se omite.\n")
      next  # pasamos al siguiente tile
    }
    
    # ---------- World file (.pgw) ----------
    # A, E: tamaño de píxel (X e Y). C, F: coordenadas del centro del píxel superior izquierdo.
    xmin <- bb["xmin"]; ymin <- bb["ymin"]; xmax <- bb["xmax"]; ymax <- bb["ymax"]
    A <- (xmax - xmin) / width
    E <- -(ymax - ymin) / height
    C <- xmin + A/2
    F <- ymax + E/2
    writeLines(sprintf("%.15f", c(A, 0, 0, E, C, F)), con = pgw_path)
    
    # ---------- Lectura del raster y blindajes ----------
    # 'stk' suele traer 4 bandas (RGB + alfa). Si no se puede leer, saltamos.
    stk <- try(rast(png_path), silent = TRUE)
    if (inherits(stk, "try-error")) {
      warning("  No se pudo leer raster del tile ", tile_id, ". Se omite.\n")
      next
    }
    
    # Si hay canal alfa, comprobamos si el tile es totalmente transparente: en ese caso no aporta geometría.
    if (nlyr(stk) >= 4) {
      alpha_max <- global(stk[[4]], max, na.rm = TRUE)[1,1]
      if (!is.finite(alpha_max) || alpha_max <= 0) {
        message("  Tile ", tile_id, ": completamente transparente (alfa = 0). Se omite.")
        next
      }
    }
    
    # Tomamos la banda 1 como referencia de intensidad y le inyectamos la georreferenciación.
    r <- stk[[1]]
    ext(r) <- ext(xmin, xmax, ymin, ymax)
    crs(r) <- crs_epsg
    
    # Si el máximo es <= 0, consideramos que no hay señal útil y saltamos.
    max_val <- global(r, max, na.rm = TRUE)[1,1]
    if (!is.finite(max_val) || max_val <= 0) {
      message("  Tile ", tile_id, ": sin señal (max <= 0). Se omite.")
      next
    }
    
    # ---------- Umbral flexible ----------
    # Nos quedamos con todo lo que esté por encima de (max_val - 30).
    # Esto captura contornos que, por suavizado o antialiasing del PNG, no llegan exactamente al máximo.
    r_bin <- r > (max_val - 30)
    
    # ---------- Vectorización ----------
    # as.polygons() devuelve un SpatVector; pedimos valores y hacemos dissolve en el paso de raster→vector.
    vect_pol <- as.polygons(r_bin, dissolve = TRUE, values = TRUE, na.rm = TRUE)
    
    # La columna con el valor binario suele llamarse 'lyr.1' o '<nombre_raster>_1'.
    # Para no depender del nombre, tomamos la primera columna de atributos y filtramos '== 1'.
    val_col <- names(vect_pol)[1]
    vect_pol <- vect_pol[ vect_pol[[val_col]] == 1 ]
    
    # Si después del filtrado hay geometría, la guardamos y acumulamos para el dissolve final.
    if (nrow(vect_pol) > 0) {
      vect_sf <- st_as_sf(vect_pol)
      st_write(vect_sf, geojson_path, delete_dsn = TRUE, quiet = TRUE)
      vect_list[[length(vect_list) + 1]] <- vect_sf
      cat("  OK: ", basename(geojson_path), "\n")
    } else {
      message("  Tile ", tile_id, ": sin polígonos tras vectorizar. Se omite.")
    }
  }
  
  # ---------- Dissolve final del rango ----------
  # Aquí ya no nos interesan atributos, sólo la geometría. Unimos todo, validamos y disolvemos.
  if (length(vect_list) > 0) {
    geom_all   <- do.call(c, lapply(vect_list, st_geometry))  # concatenamos sfc
    geom_valid <- st_make_valid(geom_all)                      # repara geometrías problemáticas
    final      <- st_sf(geometry = st_union(geom_valid)) %>%   # disuelve fronteras internas
      st_transform(4326)                                       # salida en WGS84
    
    final_path <- file.path(out_dir, "fuegos_espania.geojson")
    st_write(final, final_path, delete_dsn = TRUE, quiet = TRUE)
    cat("✔ Capa final creada:", final_path, "\n")
  } else {
    cat("✖ No se generaron polígonos para", time_param, "\n")
  }
}

cat("\nHecho.\n")
