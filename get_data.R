library(sf)
library(terra)
library(dplyr)

# ---------------- CONFIG ----------------
# Número de columnas y filas para la península
ncol <- 6
nrow <- 6
options(timeout = 60)

# CRS
crs_epsg <- "EPSG:3857"

# Carpeta de salida
dir.create("tiles", showWarnings = FALSE)

# Fecha del WMS
time_param <- "2025-08-11/2025-08-12"

# Base URL WMS
wms_base <- "https://maps.effis.emergency.copernicus.eu/gwis?service=WMS&request=GetMap&layers=nrt.ba&styles=&format=image%2Fpng&transparent=true&version=1.1.1&singletile=false"
factor <- 2
width <- 2048 * factor
height <- 2048 * factor
# ----------------------------------------

# 1) Bounding boxes aproximados en EPSG:3857
pen_bbox <- st_read("data/peninbal.geojson", quiet = TRUE)
canary <-  st_read("data/canary.geojson", quiet = TRUE)
# Obtener bounding box real en EPSG:3857
pen_bbox <- st_bbox(pen_bbox)  # Esto reemplaza mis valores aproximados
canarias_bbox <- st_bbox(canary)  # Esto reemplaza mis valores aproximados

# 2) Función para dividir en cuadrantes
make_grid <- function(bbox, ncol, nrow, id = row_number()) {
  st_make_grid(st_as_sfc(bbox), n = c(ncol, nrow)) %>%
    st_sf(geometry = .) %>%
    mutate(id = id)
}

pen_grid <- make_grid(pen_bbox, ncol, nrow)
canarias_tile <- make_grid(canarias_bbox, 1, 1, max(pen_grid$id) + 1)

# 4) Unión de todos los tiles
all_tiles <- bind_rows(pen_grid, canarias_tile)

st_write(all_tiles %>% st_transform(4326),
         'all_tiles.geojson',
         delete_dsn = T)

# Función de descarga con reintentos
safe_download <- function(url, destfile, tries = 3, timeout_sec = 60) {
  for (t in seq_len(tries)) {
    cat("Intento", t, "de descarga...\n")
    try({
      options(timeout = timeout_sec)
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      if (file.exists(destfile) && file.size(destfile) > 0) {
        return(TRUE) # éxito
      }
    }, silent = TRUE)
    Sys.sleep(2) # pequeña pausa antes de reintentar
  }
  return(FALSE) # fallo después de todos los intentos
}


# 5) Procesar cada tile
vect_list <- list()

for (i in seq_len(nrow(all_tiles))) {
# for (i in c(26, 32)) {
  bb <- st_bbox(all_tiles[i, ])
  bbox_str <- paste(bb, collapse = ",")
  
  # Construir URL
  url <- paste0(
    wms_base,
    "&time=",
    time_param,
    "&width=",
    width,
    "&height=",
    height,
    "&srs=",
    crs_epsg,
    "&bbox=",
    bbox_str
  )
  print(url)
  # Rutas
  tile_id <- all_tiles$id[i]
  png_path <- file.path("tiles", paste0("tile_", tile_id, ".png"))
  pgw_path <- sub("\\.png$", ".pgw", png_path)
  geojson_path <- file.path("tiles", paste0("tile_", tile_id, ".geojson"))
  #
  # # Descargar PNG
  cat("Descargando tile", tile_id, "...\n")
  ok <- safe_download(url, png_path, tries = 3, timeout_sec = 120)
  if (!ok) {
    warning("No se pudo descargar tile ", tile_id, ", se omite.\n")
    next
  }
  #
  # # Crear world file
  xmin <- bb["xmin"]
  ymin <- bb["ymin"]
  xmax <- bb["xmax"]
  ymax <- bb["ymax"]
  A <- (xmax - xmin) / width
  E <- -(ymax - ymin) / height
  C <- xmin + A / 2
  F <- ymax + E / 2
  cat("Guardando tile", tile_id, "...\n")
  writeLines(sprintf("%.15f", c(A, 0, 0, E, C, F)), con = pgw_path)
  
  # Leer y procesar
  r <- rast(png_path)[[1]]
  ext(r) <- ext(xmin, xmax, ymin, ymax)
  crs(r) <- crs_epsg
  
  cat("Calculando valor máximo", tile_id, "...\n")
  max_val <- global(r, max, na.rm = TRUE)[1, 1]
  if (!is.finite(max_val) || max_val <= 0) {
    message("Tile ", tile_id, " sin señal (max <= 0). Se omite.")
    next
  }
  r_bin <- r > (max_val - 30)  # margen de 30
  
  # Vectorizar
  cat("Vectorizando", tile_id, "...\n")
  vect_pol <- as.polygons(r_bin,
                          dissolve = TRUE,
                          values = TRUE,
                          na.rm = TRUE)
  
  # Nombre de la primera columna de atributos
  val_col <- names(vect_pol)[1]
  
  # Filtrar solo donde el valor sea 1
  cat("Filtrando", tile_id, "...\n")
  vect_pol <- vect_pol[vect_pol[[val_col]] == 1]
  
  if (nrow(vect_pol) > 0) {
    vect_sf <- st_as_sf(vect_pol)
    cat("Guardando", tile_id, "...\n")
    st_write(vect_sf,
             geojson_path,
             delete_dsn = TRUE,
             quiet = TRUE)
    vect_list[[length(vect_list) + 1]] <- vect_sf
  }
}

# 6) Dissolve final
if (length(vect_list) > 0) {
  # Arreglar geometrías y disolver
  final <- vect_list  %>%
    lapply(st_geometry) %>%
    do.call(what = c) %>%
    st_make_valid() %>%
    st_union() %>%
    st_sf(geometry = .) %>% 
    st_transform("EPSG:4326")
  
  st_write(final %>% st_transform("EPSG:4326"),
           "spain_fires.geojson",
           delete_dsn = TRUE)
  cat("Capa final creada: fuegos_espania.geojson\n")
} else {
  cat("No se generaron polígonos.\n")
}
