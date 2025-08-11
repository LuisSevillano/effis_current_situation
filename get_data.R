library(sf)
library(terra)
library(dplyr)

# ---------------- CONFIG ----------------
# Número de columnas y filas para la península
ncol <- 6
nrow <- 6

# CRS
crs_epsg <- "EPSG:3857"

# Carpeta de salida
dir.create("tiles", showWarnings = FALSE)

# Fecha del WMS
time_param <- "2025-08-10/2025-08-11"

# Base URL WMS
wms_base <- "https://maps.effis.emergency.copernicus.eu/gwis?service=WMS&request=GetMap&layers=nrt.ba&styles=&format=image%2Fpng&transparent=true&version=1.1.1&singletile=false"
factor <- 1
width <- 2048 * factor
height <- 2048 * factor
# ----------------------------------------

# 1) Bounding boxes aproximados en EPSG:3857
pen_bbox <- st_read("data/peninbal.geojson", quiet = TRUE)
canary <-  st_read("data/canary.geojson", quiet = TRUE)
# Obtener bounding box real en EPSG:3857
pen_bbox <- st_bbox(pen_bbox)  # Esto reemplaza mis valores aproximados
canarias_bbox <-st_bbox(canary)  # Esto reemplaza mis valores aproximados

# 2) Función para dividir en cuadrantes
make_grid <- function(bbox, ncol, nrow) {
  st_make_grid(st_as_sfc(bbox), n = c(ncol, nrow)) %>%
    st_sf(geometry = .) %>%
    mutate(id = row_number())
}

pen_grid <- make_grid(pen_bbox, ncol, nrow)
canarias_tile <- make_grid(canarias_bbox, 1, 1)

# 4) Unión de todos los tiles
all_tiles <- bind_rows(
  pen_grid,
  canarias_tile
)

st_write(all_tiles %>% st_transform(4326), 'all_tiles.geojson', delete_dsn = T)

# 5) Procesar cada tile
vect_list <- list()

# for (i in seq_len(nrow(all_tiles))) {
for (i in c(26, 32)) {
  bb <- st_bbox(all_tiles[i, ])
  bbox_str <- paste(bb, collapse = ",")
  
  # Construir URL
  url <- paste0(
    wms_base,
    "&time=", time_param,
    "&width=", width,
    "&height=", height,
    "&srs=", crs_epsg,
    "&bbox=", bbox_str
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
  download.file(url, png_path, mode = "wb", quiet = TRUE)
  # 
  # # Crear world file
  xmin <- bb["xmin"]; ymin <- bb["ymin"]; xmax <- bb["xmax"]; ymax <- bb["ymax"]
  A <- (xmax - xmin) / width
  E <- -(ymax - ymin) / height
  C <- xmin + A/2
  F <- ymax + E/2
  writeLines(sprintf("%.15f", c(A, 0, 0, E, C, F)), con = pgw_path)

  # Leer y procesar
  r <- rast(png_path)[[1]]
  ext(r) <- ext(xmin, xmax, ymin, ymax)
  crs(r) <- crs_epsg

  max_val <- global(r, max, na.rm = TRUE)[1, 1]
  if (is.finite(max_val)) {
    r_bin <- r == max_val
    vect_pol <- as.polygons(r_bin, dissolve = TRUE, values = TRUE, na.rm = TRUE)
    vect_pol <- vect_pol[vect_pol$lyr.1 == 1]

    if (nrow(vect_pol) > 0) {
      vect_sf <- st_as_sf(vect_pol)
      st_write(vect_sf, geojson_path, delete_dsn = TRUE, quiet = TRUE)
      vect_list[[length(vect_list) + 1]] <- vect_sf
    }
  }
}

# 6) Dissolve final
if (length(vect_list) > 0) {
  final <- do.call(rbind, vect_list) %>% st_union() %>% st_sf(geometry = .)
  st_write(final, "fuegos_espania.geojson", delete_dsn = TRUE)
  cat("Capa final creada: fuegos_espania.geojson\n")
} else {
  cat("No se generaron polígonos.\n")
}
