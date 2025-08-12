library(sf)
library(terra)
library(dplyr)

# ============================================================
# Descarga y vectorización EFFIS por tiles (EPSG:3857)
# - Permite elegir la capa: nrt.ba (rápida, NRT) o modis.ba (estable, histórica)
# - Divide la península+Baleares y Canarias en cuadrantes
# - Descarga por tiles para 3 ventanas temporales (last1, last2, last3)
# - Genera PNG + PGW, umbraliza (max-30), vectoriza y guarda GeoJSON por tile
# - Dissolve final por rango y por capa (archivos separados por capa)
# ============================================================


# ---------------- CONFIG ----------------
ncol <- 5 # nº de columnas (Península+Baleares)
nrow <- 5 # nº de filas    (Península+Baleares)
options(timeout = 60) # timeout base; la descarga tiene su propio timeout por intento

crs_epsg <- "EPSG:3857"
dir.create("tiles", showWarnings = FALSE) # por si quieres pruebas rápidas

# Resolución solicitada al WMS. 'factor' permite subir detalle fácilmente.
factor <- 2
width <- 2048 * factor
height <- 2048 * factor

# Archivos de entrada con bbox en EPSG:3857
peninbal_path <- "data/peninbal.geojson" # bbox península + Baleares
canary_path <- "data/canary.geojson" # bbox Canarias
# ----------------------------------------


# ------------------------------------------------------------
# Selección de capa (interactivo y/o por argumentos)
# - Si ejecutas sin args, te preguntará por consola.
# - Si pasas --layer=nrt.ba o --layer=modis.ba, usará ese valor y no preguntará.
# ------------------------------------------------------------
# Esta variable gobierna TODO: URL WMS y nombres de salida.
get_chosen_layer <- function() {
  # 1) Intentar leer de los argumentos de Rscript (automatización)
  args <- commandArgs(trailingOnly = TRUE)
  layer_arg <- sub("^--layer=", "", args[grepl("^--layer=", args)])
  if (length(layer_arg) == 1 && layer_arg %in% c("nrt.ba", "modis.ba")) {
    return(layer_arg)
  }
  # 2) Si no hay argumento válido, preguntar al usuario
  cat("Elige la capa a utilizar:\n",
    "  1) nrt.ba  (Near Real-Time, rápida para 1–3 días)\n",
    "  2) modis.ba (MODIS MCD64, estable para históricos)\n",
    sep = ""
  )
  ans <- readline("Escribe 1 o 2 (por defecto: 1): ")
  if (trimws(ans) %in% c("2", "modis.ba")) "modis.ba" else "nrt.ba"
}

layer_name <- get_chosen_layer()
cat("Capa seleccionada:", layer_name, "\n")

# Servicio WMS base (mismo endpoint, solo cambia el parámetro 'layers=' según elección)
if (layer_name == "nrt.ba") {
  wms_base <- "https://maps.effis.emergency.copernicus.eu/gwis?service=WMS&request=GetMap&layers=nrt.ba&styles=&format=image/png&transparent=true&version=1.1.1&singletile=false"
} else if (layer_name == "modis.ba") {
  wms_base <- "https://maps.effis.emergency.copernicus.eu/effis?service=WMS&request=GetMap&layers=modis.ba&styles=&format=image/png&transparent=true&version=1.1.1&singletile=false"
} else {
  stop("Capa no reconocida. Usa 'nrt.ba' o 'modis.ba'.")
}





# ------------------------------------------------------------
# Utilidad: descarga con reintentos y timeout elevable por intento
# ------------------------------------------------------------
safe_download <- function(url, destfile, tries = 3, timeout_sec = 120, sleep_sec = 2) {
  for (t in seq_len(tries)) {
    cat("  Descarga intento", t, "...\n")
    ok <- try(
      {
        options(timeout = timeout_sec)
        download.file(url, destfile, mode = "wb", quiet = TRUE)
        file.exists(destfile) && file.size(destfile) > 0
      },
      silent = TRUE
    )
    if (!inherits(ok, "try-error") && isTRUE(ok)) {
      return(TRUE)
    }
    Sys.sleep(sleep_sec)
  }
  FALSE
}

# ------------------------------------------------------------
# Utilidad: crear una malla regular (grid) sobre un bbox (sfc)
# 'id_start' fija el primer ID de esa malla para evitar colisiones
# ------------------------------------------------------------
make_grid <- function(bbox, ncol, nrow, id_start = 1) {
  g <- st_make_grid(st_as_sfc(bbox), n = c(ncol, nrow))
  st_sf(geometry = g) %>%
    mutate(id = id_start + dplyr::row_number() - 1)
}


# ---- Tiles (lectura de bboxes) ----
pen <- st_read(peninbal_path, quiet = TRUE)
can <- st_read(canary_path, quiet = TRUE)

pen_bbox <- st_bbox(pen) # península + Baleares
can_bbox <- st_bbox(can) # Canarias

pen_grid <- make_grid(pen_bbox, ncol, nrow, id_start = 1)
can_grid <- make_grid(can_bbox, 1, 1, id_start = max(pen_grid$id) + 1)

all_tiles <- bind_rows(pen_grid, can_grid)

# Export de referencia (en 4326) para revisar rápidamente
# st_write(st_transform(all_tiles, 4326),
#   paste0("all_tiles_", gsub("\\.", "_", layer_name), ".geojson"),
#   delete_dsn = TRUE, quiet = TRUE
# )


# ---- Ventanas temporales dinámicas ----
today <- Sys.Date()
# Esta lista controla los 3 archivos finales. Cada entrada define:
# - el rango 'time' que se pide al WMS
# - la carpeta de salida
ranges <- list(
  list(label = "last1", start = today - 1, end = today)
  # list(label = "last2", start = today - 2, end = today),
  # list(label = "last3", start = today - 3, end = today)
)

fmt <- function(d) format(d, "%Y-%m-%d")


# ============================================================
# Bucle principal por rango temporal
# ============================================================
for (rg in ranges) {
  time_param <- paste0(fmt(rg$start), "/", fmt(rg$end)) # se inyecta en la URL WMS
  # La carpeta de salida incluye la capa para que puedas correr ambos modos sin pisar resultados
  out_dir <- file.path("out", layer_name, paste0(fmt(rg$start), "_to_", fmt(rg$end)))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "tiles"), showWarnings = FALSE)

  cat("\n=== [", layer_name, "] Rango temporal:", time_param, "===\n", sep = "")

  # Aquí iremos metiendo las geometrías válidas de cada tile para luego disolver
  vect_list <- list()

  # -----------------------------------------
  # Bucle por tile
  # -----------------------------------------
  for (i in seq_len(nrow(all_tiles))) {
    bb <- st_bbox(all_tiles[i, ])
    bbox_str <- paste(bb, collapse = ",")

    url <- paste0(
      wms_base,
      "&time=", time_param,
      "&width=", width,
      "&height=", height,
      "&srs=", crs_epsg,
      "&bbox=", bbox_str
    )

    tile_id <- all_tiles$id[i]
    # Los nombres incluyen la capa, por si descargas ambas en paralelo
    png_path <- file.path(out_dir, "tiles", sprintf("tile_%d_%s.png", tile_id, gsub("\\.", "_", layer_name)))
    pgw_path <- sub("\\.png$", ".pgw", png_path)
    geojson_path <- file.path(out_dir, "tiles", sprintf("tile_%d_%s.geojson", tile_id, gsub("\\.", "_", layer_name)))

    cat("Tile", tile_id, "→\n  URL:", url, "\n")

    # ---------- Descarga con reintentos ----------
    ok <- safe_download(url, png_path, tries = 3, timeout_sec = 120)
    if (!ok) {
      warning("  No se pudo descargar tile ", tile_id, ". Se omite.\n")
      next
    }

    # ---------- World file (.pgw) ----------
    xmin <- bb["xmin"]
    ymin <- bb["ymin"]
    xmax <- bb["xmax"]
    ymax <- bb["ymax"]
    A <- (xmax - xmin) / width
    E <- -(ymax - ymin) / height
    C <- xmin + A / 2
    F <- ymax + E / 2
    writeLines(sprintf("%.15f", c(A, 0, 0, E, C, F)), con = pgw_path)

    # ---------- Lectura del raster y blindajes ----------
    stk <- try(rast(png_path), silent = TRUE)
    if (inherits(stk, "try-error")) {
      warning("  No se pudo leer raster del tile ", tile_id, ". Se omite.\n")
      next
    }

    # Si hay canal alfa, comprobamos transparencia total
    if (nlyr(stk) >= 4) {
      alpha_max <- global(stk[[4]], max, na.rm = TRUE)[1, 1]
      if (!is.finite(alpha_max) || alpha_max <= 0) {
        message("  Tile ", tile_id, ": completamente transparente (alfa = 0). Se omite.")
        next
      }
    }

    # Banda 1 y georreferenciación
    r <- stk[[1]]
    ext(r) <- ext(xmin, xmax, ymin, ymax)
    crs(r) <- crs_epsg

    max_val <- global(r, max, na.rm = TRUE)[1, 1]
    if (!is.finite(max_val) || max_val <= 0) {
      message("  Tile ", tile_id, ": sin señal (max <= 0). Se omite.")
      next
    }

    # ---------- Umbral flexible ----------
    # Nos quedamos con todo lo que esté por encima de (max_val - 30).
    r_bin <- r > (max_val - 30)

    # ---------- Vectorización ----------
    vect_pol <- as.polygons(r_bin, dissolve = TRUE, values = TRUE, na.rm = TRUE)

    # Tomamos la primera columna de atributos (sea 'lyr.1' o '<nombre>_1') y filtramos == 1
    val_col <- names(vect_pol)[1]
    vect_pol <- vect_pol[vect_pol[[val_col]] == 1]

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
  if (length(vect_list) > 0) {
    geom_all <- do.call(c, lapply(vect_list, st_geometry)) # concatenamos sfc
    geom_valid <- st_make_valid(geom_all) # arreglamos geometrías problemáticas
    final <- st_sf(geometry = st_union(geom_valid)) %>% # disolvemos
      st_transform(4326) # salida en WGS84

    # El nombre final incluye la capa y el rango; así puedes tener ambos productos a la vez
    final_path <- file.path(out_dir, paste0("spain_fires_", gsub("\\.", "_", layer_name), ".geojson"))
    st_write(final, final_path, delete_dsn = TRUE, quiet = TRUE)
    cat("✔ Capa final creada:", final_path, "\n")
  } else {
    cat("✖ No se generaron polígonos para", time_param, "\n")
  }
}

cat("\nHecho.\n")
