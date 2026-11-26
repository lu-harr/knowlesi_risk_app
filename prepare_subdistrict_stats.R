# =============================================================================
# prepare_subdistrict_stats.R
# Builds the subdistrict (kecamatan, admin-3) layer used for drill-down
# filtering in the app. Run once (after prepare_district_stats.R) whenever the
# surfaces or boundaries change:
#
#     Rscript prepare_subdistrict_stats.R
#
# Boundaries come from malariaAtlas (same source/version family as the
# admin-2 file written by data_setup.R), so subdistricts link to districts by
# the shared `id_2` code. The raw download is cached in data/idn_subdistricts_raw.rds.
#
# Produces:
#   data/idn_subdistricts.rds     simplified sf polygons for display, with
#                                 subdistrict_id, district_id, name_3, labels
#   data/subdistrict_stats.rds    one row per subdistrict (mean risk, ...)
#   data/subdistrict_cells.rds    named list of within-subdistrict cell values
#
# Stats use area-weighted extraction (exact = TRUE) on the full-resolution
# boundaries: many kecamatan are only a few 5 km raster cells across, so plain
# cell-centre extraction would leave lots of them empty.
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

risk_path      <- "data/risk_masked.tif"
districts_path <- "data/idn_districts.shp"
raw_path       <- "data/idn_subdistricts_raw.rds"

stopifnot(file.exists(risk_path), file.exists(districts_path))

risk      <- terra::rast(risk_path)
districts <- sf::st_read(districts_path, quiet = TRUE)
districts <- sf::st_transform(districts, 4326)
districts$district_id <- seq_len(nrow(districts))

# ---- admin-3 boundaries -----------------------------------------------------
if (file.exists(raw_path)) {
  sub <- readRDS(raw_path)
} else {
  message("Downloading Indonesia admin-3 boundaries from malariaAtlas...")
  sub <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin3")
  saveRDS(sub, raw_path)
}
sub <- sf::st_transform(sub, 4326)

# planar ops: some source rings have duplicate vertices that s2 rejects
old_s2 <- sf::sf_use_s2(FALSE)
on.exit(sf::sf_use_s2(old_s2), add = TRUE)
# make_valid can return GEOMETRYCOLLECTIONs (polygon + stray lines/points);
# keep only the polygon parts so every row stays a single MULTIPOLYGON
polygons_only <- function(x) {
  x <- sf::st_make_valid(x)
  g <- sf::st_geometry(x)
  is_gc <- sf::st_geometry_type(g) == "GEOMETRYCOLLECTION"
  if (any(is_gc)) {
    g[is_gc] <- lapply(g[is_gc], function(gc) {
      polys <- Filter(function(p) inherits(p, c("POLYGON", "MULTIPOLYGON")), gc)
      if (length(polys) == 0) sf::st_multipolygon()
      else sf::st_union(sf::st_sfc(polys))[[1]]
    })
  }
  sf::st_geometry(x) <- sf::st_cast(g, "MULTIPOLYGON")
  x
}
sub <- polygons_only(sub)

# link to the app's district row index via the shared admin-2 code
sub$district_id <- match(sub$id_2, districts$id_2)
if (anyNA(sub$district_id)) {
  warning(sum(is.na(sub$district_id)),
          " subdistrict(s) have no matching district and were dropped")
  sub <- sub[!is.na(sub$district_id), ]
}
sub <- sub[order(sub$district_id, sub$name_3), ]
sub$subdistrict_id <- seq_len(nrow(sub))
n <- nrow(sub)

# ---- area-weighted extraction ----------------------------------------------
message("Extracting raster cells for ", n, " subdistricts...")
ex <- terra::extract(risk, terra::vect(sub), exact = TRUE)
ex <- ex[!is.na(ex$prediction_mean) & ex$fraction > 0, ]
by_id <- split(ex, ex$ID)

wmean <- function(x, w) sum(x * w) / sum(w)

subdistrict_cells <- vector("list", n)
stats_rows <- vector("list", n)
for (i in seq_len(n)) {
  e <- by_id[[as.character(i)]]
  m <- if (is.null(e)) numeric(0) else e$prediction_mean
  subdistrict_cells[[i]] <- m
  stats_rows[[i]] <- data.frame(
    subdistrict_id   = i,
    district_id      = sub$district_id[i],
    name_3           = sub$name_3[i],
    name_2           = sub$name_2[i],
    name_1           = sub$name_1[i],
    mean_risk        = if (length(m)) wmean(m, e$fraction) else NA_real_,
    sd_risk          = if (length(m) > 1) stats::sd(m) else NA_real_,
    mean_uncertainty = if (length(m)) wmean(e$prediction_variance, e$fraction) else NA_real_,
    risk_min         = if (length(m)) min(m) else NA_real_,
    risk_max         = if (length(m)) max(m) else NA_real_,
    n_cells          = length(m),
    stringsAsFactors = FALSE
  )
}
subdistrict_stats <- do.call(rbind, stats_rows)

# share of each subdistrict inside the model's host/vector mask (area-weighted)
mask_path <- "data/prediction_masking.tif"
if (file.exists(mask_path)) {
  # denominator = cells with a model prediction; covered only where mask == 1
  mk <- terra::extract(c(risk[["prediction_mean"]], terra::rast(mask_path)),
                       terra::vect(sub), exact = TRUE)
  mk <- mk[!is.na(mk[[2]]) & mk$fraction > 0, ]
  cov <- vapply(split(mk, factor(mk$ID, levels = seq_len(n))), function(e) {
    if (!nrow(e)) return(NA_real_)
    inside <- !is.na(e[[3]]) & e[[3]] == 1
    sum(inside * e$fraction) / sum(e$fraction)
  }, numeric(1))
  subdistrict_stats$model_coverage <- unname(cov)
}
names(subdistrict_cells) <- as.character(seq_len(n))

# ---- light simplification for display only ----------------------------------
# ~100 m tolerance: keeps outlines faithful while cutting file size a lot
disp <- sub[, c("subdistrict_id", "district_id", "name_3", "name_2", "name_1")]
disp <- sf::st_simplify(disp, preserveTopology = TRUE, dTolerance = 0.001)
disp <- polygons_only(disp)
disp$subdistrict_label <- disp$name_3

saveRDS(disp, "data/idn_subdistricts.rds")
saveRDS(subdistrict_stats, "data/subdistrict_stats.rds")
saveRDS(subdistrict_cells, "data/subdistrict_cells.rds")

utils::write.csv(
  subdistrict_stats[, c("name_3", "name_2", "name_1", "mean_risk", "sd_risk",
                        "mean_uncertainty", "risk_min", "risk_max", "n_cells")],
  "data/subdistrict_risk_table.csv", row.names = FALSE
)

message("Done. ", n, " subdistricts; ",
        sum(subdistrict_stats$n_cells == 0), " have no unmasked risk cells.")
