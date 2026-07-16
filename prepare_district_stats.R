# =============================================================================
# prepare_district_stats.R
# Precompute per-district risk statistics + within-district cell values so the
# Shiny app starts instantly. Run once whenever the surfaces or shapefile change:
#
#     Rscript prepare_district_stats.R
#
# Produces:
#   data/district_stats.rds   one row per district (mean risk, uncertainty, ...)
#   data/district_cells.rds   named list of within-district risk cell values
#
# The app will also compute these on the fly if the RDS files are missing, but
# precomputing keeps launch fast (useful for Posit Connect Cloud deployment).
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

risk_path      <- "data/risk_masked.tif"
districts_path <- "data/idn_districts.shp"

stopifnot(file.exists(risk_path), file.exists(districts_path))

risk <- terra::rast(risk_path)
required_layers <- c("prediction_mean", "prediction_variance")
missing_layers <- setdiff(required_layers, names(risk))
if (length(missing_layers) > 0) {
  stop("risk_masked.tif is missing layer(s): ",
       paste(missing_layers, collapse = ", "), call. = FALSE)
}

districts <- sf::st_read(districts_path, quiet = TRUE)
districts <- sf::st_transform(districts, 4326)
n <- nrow(districts)

message("Extracting raster cells for ", n, " districts...")
dv <- terra::vect(districts)
extracted <- terra::extract(risk, dv)   # cols: ID, prediction_mean, prediction_variance

split_mean <- split(extracted$prediction_mean, extracted$ID)
split_var  <- split(extracted$prediction_variance, extracted$ID)

# within-district risk cell values (drop NA), keyed by district row index
district_cells <- lapply(as.character(seq_len(n)), function(k) {
  v <- split_mean[[k]]
  if (is.null(v)) numeric(0) else v[!is.na(v)]
})
names(district_cells) <- as.character(seq_len(n))

# summary table, one row per district
district_stats <- do.call(rbind, lapply(seq_len(n), function(i) {
  key <- as.character(i)
  m <- district_cells[[key]]
  v <- split_var[[key]]; if (is.null(v)) v <- numeric(0); v <- v[!is.na(v)]
  data.frame(
    district_id      = i,
    name_2           = districts$name_2[i],
    name_1           = districts$name_1[i],
    mean_risk        = if (length(m))     mean(m)          else NA_real_,
    sd_risk          = if (length(m) > 1) stats::sd(m)     else NA_real_,
    mean_uncertainty = if (length(v))     mean(v)          else NA_real_,
    risk_min         = if (length(m))     min(m)           else NA_real_,
    risk_max         = if (length(m))     max(m)           else NA_real_,
    n_cells          = length(m),
    stringsAsFactors = FALSE
  )
}))

saveRDS(district_stats, "data/district_stats.rds")
saveRDS(district_cells, "data/district_cells.rds")

# handy flat export for colleagues who just want the numbers
utils::write.csv(
  district_stats[, c("name_2", "name_1", "mean_risk", "sd_risk",
                     "mean_uncertainty", "risk_min", "risk_max", "n_cells")],
  "data/district_risk_table.csv", row.names = FALSE
)