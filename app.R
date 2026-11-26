# =============================================================================
# Plasmodium knowlesi transmission-suitability risk app  
#
# Enhancements in this version
#   (a) Plain-language lay summary shown as a pop-up on entry
#   (b) Switchable basemaps (clean / street / satellite) so landmarks are visible
#   (c) Overall predicted transmission suitability + uncertainty per district, shown as values
#   (d) Within-district risk distribution shown as a plot
#   +   English <-> Bahasa Indonesia toggle throughout
#   +   District search box, CSV/PNG export, methods & citation panel,
#       responsive (mobile-friendly) layout
#   +   Subdistrict (kecamatan) drill-down, optional subdistrict boundaries
#   +   Risk-list categories for the draft Indonesian Pk surveillance SOP
#       (evidence first; map adds receptive areas), "outside model" layer,
#       risk-list export (SOP annex)
#   +   Pk incidence, Kemenkes API and malaria elimination status layers
# =============================================================================

# Labels-on-top now uses Esri's key-free reference layer for every basemap
# (CARTO tiles need an API key), which also fixes the street-map labels bug.

library(shiny)
library(leaflet)
library(terra)
library(sf)

# small null-coalescing helper
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------
risk_path      <- "data/risk_masked.tif"
districts_path <- "data/idn_districts.shp"

risk <- terra::rast(risk_path)
required_layers <- c("prediction_mean", "prediction_variance")
missing_layers <- setdiff(required_layers, names(risk))
if (length(missing_layers) > 0) {
  stop(
    "risk_masked.tif is missing required layer(s): ",
    paste(missing_layers, collapse = ", "),
    call. = FALSE
  )
}

districts <- sf::st_read(districts_path, quiet = TRUE)
districts <- sf::st_transform(districts, 4326)
# Stable per-district id used to link map <-> search <-> stats
districts$district_id <- seq_len(nrow(districts))
districts$district_label <- paste(districts$name_2, districts$name_1, sep = ", ")
district_bbox <- sf::st_bbox(districts)

# Per-district bounding boxes (for "fly to" on search / click)
district_bounds <- lapply(seq_len(nrow(districts)), function(i) {
  as.numeric(sf::st_bbox(districts[i, ]))
})

# Lighter geometry for drawing only (~200 m tolerance). The full boundaries
# are ~30 MB as GeoJSON and were re-sent on every layer switch; this copy is
# ~2.4 MB and looks the same at map zoom levels. Stats use the full shapes.
districts_map <- local({
  old_s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old_s2))
  x <- suppressWarnings(sf::st_simplify(districts, preserveTopology = TRUE, dTolerance = 0.002))
  x[!sf::st_is_empty(x), ]
})

surface_layers <- list(
  mean        = risk[["prediction_mean"]],
  uncertainty = risk[["prediction_variance"]]
)

surface_domains <- lapply(surface_layers, function(x) {
  domain <- as.numeric(terra::global(x, range, na.rm = TRUE)[1, ])
  names(domain) <- c("min", "max")
  domain
})

# Project once at startup so switching layers stays smooth
surface_layers <- lapply(
  surface_layers,
  leaflet::projectRasterForLeaflet,
  method = "bilinear"
)

map_palette <- viridisLite::viridis(256)

# -----------------------------------------------------------------------------
# 2. Per-district zonal statistics + within-district cell values
#    Prefer precomputed RDS (see prepare_district_stats.R). If absent, compute
#    on first launch and cache to disk.
# -----------------------------------------------------------------------------
stats_rds <- "data/district_stats.rds"
cells_rds <- "data/district_cells.rds"

if (file.exists(stats_rds) && file.exists(cells_rds)) {
  district_stats <- readRDS(stats_rds)
  district_cells <- readRDS(cells_rds)
} else {
  message("Computing per-district statistics (first run may take a minute)...")
  dv <- terra::vect(districts)
  extracted <- terra::extract(risk, dv)          # cols: ID, prediction_mean, prediction_variance
  split_mean <- split(extracted$prediction_mean, extracted$ID)
  split_var  <- split(extracted$prediction_variance, extracted$ID)

  district_cells <- lapply(split_mean, function(v) v[!is.na(v)])

  district_stats <- do.call(rbind, lapply(seq_len(nrow(districts)), function(i) {
    key <- as.character(i)
    m <- district_cells[[key]]
    v <- split_var[[key]]; v <- v[!is.na(v)]
    data.frame(
      district_id      = i,
      name_2           = districts$name_2[i],
      name_1           = districts$name_1[i],
      mean_risk        = if (length(m)) mean(m) else NA_real_,
      sd_risk          = if (length(m) > 1) stats::sd(m) else NA_real_,
      mean_uncertainty = if (length(v)) mean(v) else NA_real_,
      risk_min         = if (length(m)) min(m) else NA_real_,
      risk_max         = if (length(m)) max(m) else NA_real_,
      n_cells          = length(m),
      stringsAsFactors = FALSE
    )
  }))
  names(district_cells) <- as.character(seq_len(nrow(districts)))
  try({
    saveRDS(district_stats, stats_rds)
    saveRDS(district_cells, cells_rds)
  }, silent = TRUE)
}

# -----------------------------------------------------------------------------
# 2b. Subdistrict (kecamatan, admin-3) layer for drill-down filtering
#     Built by prepare_subdistrict_stats.R. If the files are absent the app
#     still runs; the subdistrict controls are simply hidden.
# -----------------------------------------------------------------------------
sub_geom_rds  <- "data/idn_subdistricts.rds"
sub_stats_rds <- "data/subdistrict_stats.rds"
sub_cells_rds <- "data/subdistrict_cells.rds"

has_subdistricts <- all(file.exists(c(sub_geom_rds, sub_stats_rds, sub_cells_rds)))
if (has_subdistricts) {
  subdistricts      <- readRDS(sub_geom_rds)
  subdistrict_stats <- readRDS(sub_stats_rds)
  subdistrict_cells <- readRDS(sub_cells_rds)
  # row indices of each district's subdistricts (subdistrict_id == row number)
  subdistricts_by_district <- split(seq_len(nrow(subdistricts)),
                                    factor(subdistricts$district_id,
                                           levels = districts$district_id))
} else {
  message("Subdistrict files not found; run prepare_subdistrict_stats.R ",
          "to enable subdistrict filtering.")
}

# -----------------------------------------------------------------------------
# 2c. Risk-list classification (for the SOP "Daftar risiko per wilayah")
#
#  Evidence comes first, the map only adds areas:
#    Category 1  confirmed transmission   (from data/district_evidence.csv)
#    Category 2  probable, not confirmed  (from data/district_evidence.csv)
#    Category 3  receptive: map tier "high" (rule below)
#    Watch list  map tier "moderate"
#    Outside model: no model coverage -> judge on local evidence, NOT "low"
#
#  Map tier uses the SHARE OF AREA above a suitability value rather than the
#  district mean, because exposure happens in forest-edge pockets that a mean
#  dilutes. Edit these values to change the rule; everything downstream
#  (map, popups, export, calibration note) follows.
# -----------------------------------------------------------------------------
risk_rule <- list(
  high       = 0.5,   # suitability value for "high"
  moderate   = 0.3,   # suitability value for "moderate"
  area_share = 0.25,  # share of the area that must reach the value
  rule_label = "share of area >= value: high 0.5, moderate 0.3, 25% of area"
)

map_tier <- function(cells, coverage) {
  if (is.na(coverage) || coverage == 0 || length(cells) == 0) return("outside")
  if (mean(cells >= risk_rule$high) >= risk_rule$area_share) return("high")
  if (mean(cells >= risk_rule$moderate) >= risk_rule$area_share) return("moderate")
  "low"
}

coverage_of <- function(stats) {
  if ("model_coverage" %in% names(stats)) stats$model_coverage
  else rep(1, nrow(stats))   # older RDS without coverage: assume covered
}

district_stats$model_coverage <- coverage_of(district_stats)
district_stats$tier <- vapply(seq_len(nrow(district_stats)), function(i)
  map_tier(district_cells[[as.character(i)]], district_stats$model_coverage[i]),
  character(1))
district_stats$share_high <- vapply(district_cells, function(x)
  if (length(x)) mean(x >= risk_rule$high) else NA_real_, numeric(1))[
    as.character(district_stats$district_id)]
district_stats$share_moderate <- vapply(district_cells, function(x)
  if (length(x)) mean(x >= risk_rule$moderate) else NA_real_, numeric(1))[
    as.character(district_stats$district_id)]

if (has_subdistricts) {
  subdistrict_stats$model_coverage <- coverage_of(subdistrict_stats)
  subdistrict_stats$tier <- vapply(seq_len(nrow(subdistrict_stats)), function(i)
    map_tier(subdistrict_cells[[as.character(i)]], subdistrict_stats$model_coverage[i]),
    character(1))
}

# ---- evidence (case data + API) per district ---------------------------------
# One row per district, matched on name_2 + name_1 (malariaAtlas names).
# Definitions follow the Indonesian MoH (Kemenkes) / SISMAL conventions:
#   evidence_category  1 = confirmed transmission, 2 = probable only, blank
#                      (draft Pk surveillance SOP, Table 4)
#   pk_probable, pk_pcr_tested, pk_confirmed   case counts (period in source)
#   pk_api             Pk incidence ("insidens malaria knowlesi") per 1,000
#                      population per year, MEDIAN of the last 3 years, as in
#                      the SOP risk stratification (>= 1 = Risiko Tinggi)
#   pk_api_years       e.g. "2023-2025"
#   api                Annual Parasite Incidence as reported by Kemenkes/SISMAL:
#                      confirmed malaria cases (microscopy/RDT), ALL species,
#                      per 1,000 population in that year. Use the published
#                      figure as-is; do not remove Pk.
#   api_year           e.g. 2025
#   evidence_source, notes
# Older column names api / api_year are read as api / api_year.
evidence_path <- "data/district_evidence.csv"
ev_cols <- c("evidence_category", "pk_probable", "pk_pcr_tested", "pk_confirmed",
             "pk_api", "pk_api_years", "api", "api_year",
             "evidence_source", "notes")
for (col in ev_cols) district_stats[[col]] <- NA
if (file.exists(evidence_path)) {
  ev <- utils::read.csv(evidence_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"api" %in% names(ev) && "api" %in% names(ev)) ev$api <- ev$api
  if (!"api_year" %in% names(ev) && "api_year" %in% names(ev)) ev$api_year <- ev$api_year
  key_d <- paste(district_stats$name_2, district_stats$name_1, sep = "|")
  key_e <- paste(trimws(ev$name_2), trimws(ev$name_1), sep = "|")
  unmatched <- ev$name_2[!key_e %in% key_d]
  if (length(unmatched))
    warning("district_evidence.csv rows not matched to a district: ",
            paste(unmatched, collapse = ", "), call. = FALSE)
  m <- match(key_d, key_e)
  for (col in intersect(ev_cols, names(ev))) district_stats[[col]] <- ev[[col]][m]
}
district_stats$evidence_category <- suppressWarnings(as.integer(district_stats$evidence_category))
district_stats$pk_api <- suppressWarnings(as.numeric(district_stats$pk_api))
district_stats$api    <- suppressWarnings(as.numeric(district_stats$api))

# SOP annual stratification: median Pk incidence >= 1 per 1,000/yr = high
sop_pk_threshold <- 1
district_stats$sop_stratum <- ifelse(is.na(district_stats$pk_api), NA_character_,
  ifelse(district_stats$pk_api >= sop_pk_threshold, "high", "low"))

# Binned colour scales ("no data" = grey). Bins are [lower, upper); the first
# bin holds exactly 0 (no cases). Labels come from i18n (bands_pk_api /
# bands_api) so they follow the language toggle.
api_scales <- list(
  # SOP: Risiko Tinggi >= 1, Risiko Rendah < 1 (split further for display)
  pk_api = list(
    bins   = c(0, 1e-9, 0.1, sop_pk_threshold, Inf),
    colors = c("#fdf0e8", "#f6a77c", "#e0533b", "#8c1d18")
  ),
  # Kemenkes endemicity strata: endemis tinggi API >= 5, sedang 1-<5,
  # rendah >0-<1; API 0 = no cases reported
  api = list(
    bins   = c(0, 1e-9, 1, 5, Inf),
    colors = c("#eef3f8", "#9ebcda", "#6a8ec8", "#2c3e8c")
  )
)
api_nodata_color <- "#d4d4d8"
api_fill_colors <- function(var) {
  sc <- api_scales[[var]]
  x <- district_stats[[var]][match(districts_map$district_id, district_stats$district_id)]
  k <- cut(x, breaks = sc$bins, right = FALSE, include.lowest = TRUE, labels = FALSE)
  ifelse(is.na(k), api_nodata_color, sc$colors[k])
}

# ---- malaria elimination status (human malaria, Kemenkes certification) -----
# data/elimination_status.csv: name_2, name_1, elimination_status
# ("eliminated" / "not_eliminated"), elimination_year, elimination_source.
# Seeded from public Kemenkes sources only (province certificates, named
# district certificates, KMK 1988/2024); districts not listed show "not
# supplied". Replace with the full Kemenkes list when available.
elim_path <- "data/elimination_status.csv"
district_stats$elimination_status <- NA_character_
district_stats$elimination_year   <- NA_character_
district_stats$elimination_source <- NA_character_
if (file.exists(elim_path)) {
  el <- utils::read.csv(elim_path, stringsAsFactors = FALSE, colClasses = "character")
  m <- match(paste(district_stats$name_2, district_stats$name_1, sep = "|"),
             paste(trimws(el$name_2), trimws(el$name_1), sep = "|"))
  district_stats$elimination_status <- el$elimination_status[m]
  district_stats$elimination_year   <- el$elimination_year[m]
  district_stats$elimination_source <- el$elimination_source[m]
}
elim_colors <- c(eliminated = "#16a34a", not_eliminated = "#c2410c")
n_elim_listed <- sum(district_stats$elimination_status %in% "eliminated")

# ---- published research records of P. knowlesi -------------------------------
# data/published_pk_records.csv: one row per study x location (see
# data/published_pk_records_README.md for columns). Records are matched to a
# kecamatan / district by coordinates, else kecamatan name, else district name.
# A human, molecularly confirmed (PCR etc.), non-imported record with >= 1
# positive counts as research-confirmed transmission:
#   * its district becomes evidence category 1 (if not already), and
#   * if located to village/kecamatan/point, its kecamatan is "confirmed here".
# MoH evidence = district_evidence.csv rows sourced from SISMAL / the SOP.
norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("^(kabupaten|kab\\.?|kota|city of|regency of)\\s+", "", x)
  x <- gsub("\\s+(regency|district|city|municipality)$", "", x)
  gsub("[^a-z]", "", x)
}
prov_alias <- c(acehdarussalam = "aceh", nanggroeacehdarussalam = "aceh",
  northsumatra = "sumaterautara", northsumatera = "sumaterautara",
  westsumatra = "sumaterabarat", westsumatera = "sumaterabarat",
  southsumatra = "sumateraselatan", southsumatera = "sumateraselatan",
  westkalimantan = "kalimantanbarat", centralkalimantan = "kalimantantengah",
  southkalimantan = "kalimantanselatan", eastkalimantan = "kalimantantimur",
  northkalimantan = "kalimantanutara", westjava = "jawabarat", centraljava = "jawatengah",
  eastjava = "jawatimur", northsulawesi = "sulawesiutara", riauislands = "kepulauanriau",
  jakarta = "dkijakarta", yogyakarta = "daerahistimewayogyakarta")
norm_prov <- function(x) { n <- norm_name(x); ifelse(n %in% names(prov_alias), prov_alias[n], n) }

pub_path <- "data/published_pk_records.csv"
pub <- NULL
if (file.exists(pub_path) && has_subdistricts) {
  pub <- utils::read.csv(pub_path, stringsAsFactors = FALSE, check.names = FALSE,
                         na.strings = c("", "NA"))
  pub$lat <- suppressWarnings(as.numeric(pub$lat))
  pub$lon <- suppressWarnings(as.numeric(pub$lon))
  # first number in e.g. "114-148" or "3 (2 single, 1 mixed)"
  pub$n_pos <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", pub$n_pk_positive)))
  pub$molecular <- grepl("PCR|molecular|LAMP|sequenc", pub$diagnostic, ignore.case = TRUE)
  pub$confirmed <- pub$host %in% "human" & pub$molecular & !is.na(pub$n_pos) &
    pub$n_pos > 0 & !(pub$case_origin %in% "imported")
  pub$district_id <- NA_integer_
  pub$subdistrict_id <- NA_integer_
  pub$matched_by <- NA_character_

  d_key <- paste(norm_name(district_stats$name_2), norm_prov(district_stats$name_1))
  s_key <- paste(norm_name(subdistrict_stats$name_3), subdistrict_stats$district_id)
  fine <- pub$location_precision %in% c("point", "village", "kecamatan")

  # 1. coordinates (point/village precision) -> kecamatan containing it, or the
  #    nearest one within ~5 km (coastal points can fall just off simplified edges)
  xy <- which(!is.na(pub$lat) & !is.na(pub$lon) & pub$location_precision %in% c("point", "village"))
  if (length(xy)) {
    old_s2 <- sf::sf_use_s2(FALSE)
    pts <- sf::st_as_sf(pub[xy, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
    hit <- suppressMessages(sf::st_intersects(pts, subdistricts))
    for (k in seq_along(xy)) {
      sid <- if (length(hit[[k]])) hit[[k]][1] else {
        nn <- suppressMessages(sf::st_nearest_feature(pts[k, ], subdistricts))
        if (as.numeric(suppressMessages(sf::st_distance(pts[k, ], subdistricts[nn, ]))) < 0.05) nn else NA
      }
      if (!is.na(sid)) {
        pub$subdistrict_id[xy[k]] <- subdistricts$subdistrict_id[sid]
        pub$district_id[xy[k]] <- subdistricts$district_id[sid]
        pub$matched_by[xy[k]] <- "coordinates"
      }
    }
    sf::sf_use_s2(old_s2)
  }
  # 2. district by name (+ province)
  todo <- is.na(pub$district_id) & !is.na(pub$district)
  pub$district_id[todo] <- district_stats$district_id[
    match(paste(norm_name(pub$district[todo]), norm_prov(pub$province[todo])), d_key)]
  # 3. kecamatan by name within that district
  todo <- is.na(pub$subdistrict_id) & !is.na(pub$district_id) & !is.na(pub$kecamatan)
  pub$subdistrict_id[todo] <- subdistrict_stats$subdistrict_id[
    match(paste(norm_name(pub$kecamatan[todo]), pub$district_id[todo]), s_key)]
  pub$matched_by[is.na(pub$matched_by) & !is.na(pub$subdistrict_id)] <- "kecamatan name"
  pub$matched_by[is.na(pub$matched_by) & !is.na(pub$district_id)] <- "district name"
  pub$matched_by[is.na(pub$matched_by)] <- "province only / unmatched"
  # a coordinate match only counts at kecamatan level if the record is that precise
  pub$confirmed_here <- pub$confirmed & !is.na(pub$subdistrict_id) & fine

  un <- pub$confirmed & is.na(pub$district_id)
  if (any(un)) message(sum(un), " confirmed published record(s) are province-level or unmatched; ",
                       "shown in the records table only.")
}

# evidence basis per district: MoH (SISMAL/SOP) vs published research
ev_src <- ifelse(is.na(district_stats$evidence_source), "", district_stats$evidence_source)
moh_conf  <- district_stats$evidence_category %in% 1 & !grepl("^Research", ev_src)
moh_prob  <- district_stats$evidence_category %in% 2
res_sop   <- district_stats$evidence_category %in% 1 & grepl("^Research", ev_src)
res_pub   <- if (is.null(pub)) rep(FALSE, nrow(district_stats)) else
  district_stats$district_id %in% pub$district_id[pub$confirmed]
district_stats$research_confirmed <- res_sop | res_pub
district_stats$research_sources <- if (is.null(pub)) NA_character_ else
  vapply(district_stats$district_id, function(id) {
    s <- unique(pub$source_short[pub$confirmed & pub$district_id %in% id])
    if (length(s)) paste(sort(s), collapse = "; ") else NA_character_
  }, character(1))
district_stats$evidence_basis <- ifelse(moh_conf & district_stats$research_confirmed, "both",
  ifelse(moh_conf, "moh", ifelse(district_stats$research_confirmed, "research",
  ifelse(moh_prob, "moh_probable", NA_character_))))
district_stats$evidence_category[district_stats$research_confirmed] <- 1L

basis_colors <- c(both = "#7f1d1d", moh = "#dc2626", research = "#2563eb",
                  moh_probable = "#f59e0b")

risk_status <- function(evidence_category, tier) {
  ifelse(!is.na(evidence_category) & evidence_category == 1, "cat1",
  ifelse(!is.na(evidence_category) & evidence_category == 2, "cat2",
  ifelse(tier == "high", "cat3",
  ifelse(tier == "moderate", "watch",
  ifelse(tier == "outside", "outside", "none")))))
}
district_stats$status <- risk_status(district_stats$evidence_category, district_stats$tier)
district_stats$on_risk_list <- district_stats$status %in% c("cat1", "cat2", "cat3")

# ---- subdistrict (kecamatan) evidence + risk list category ------------------
# Optional data/subdistrict_evidence.csv: same columns as the district file
# plus name_3 (kecamatan), matched on name_3 + name_2 + name_1. SISMAL can
# report cases and API by puskesmas/kecamatan; enter them here when available.
# Where a kecamatan has no value of its own, the district's evidence category
# (1/2), Pk incidence and API are shown instead and flagged "from district".
# The map tier is always the kecamatan's own.
if (has_subdistricts) {
  sub_ev_path <- "data/subdistrict_evidence.csv"
  for (col in ev_cols) subdistrict_stats[[col]] <- NA
  if (file.exists(sub_ev_path)) {
    sev <- utils::read.csv(sub_ev_path, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(sev)) {
      key_s <- paste(subdistrict_stats$name_3, subdistrict_stats$name_2,
                     subdistrict_stats$name_1, sep = "|")
      key_v <- paste(trimws(sev$name_3), trimws(sev$name_2), trimws(sev$name_1), sep = "|")
      unmatched <- sev$name_3[!key_v %in% key_s]
      if (length(unmatched))
        warning("subdistrict_evidence.csv rows not matched: ",
                paste(unmatched, collapse = ", "), call. = FALSE)
      m <- match(key_s, key_v)
      for (col in intersect(ev_cols, names(sev))) subdistrict_stats[[col]] <- sev[[col]][m]
    }
  }
  subdistrict_stats$evidence_category <- suppressWarnings(as.integer(subdistrict_stats$evidence_category))
  # kecamatan with their own published, PCR-confirmed human cases
  subdistrict_stats$confirmed_here <- if (is.null(pub)) FALSE else
    subdistrict_stats$subdistrict_id %in% pub$subdistrict_id[pub$confirmed_here]
  subdistrict_stats$evidence_category[subdistrict_stats$confirmed_here] <- 1L
  subdistrict_stats$pk_api <- suppressWarnings(as.numeric(subdistrict_stats$pk_api))
  subdistrict_stats$api    <- suppressWarnings(as.numeric(subdistrict_stats$api))

  d_row <- match(subdistrict_stats$district_id, district_stats$district_id)
  inherit <- function(col, years_col) {
    from <- is.na(subdistrict_stats[[col]]) & !is.na(district_stats[[col]][d_row])
    subdistrict_stats[[col]][from] <<- district_stats[[col]][d_row][from]
    if (!is.null(years_col))
      subdistrict_stats[[years_col]][from] <<- district_stats[[years_col]][d_row][from]
    from
  }
  subdistrict_stats$category_from_district <- inherit("evidence_category", NULL)
  subdistrict_stats$pk_api_from_district   <- inherit("pk_api", "pk_api_years")
  subdistrict_stats$api_from_district      <- inherit("api", "api_year")
  subdistrict_stats$status <- risk_status(subdistrict_stats$evidence_category,
                                          subdistrict_stats$tier)
  subdistrict_stats$sop_stratum <- ifelse(is.na(subdistrict_stats$pk_api), NA_character_,
    ifelse(subdistrict_stats$pk_api >= sop_pk_threshold, "high", "low"))
}

status_colors <- c(cat1 = "#991b1b", cat2 = "#ea580c", cat3 = "#7c3aed",
                   watch = "#facc15", outside = "#94a3b8", none = "#f1f5f9")

# How well does the map rule recover districts with confirmed transmission?
cat1_rows <- which(district_stats$evidence_category %in% 1)
calibration <- table(factor(district_stats$tier[cat1_rows],
                            levels = c("high", "moderate", "low", "outside")))

# ---- "outside model" overlay for the suitability views -----------------------
mask_path <- "data/prediction_masking.tif"
outside_layer <- NULL
if (file.exists(mask_path)) {
  mk <- terra::rast(mask_path)
  outside_layer <- terra::ifel(!is.na(risk[["prediction_mean"]]) &
                                 (is.na(mk) | mk != 1), 1, NA)
  outside_layer <- leaflet::projectRasterForLeaflet(outside_layer, method = "ngb")
}

# -----------------------------------------------------------------------------
# 3. Bilingual dictionary (English / Bahasa Indonesia)
# -----------------------------------------------------------------------------
i18n <- list(
  en = list(
    title_html      = HTML("<em>Plasmodium knowlesi</em> transmission suitability"),
    subtitle        = HTML("Tobin et al., 2024, PLOS Negl. Trop. Dis. &middot; <a href='https://pubmed.ncbi.nlm.nih.gov/38252650/' target='_blank' rel='noopener'>PMID: 38252650</a>"),
    language         = "Language",
    display          = "Display",
    layer_mean       = "Mean predicted transmission suitability",
    layer_uncertainty= "Prediction uncertainty (variance)",
    basemap          = "Base map",
    basemap_clean    = "Light grey (minimal labels)",
    show_risk        = "Show transmission suitability layer",
    show_sub_all     = "Show subdistrict boundaries (zoom in)",
    search_all       = "\u2014 All of Indonesia (no filter) \u2014",
    reset_district   = "Reset: show all of Indonesia",
    basemap_street   = "Street map (landmarks)",
    basemap_sat      = "Satellite",
    opacity          = "Transmission suitability layer opacity",
    search           = "Find a district",
    search_ph        = "Type a district or regency name...",
    district_panel   = "Selected district",
    no_district      = "Click a district on the map, or search above, to see its predicted transmission suitability.",
    mean_risk_lab    = "Mean predicted transmission suitability",
    uncertainty_lab  = "Mean uncertainty (variance)",
    range_lab        = "Range across district",
    dist_title       = "Transmission suitability distribution within district",
    dist_x           = "Predicted transmission suitability",
    dist_y           = "Grid cells",
    dist_meanline    = "district mean",
    export           = "Download data",
    export_csv       = HTML("District transmission suitability<br>table (CSV)"),
    export_sub_csv   = HTML("Subdistrict transmission suitability<br>table (CSV)"),
    sub_search       = "Filter to a subdistrict",
    sub_search_ph    = "\u2014 Whole district (all subdistricts) \u2014",
    sub_panel        = "Selected subdistrict",
    sub_hint         = "Subdistrict outlines are shown on the map; click one or pick it above.",
    sub_back         = "Back to whole district",
    sub_dist_title   = "Transmission suitability distribution within subdistrict",
    sub_meanline     = "subdistrict mean",
    sub_no_cells     = "No unmasked transmission suitability cells fall in this subdistrict.",
    range_sub_lab    = "Range across subdistrict",
    layer_category   = "Risk list category (SOP)",
    legend_category  = "Risk list category",
    st_cat1          = "Cat. 1: confirmed transmission",
    st_cat2          = "Cat. 2: probable, not yet confirmed",
    st_cat3          = "Cat. 3: receptive (map: high)",
    st_watch         = "Watch list (map: moderate)",
    st_outside       = "Outside model: use local evidence",
    st_none          = "Not listed (map: low)",
    tier_lab         = "Map tier",
    tier_high        = "high", tier_moderate = "moderate", tier_low = "low",
    tier_outside     = "outside model",
    status_lab       = "Risk list category",
    coverage_lab     = "Inside model coverage",
    share_lab        = "Area with suitability ≥ %s",
    api_missing      = "not yet supplied",
    pk_api_lab       = "Pk incidence (per 1,000/yr, 3-yr median)",
    api_lab          = "API, all species (Kemenkes, per 1,000/yr)",
    layer_pk_api     = "Pk incidence (SOP stratification)",
    layer_api        = "Malaria API, all species (Kemenkes)",
    legend_pk_api    = "Pk incidence per 1,000/yr (3-yr median, SOP)",
    legend_api       = "Malaria API per 1,000/yr (Kemenkes endemicity)",
    bands_pk_api     = c("0 (no cases)", ">0 \u2013 <0.1", "0.1 \u2013 <1 (SOP: low risk)", "\u2265 1 (SOP: high risk)"),
    bands_api        = c("0 (no cases)", ">0 \u2013 <1 (low endemic)", "1 \u2013 <5 (moderate endemic)", "\u2265 5 (high endemic)"),
    api_nodata       = "No data supplied",
    from_district    = "from district",
    confirmed_here   = "confirmed here: published research",
    layer_evidence   = "Pk evidence: MoH vs research",
    evid_lab         = "Pk evidence basis",
    evid_legend      = "Confirmed P. knowlesi transmission",
    evid_both        = "MoH (SISMAL) + research",
    evid_moh         = "MoH (SISMAL) confirmed",
    evid_research    = "Research PCR-confirmed only",
    evid_moh_prob    = "MoH probable only",
    evid_none        = "No confirmed record",
    evid_site        = "Research case site (PCR)",
    evid_site_neg    = "Tested, no Pk / non-human",
    evid_sources     = "Research sources",
    layer_elim       = "Malaria elimination status (Kemenkes)",
    elim_lab         = "Malaria elimination",
    elim_eliminated  = "Eliminated (certified)",
    elim_not         = "Not yet eliminated",
    elim_nodata      = "Not supplied",
    elim_legend      = "Malaria elimination status",
    elim_partial     = "Partial list from public Kemenkes sources (%d eliminated districts listed); full list pending.",
    elim_pk_note     = "Certified malaria-free, but P. knowlesi is not counted in elimination. Confirmed or receptive Pk area: send every malaria-positive for PCR.",
    defs_html        = HTML(paste0(
      "<p><strong>Case data definitions (Kemenkes).</strong> <em>API</em> = Annual ",
      "Parasite Incidence as reported in SISMAL: confirmed malaria cases (microscopy ",
      "or RDT), all species, per 1,000 population per year; strata: high endemic ",
      "&ge; 5, moderate 1&ndash;&lt;5, low &gt;0&ndash;&lt;1. <em>Pk incidence</em> = ",
      "median annual <em>P. knowlesi</em> cases per 1,000 population over the last ",
      "3 years, as used for the draft Pk surveillance SOP stratification (&ge; 1 = ",
      "high risk).</p>")),
    sop_stratum_lab  = "SOP stratum (Pk \u2265 1/1,000)",
    stratum_high     = "high", stratum_low = "low",
    pk_lab           = "Pk probable / PCR / confirmed",
    outside_legend   = "Outside model (no prediction)",
    outside_note     = "This area is outside the model's extent: predictions were restricted to areas within the range of macaque and mosquito species known to be required for zoonotic transmission of P. knowlesi (the overlap in range maps of at least one reservoir and one vector species), so the map gives no information here. A value of 0 does NOT mean low risk; use case data and local evidence.",
    export_list      = "Risk list by area (SOP annex, CSV)",
    calib_title      = "How the map rule matches known cases",
    calib_text       = "Of %d districts with confirmed transmission, the map rule rates %d high, %d moderate, %d low and %d outside the model. Evidence categories therefore always override the map.",
    export_png       = "Save current map (PNG)",
    methods          = "Methods, data & citation",
    legend_mean      = "Mean predicted transmission suitability",
    legend_uncert    = "Prediction variance",
    modal_title      = "About this map",
    modal_close      = "Explore the map",
    modal_dont       = "Don't show this again",
    reopen_summary   = "About this map",
    lay_summary      = HTML(paste0(
      "<p>To predict spatial variation in <em>P. knowlesi</em> relative transmission risk, we ",
      "built a <strong>boosted regression tree</strong> model to learn associations ",
      "between remotely-sensed environmental data, including <strong>elevation, land ",
      "cover</strong>, and annually-varying <strong>forest disturbance, temperature",
      "</strong> and <strong>wetness</strong> indices, and locations where ",
      "<em>P. knowlesi</em> malaria was identified.</p>",
      "<p>Our predictions are therefore of <strong>environmental suitability for ",
      "transmission</strong> of <em>P. knowlesi</em> malaria. ",
      "The model predictions were restricted to areas within the range of macaque and mosquito species known to be required for zoonotic transmission of <em>P. knowlesi</em> (i.e. the overlap in range maps of at least one reservoir and one vector species). ",
      "The uncertainty surface describes <strong>variance in bootstrapped ",
      "predictions</strong> from the model: we fitted the model to different ",
      "samples of the <em>P. knowlesi</em> malaria infection dataset to evaluate ",
      "model sensitivity to environmentally-outlying infections.</p>",
      "<p>The model surfaces have spatial resolution of &asymp; 5 x 5 km<sup>2</sup>.</p>",
      "<p>Read more about the model in Tobin et al., 2024: ",
      "<a href='https://doi.org/10.1371/journal.pntd.0011570' target='_blank' ",
      "rel='noopener'>doi:10.1371/journal.pntd.0011570</a></p>"
    )),
    methods_html = HTML(paste0(
      "<p><strong>Model.</strong> An environmental niche model (extending Shearer ",
      "et al. 2016) fitted to 524 <em>P. knowlesi</em> occurrence records collected ",
      "up to March 2020, with covariates added to capture deforestation and ",
      "urbanisation.</p>",
      "<p><strong>Layers.</strong> <em>Mean</em> = mean predicted relative ",
      "transmission suitability. <em>Uncertainty</em> = variance of the prediction ",
      "(higher = less certain).</p>",
      "<p><strong>District values.</strong> Per-district figures are the average of ",
      "all grid cells falling within each administrative-2 boundary.</p>",
      "<p><strong>Citation.</strong> Tobin RJ, et al. Updating estimates of ",
      "<em>Plasmodium knowlesi</em> malaria risk in response to changing land use ",
      "patterns across Southeast Asia. PLOS Negl Trop Dis. 2024. ",
      "<a href='https://doi.org/10.1371/journal.pntd.0011570' target='_blank'>",
      "doi:10.1371/journal.pntd.0011570</a> &middot; ",
      "<a href='https://pubmed.ncbi.nlm.nih.gov/38252650/' target='_blank'>",
      "PMID: 38252650</a></p>",
      "<p><strong>Surfaces:</strong> ",
      "<a href='https://osf.io/k5bsa/overview' target='_blank'>osf.io/k5bsa</a></p>"
    ))
  ),
  id = list(
    title_html      = HTML("Kecocokan penularan <em>Plasmodium knowlesi</em>"),
    subtitle        = HTML("Tobin dkk., 2024, PLOS Negl. Trop. Dis. &middot; <a href='https://pubmed.ncbi.nlm.nih.gov/38252650/' target='_blank' rel='noopener'>PMID: 38252650</a>"),
    language         = "Bahasa",
    display          = "Tampilan",
    layer_mean       = "Perkiraan rata-rata kecocokan penularan",
    layer_uncertainty= "Ketidakpastian prediksi (varians)",
    basemap          = "Peta dasar",
    basemap_clean    = "Abu-abu muda (label minimal)",
    show_risk        = "Tampilkan lapisan kecocokan penularan",
    show_sub_all     = "Tampilkan batas kecamatan (perbesar peta)",
    search_all       = "\u2014 Seluruh Indonesia (tanpa saringan) \u2014",
    reset_district   = "Atur ulang: tampilkan seluruh Indonesia",
    basemap_street   = "Peta jalan (penanda lokasi)",
    basemap_sat      = "Satelit",
    opacity          = "Opasitas lapisan kecocokan penularan",
    search           = "Cari kabupaten/kota",
    search_ph        = "Ketik nama kabupaten atau kota...",
    district_panel   = "Kabupaten/kota terpilih",
    no_district      = "Klik sebuah kabupaten pada peta, atau cari di atas, untuk melihat perkiraan kecocokan penularannya.",
    mean_risk_lab    = "Rata-rata perkiraan kecocokan penularan",
    uncertainty_lab  = "Rata-rata ketidakpastian (varians)",
    range_lab        = "Rentang dalam kabupaten",
    dist_title       = "Sebaran kecocokan penularan dalam kabupaten",
    dist_x           = "Perkiraan kecocokan penularan",
    dist_y           = "Sel grid",
    dist_meanline    = "rata-rata kabupaten",
    export           = "Unduh data",
    export_csv       = HTML("Tabel kecocokan penularan<br>kabupaten/kota (CSV)"),
    export_sub_csv   = HTML("Tabel kecocokan penularan<br>kecamatan (CSV)"),
    sub_search       = "Saring ke kecamatan",
    sub_search_ph    = "\u2014 Seluruh kabupaten (semua kecamatan) \u2014",
    sub_panel        = "Kecamatan terpilih",
    sub_hint         = "Batas kecamatan ditampilkan di peta; klik salah satu atau pilih di atas.",
    sub_back         = "Kembali ke seluruh kabupaten",
    sub_dist_title   = "Sebaran kecocokan penularan dalam kecamatan",
    sub_meanline     = "rata-rata kecamatan",
    sub_no_cells     = "Tidak ada sel kecocokan penularan (di luar masker) di kecamatan ini.",
    range_sub_lab    = "Rentang dalam kecamatan",
    layer_category   = "Kategori daftar risiko (SOP)",
    legend_category  = "Kategori daftar risiko",
    st_cat1          = "Kat. 1: penularan terkonfirmasi",
    st_cat2          = "Kat. 2: probable, belum terkonfirmasi",
    st_cat3          = "Kat. 3: reseptif (peta: tinggi)",
    st_watch         = "Daftar pantau (peta: sedang)",
    st_outside       = "Di luar model: gunakan bukti setempat",
    st_none          = "Tidak terdaftar (peta: rendah)",
    tier_lab         = "Tingkat peta",
    tier_high        = "tinggi", tier_moderate = "sedang", tier_low = "rendah",
    tier_outside     = "di luar model",
    status_lab       = "Kategori daftar risiko",
    coverage_lab     = "Tercakup model",
    share_lab        = "Luas dengan kecocokan ≥ %s",
    api_missing      = "belum tersedia",
    pk_api_lab       = "Insidens Pk (per 1.000/thn, median 3 thn)",
    api_lab          = "API semua spesies (Kemenkes, per 1.000/thn)",
    layer_pk_api     = "Insidens Pk (stratifikasi SOP)",
    layer_api        = "API malaria semua spesies (Kemenkes)",
    legend_pk_api    = "Insidens Pk per 1.000/thn (median 3 thn, SOP)",
    legend_api       = "API malaria per 1.000/thn (endemisitas Kemenkes)",
    bands_pk_api     = c("0 (tidak ada kasus)", ">0 \u2013 <0,1", "0,1 \u2013 <1 (SOP: risiko rendah)", "\u2265 1 (SOP: risiko tinggi)"),
    bands_api        = c("0 (tidak ada kasus)", ">0 \u2013 <1 (endemis rendah)", "1 \u2013 <5 (endemis sedang)", "\u2265 5 (endemis tinggi)"),
    api_nodata       = "Data belum tersedia",
    from_district    = "dari kabupaten/kota",
    confirmed_here   = "terkonfirmasi di sini: penelitian terbit",
    layer_evidence   = "Bukti Pk: Kemenkes vs penelitian",
    evid_lab         = "Dasar bukti Pk",
    evid_legend      = "Penularan P. knowlesi terkonfirmasi",
    evid_both        = "Kemenkes (SISMAL) + penelitian",
    evid_moh         = "Terkonfirmasi Kemenkes (SISMAL)",
    evid_research    = "Hanya terkonfirmasi PCR penelitian",
    evid_moh_prob    = "Hanya probable Kemenkes",
    evid_none        = "Tidak ada catatan terkonfirmasi",
    evid_site        = "Lokasi kasus penelitian (PCR)",
    evid_site_neg    = "Diperiksa, tanpa Pk / non-manusia",
    evid_sources     = "Sumber penelitian",
    layer_elim       = "Status eliminasi malaria (Kemenkes)",
    elim_lab         = "Eliminasi malaria",
    elim_eliminated  = "Sudah eliminasi (bersertifikat)",
    elim_not         = "Belum eliminasi",
    elim_nodata      = "Data belum tersedia",
    elim_legend      = "Status eliminasi malaria",
    elim_partial     = "Daftar sebagian dari sumber publik Kemenkes (%d kabupaten/kota eliminasi tercantum); daftar lengkap menyusul.",
    elim_pk_note     = "Sudah bersertifikat bebas malaria, tetapi P. knowlesi tidak dihitung dalam eliminasi. Wilayah Pk terkonfirmasi atau reseptif: kirim setiap kasus positif malaria untuk PCR.",
    defs_html        = HTML(paste0(
      "<p><strong>Definisi data kasus (Kemenkes).</strong> <em>API</em> = Annual ",
      "Parasite Incidence sesuai pelaporan SISMAL: kasus malaria terkonfirmasi ",
      "(mikroskopis atau RDT), semua spesies, per 1.000 penduduk per tahun; strata: ",
      "endemis tinggi &ge; 5, sedang 1&ndash;&lt;5, rendah &gt;0&ndash;&lt;1. ",
      "<em>Insidens Pk</em> = median jumlah kasus <em>P. knowlesi</em> per 1.000 ",
      "penduduk per tahun selama 3 tahun terakhir, sesuai stratifikasi risiko pada ",
      "draf SOP surveilans knowlesi (&ge; 1 = risiko tinggi).</p>")),
    sop_stratum_lab  = "Strata SOP (Pk \u2265 1/1.000)",
    stratum_high     = "tinggi", stratum_low = "rendah",
    pk_lab           = "Pk probable / PCR / terkonfirmasi",
    outside_legend   = "Di luar model (tanpa prediksi)",
    outside_note     = "Wilayah ini berada di luar cakupan model: prediksi dibatasi pada wilayah dalam jangkauan spesies kera dan nyamuk yang diperlukan untuk penularan zoonosis P. knowlesi (tumpang tindih peta jangkauan sekurang-kurangnya satu spesies reservoir dan satu spesies vektor), sehingga peta tidak memberi informasi di sini. Nilai 0 BUKAN berarti risiko rendah; gunakan data kasus dan bukti setempat.",
    export_list      = "Daftar risiko per wilayah (lampiran SOP, CSV)",
    calib_title      = "Kesesuaian aturan peta dengan kasus yang diketahui",
    calib_text       = "Dari %d kabupaten/kota dengan penularan terkonfirmasi, aturan peta menilai %d tinggi, %d sedang, %d rendah, dan %d di luar model. Karena itu kategori bukti selalu didahulukan di atas peta.",
    export_png       = "Simpan peta saat ini (PNG)",
    methods          = "Metode, data & sitasi",
    legend_mean      = "Rata-rata perkiraan kecocokan penularan",
    legend_uncert    = "Varians prediksi",
    modal_title      = "Tentang peta ini",
    modal_close      = "Jelajahi peta",
    modal_dont       = "Jangan tampilkan lagi",
    reopen_summary   = "Tentang peta ini",
    lay_summary      = HTML(paste0(
      "<p>Untuk memprediksi variasi spasial risiko relatif penularan <em>P. knowlesi</em>, ",
      "kami membangun model <strong>boosted regression tree</strong> untuk ",
      "mempelajari hubungan antara data lingkungan hasil penginderaan jauh &mdash; ",
      "meliputi <strong>elevasi, tutupan lahan</strong>, serta indeks <strong>gangguan ",
      "hutan, suhu</strong> dan <strong>kelembapan</strong> yang bervariasi setiap ",
      "tahun &mdash; dengan lokasi-lokasi tempat malaria <em>P. knowlesi</em> ",
      "teridentifikasi.</p>",
      "<p>Dengan demikian, prediksi kami menggambarkan <strong>kecocokan lingkungan ",
      "bagi penularan</strong> malaria <em>P. knowlesi</em>. ",
      "Prediksi model dibatasi pada wilayah dalam jangkauan spesies kera dan nyamuk yang diketahui diperlukan untuk penularan zoonosis <em>P. knowlesi</em> (yaitu tumpang tindih peta jangkauan sekurang-kurangnya satu spesies reservoir dan satu spesies vektor). ",
      "Permukaan ketidakpastian menggambarkan <strong>varians prediksi bootstrap",
      "</strong> dari model: kami menyesuaikan model pada sampel yang berbeda ",
      "dari kumpulan data infeksi malaria <em>P. knowlesi</em> untuk mengevaluasi ",
      "sensitivitas model terhadap infeksi yang secara lingkungan merupakan pencilan ",
      "(outlier).</p>",
      "<p>Permukaan model memiliki resolusi spasial &asymp; 5 x 5 km<sup>2</sup>.</p>",
      "<p>Baca selengkapnya tentang model ini di Tobin dkk., 2024: ",
      "<a href='https://doi.org/10.1371/journal.pntd.0011570' target='_blank' ",
      "rel='noopener'>doi:10.1371/journal.pntd.0011570</a></p>"
    )),
    methods_html = HTML(paste0(
      "<p><strong>Model.</strong> Model niche lingkungan (pengembangan dari Shearer ",
      "dkk. 2016) yang dilatih pada 524 catatan kemunculan <em>P. knowlesi</em> hingga ",
      "Maret 2020, dengan kovariat tambahan untuk deforestasi dan urbanisasi.</p>",
      "<p><strong>Lapisan.</strong> <em>Rata-rata</em> = rata-rata perkiraan kecocokan ",
      "penularan relatif. <em>Ketidakpastian</em> = varians prediksi (makin tinggi = ",
      "makin tidak pasti).</p>",
      "<p><strong>Nilai kabupaten.</strong> Angka per kabupaten adalah rata-rata semua ",
      "sel grid di dalam batas administrasi tingkat 2.</p>",
      "<p><strong>Sitasi.</strong> Tobin RJ, dkk. Updating estimates of ",
      "<em>Plasmodium knowlesi</em> malaria risk in response to changing land use ",
      "patterns across Southeast Asia. PLOS Negl Trop Dis. 2024. ",
      "<a href='https://doi.org/10.1371/journal.pntd.0011570' target='_blank'>",
      "doi:10.1371/journal.pntd.0011570</a> &middot; ",
      "<a href='https://pubmed.ncbi.nlm.nih.gov/38252650/' target='_blank'>",
      "PMID: 38252650</a></p>",
      "<p><strong>Permukaan data:</strong> ",
      "<a href='https://osf.io/k5bsa/overview' target='_blank'>osf.io/k5bsa</a></p>"
    ))
  )
)

# Basemap providers keyed by choice value. All work without an API key.
# (CARTO's Positron tiles now need a key and only show "API KEY REQUIRED".)
basemap_providers <- list(
  clean  = leaflet::providers$Esri.WorldGrayCanvas,
  street = leaflet::providers$OpenStreetMap.Mapnik,
  sat    = leaflet::providers$Esri.WorldImagery
)

# Use label-free variants below the raster where the provider offers one.
# Mapnik has no labels-only or no-label variant, so its original tiles remain
# below the raster while the closest transparent street labels are added above.
basemap_no_label_providers <- list(
  clean  = leaflet::providers$Esri.WorldGrayCanvas,
  street = leaflet::providers$OpenStreetMap.Mapnik,
  sat    = leaflet::providers$Esri.WorldImagery
)

# Labels drawn above the raster: Esri's labels-only reference layer works for
# every basemap and needs no API key (CARTO label tiles now require one).
# White text with a dark halo stays readable over the coloured layers.
labels_tile_url <- paste0(
  "https://server.arcgisonline.com/ArcGIS/rest/services/",
  "Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}"
)

# -----------------------------------------------------------------------------
# 4. Map-building helpers
# -----------------------------------------------------------------------------
# ---- preloaded district fill layer ---------------------------------------------
# The district shapes are sent ONCE (transparent) when the map is created. Each
# coloured view (category, evidence, Pk incidence, API, elimination) then only
# sends a vector of colours, applied in the browser (see pkSetFill in the UI).
add_fill_base <- function(map) {
  leaflet::addPolygons(
    map, data = districts_map, layerId = paste0("fill_", districts_map$district_id),
    group = "lyr_fill", fillColor = "#ffffff", fillOpacity = 0, stroke = FALSE,
    smoothFactor = 0.4, options = leaflet::pathOptions(pane = "risk", interactive = FALSE)
  )
}

# colour per district (districts_map order) for a coloured view; "" = no fill
fill_colors <- function(surface) {
  idx <- match(districts_map$district_id, district_stats$district_id)
  col <- switch(surface,
    category    = unname(status_colors[district_stats$status[idx]]),
    evidence    = { b <- district_stats$evidence_basis[idx]
                    ifelse(is.na(b), "#f1f5f9", unname(basis_colors[b])) },
    elimination = { e <- district_stats$elimination_status[idx]
                    ifelse(is.na(e), api_nodata_color, unname(elim_colors[e])) },
    pk_api      = api_fill_colors("pk_api"),
    api         = api_fill_colors("api"),
    rep("", nrow(districts_map)))
  col[is.na(col)] <- ""
  col
}

add_districts <- function(map) {
  leaflet::addPolygons(
    map,
    data = districts_map,
    layerId = ~district_id,
    group = "Districts",
    fill = TRUE,
    fillOpacity = 0,          # invisible fill keeps polygons clickable
    color = "#1f2933",
    opacity = 0.9,
    weight = 1,
    smoothFactor = 0.4,
    label = ~district_label,
    highlightOptions = leaflet::highlightOptions(
      color = "#0f172a",
      weight = 2,
      bringToFront = TRUE
    ),
    options = leaflet::pathOptions(pane = "districts")
  )
}

add_surface <- function(map, surface, opacity = 0.82) {
  domain <- surface_domains[[surface]]
  palette <- leaflet::colorNumeric(
    palette = map_palette,
    domain = domain,
    na.color = "transparent"
  )
  leaflet::addRasterImage(
    map,
    x = surface_layers[[surface]],
    colors = palette,
    opacity = opacity,
    group = paste0("lyr_", surface),
    project = FALSE,
    maxBytes = Inf,
    options = leaflet::gridOptions(pane = "risk", zIndex = 410)
  )
}

format_legend_labels <- function(x) {
  labels <- format(round(x, 2), trim = TRUE, scientific = FALSE)
  decimal_labels <- grepl("\\.", labels)
  labels[decimal_labels] <- sub("0+$", "", labels[decimal_labels])
  labels[decimal_labels] <- sub("\\.$", "", labels[decimal_labels])
  labels
}

add_surface_legend <- function(map, surface, lang) {
  tr <- i18n[[lang]]
  legend_title  <- if (surface == "mean") tr$legend_mean else tr$legend_uncert
  legend_breaks <- if (surface == "mean") seq(0, 0.8, by = 0.2) else seq(0, 3, by = 1)
  legend_labels <- format_legend_labels(legend_breaks)
  legend_domain <- surface_domains[[surface]]
  legend_positions <- 100 * (
    legend_breaks - legend_domain[["min"]]
  ) / (
    legend_domain[["max"]] - legend_domain[["min"]]
  )
  legend_positions <- pmin(100, pmax(0, legend_positions))
  legend_label_tags <- Map(function(label, position) {
    label_style <- if (position == 0) {
      "left: 0;"
    } else {
      paste0(
        "left: ", sprintf("%.6f", position),
        "%; transform: translateX(-50%);"
      )
    }
    tags$span(style = label_style, label)
  }, legend_labels, legend_positions)
  legend_gradient <- paste(map_palette, collapse = ", ")

  legend_html <- tags$div(
    tags$div(class = "legend-title", legend_title),
    tags$div(
      class = "legend-scale",
      tags$div(
        class = "legend-gradient",
        style = paste0(
          "background: linear-gradient(to right, ", legend_gradient, ");"
        )
      ),
      tags$div(
        class = "legend-labels",
        legend_label_tags
      )
    )
  )

  if (!is.null(outside_layer)) {
    legend_html <- tagList(legend_html, tags$div(
      class = "legend-item", style = "margin-top:6px;",
      tags$span(class = "legend-swatch", style = "background:#94a3b8;"),
      tr$outside_legend
    ))
  }

  leaflet::addControl(
    map,
    html = legend_html,
    position = "bottomright",
    layerId = "risk_legend",
    className = "risk-legend-control"
  )
}

# grey overlay where the model makes no prediction (stored as 0 in the tif)
add_outside_layer <- function(map, opacity = 0.82) {
  if (is.null(outside_layer)) return(map)
  leaflet::addRasterImage(
    map, x = outside_layer, colors = "#94a3b8", opacity = min(1, opacity),
    group = "lyr_outside", project = FALSE, maxBytes = Inf,
    options = leaflet::gridOptions(pane = "risk", zIndex = 411)
  )
}

status_label <- function(status, tr) {
  unname(c(cat1 = tr$st_cat1, cat2 = tr$st_cat2, cat3 = tr$st_cat3,
           watch = tr$st_watch, outside = tr$st_outside, none = tr$st_none)[status])
}
tier_label <- function(tier, tr) {
  unname(c(high = tr$tier_high, moderate = tr$tier_moderate, low = tr$tier_low,
           outside = tr$tier_outside)[tier])
}

add_category_legend <- function(map, lang) {
  tr <- i18n[[lang]]
  keys <- c("cat1", "cat2", "cat3", "watch", "outside", "none")
  html <- tags$div(
    tags$div(class = "legend-title", tr$legend_category),
    lapply(keys, function(k) tags$div(
      class = "legend-item",
      tags$span(class = "legend-swatch",
                style = paste0("background:", status_colors[[k]], ";")),
      status_label(k, tr)
    ))
  )
  leaflet::addControl(map, html = html, position = "bottomright",
                      layerId = "risk_legend", className = "risk-legend-control")
}

add_api_legend <- function(map, var, lang) {
  tr <- i18n[[lang]]
  sc <- api_scales[[var]]
  title  <- if (var == "pk_api") tr$legend_pk_api else tr$legend_api
  labels <- if (var == "pk_api") tr$bands_pk_api else tr$bands_api
  html <- tags$div(
    tags$div(class = "legend-title", style = "max-width:200px;white-space:normal;", title),
    lapply(seq_along(labels), function(i) tags$div(
      class = "legend-item",
      tags$span(class = "legend-swatch", style = paste0("background:", sc$colors[i], ";")),
      labels[i]
    )),
    tags$div(class = "legend-item",
             tags$span(class = "legend-swatch", style = paste0("background:", api_nodata_color, ";")),
             tr$api_nodata)
  )
  leaflet::addControl(map, html = html, position = "bottomright",
                      layerId = "risk_legend", className = "risk-legend-control")
}

elim_label <- function(status, year, tr) {
  if (is.na(status)) return(tr$elim_nodata)
  if (status == "eliminated")
    return(paste0(tr$elim_eliminated, if (!is.na(year) && nzchar(year)) paste0(", ", year) else ""))
  tr$elim_not
}

add_elim_legend <- function(map, lang) {
  tr <- i18n[[lang]]
  item <- function(col, txt) tags$div(class = "legend-item",
    tags$span(class = "legend-swatch", style = paste0("background:", col, ";")), txt)
  html <- tags$div(
    tags$div(class = "legend-title", tr$elim_legend),
    item(elim_colors[["eliminated"]], tr$elim_eliminated),
    item(elim_colors[["not_eliminated"]], tr$elim_not),
    item(api_nodata_color, tr$elim_nodata),
    tags$div(style = "max-width:200px;white-space:normal;font-size:10.5px;color:#64748b;margin-top:4px;",
             sprintf(tr$elim_partial, n_elim_listed))
  )
  leaflet::addControl(map, html = html, position = "bottomright",
                      layerId = "risk_legend", className = "risk-legend-control")
}

evid_label <- function(basis, tr) {
  if (is.na(basis)) return(tr$evid_none)
  c(both = tr$evid_both, moh = tr$evid_moh, research = tr$evid_research,
    moh_probable = tr$evid_moh_prob)[[basis]]
}

# research record markers: at coordinates, else kecamatan / district centre
pub_marker_xy <- function() {
  if (is.null(pub)) return(NULL)
  p <- pub[!is.na(pub$district_id), ]
  if (!nrow(p)) return(NULL)
  old_s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old_s2))
  cen_s <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(subdistricts))))
  cen_d <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(districts))))
  use_xy <- !is.na(p$lat) & p$matched_by == "coordinates"
  use_s  <- !use_xy & !is.na(p$subdistrict_id)
  p$mx <- ifelse(use_xy, p$lon, ifelse(use_s, cen_s[p$subdistrict_id, 1], cen_d[p$district_id, 1]))
  p$my <- ifelse(use_xy, p$lat, ifelse(use_s, cen_s[p$subdistrict_id, 2], cen_d[p$district_id, 2]))
  # small jitter so several records at one centre stay clickable
  dup <- duplicated(paste(round(p$mx, 3), round(p$my, 3)))
  set.seed(1); p$mx[dup] <- p$mx[dup] + stats::runif(sum(dup), -0.04, 0.04)
  p$my[dup] <- p$my[dup] + stats::runif(sum(dup), -0.04, 0.04)
  p
}
pub_markers <- NULL  # filled lazily (needs subdistricts)

add_evidence_markers <- function(map, lang) {
  if (is.null(pub_markers)) pub_markers <<- pub_marker_xy()
  p <- pub_markers
  if (is.null(p) || !nrow(p)) return(map)
  esc <- htmltools::htmlEscape
  popup <- paste0(
    "<div class='district-popup' style='max-width:280px'><strong>", esc(p$source_short), "</strong><br/>",
    "<span style='color:#64748b'>", esc(ifelse(is.na(p$site_text), "", p$site_text)), "</span><br/>",
    "Host: <b>", esc(p$host), "</b> &middot; ", esc(ifelse(is.na(p$diagnostic), "", p$diagnostic)), "<br/>",
    "Pk positive: <b>", esc(ifelse(is.na(p$n_pk_positive), "?", p$n_pk_positive)), "</b>",
    ifelse(is.na(p$n_tested), "", paste0(" / tested ", esc(p$n_tested))), "<br/>",
    "Sampling: ", esc(ifelse(is.na(p$sampling_years), "?", p$sampling_years)),
    " &middot; precision: ", esc(p$location_precision), " (", esc(p$matched_by), ")<br/>",
    ifelse(is.na(p$doi), "", paste0("<a href='https://doi.org/", esc(p$doi), "' target='_blank'>doi:", esc(p$doi), "</a>")),
    "</div>")
  leaflet::addCircleMarkers(
    map, lng = p$mx, lat = p$my, group = "evidence_sites",
    radius = ifelse(p$confirmed, 6, 4), weight = 1.5, color = "#ffffff",
    fillColor = ifelse(p$confirmed, "#111827", "#9ca3af"), fillOpacity = 0.95,
    label = paste0(p$source_short, ": ", ifelse(is.na(p$site_text), "", p$site_text)),
    popup = popup, options = leaflet::pathOptions(pane = "subdistricts")
  )
}

add_evidence_legend <- function(map, lang) {
  tr <- i18n[[lang]]
  item <- function(col, txt, round = FALSE) tags$div(class = "legend-item",
    tags$span(class = "legend-swatch", style = paste0("background:", col, ";",
              if (round) "border-radius:50%;width:11px;height:11px;" else "")), txt)
  html <- tags$div(
    tags$div(class = "legend-title", tr$evid_legend),
    item(basis_colors[["both"]], tr$evid_both),
    item(basis_colors[["moh"]], tr$evid_moh),
    item(basis_colors[["research"]], tr$evid_research),
    item(basis_colors[["moh_probable"]], tr$evid_moh_prob),
    item("#f1f5f9", tr$evid_none),
    item("#111827", tr$evid_site, TRUE),
    item("#9ca3af", tr$evid_site_neg, TRUE)
  )
  leaflet::addControl(map, html = html, position = "bottomright",
                      layerId = "risk_legend", className = "risk-legend-control")
}

# Pk caveat for eliminated districts that are on the Pk risk list
elim_pk_flag <- function(row) {
  isTRUE(row$elimination_status == "eliminated") && isTRUE(row$status %in% c("cat1", "cat2", "cat3"))
}

# Popup HTML for a district (bilingual)
district_popup <- function(id, lang) {
  tr <- i18n[[lang]]
  row <- district_stats[district_stats$district_id == id, ]
  if (nrow(row) == 0) return("")
  paste0(
    "<div class='district-popup'>",
    "<strong>", districts$district_label[id], "</strong><br/>",
    tr$mean_risk_lab, ": <b>", format(round(row$mean_risk, 2), nsmall = 2), "</b><br/>",
    tr$uncertainty_lab, ": <b>", format(round(row$mean_uncertainty, 2), nsmall = 2), "</b><br/>",
    tr$status_lab, ": <b style='color:", status_colors[[row$status]], "'>",
    status_label(row$status, tr), "</b><br/>",
    tr$evid_lab, ": <b>", evid_label(row$evidence_basis, tr), "</b>",
    if (!is.na(row$research_sources)) paste0(" <span style='color:#64748b;font-size:11px'>(",
      htmltools::htmlEscape(row$research_sources), ")</span>") else "", "<br/>",
    tr$pk_api_lab, ": <b>", fmt_api(row$pk_api, row$pk_api_years, tr), "</b><br/>",
    tr$api_lab, ": <b>", fmt_api(row$api, row$api_year, tr), "</b><br/>",
    tr$elim_lab, ": <b>", elim_label(row$elimination_status, row$elimination_year, tr), "</b>",
    "</div>"
  )
}

fmt_num <- function(x, d) if (is.na(x)) "–" else format(round(x, d), nsmall = d)

# "0.42 (2023-2025)" or "not yet supplied"
fmt_api <- function(value, period, tr) {
  if (is.na(value)) return(tr$api_missing)
  paste0(format(round(value, 2), nsmall = 2),
         if (!is.na(period) && nzchar(as.character(period))) paste0(" (", period, ")") else "")
}
stratum_label <- function(x, tr) {
  if (is.na(x)) tr$api_missing else if (x == "high") tr$stratum_high else tr$stratum_low
}

# small grey "(from district)" marker for values inherited from the district
from_tag <- function(flag, tr) {
  if (isTRUE(flag)) paste0(" <span style='color:#64748b;font-size:11px'>(", tr$from_district, ")</span>") else ""
}

# Popup HTML for a subdistrict (bilingual)
subdistrict_popup <- function(sid, lang) {
  tr <- i18n[[lang]]
  row <- subdistrict_stats[sid, ]
  paste0(
    "<div class='district-popup'>",
    "<strong>", row$name_3, "</strong><br/>",
    "<span style='color:#64748b'>", row$name_2, ", ", row$name_1, "</span><br/>",
    tr$mean_risk_lab, ": <b>", fmt_num(row$mean_risk, 2), "</b><br/>",
    tr$uncertainty_lab, ": <b>", fmt_num(row$mean_uncertainty, 2), "</b><br/>",
    tr$status_lab, ": <b style='color:", status_colors[[row$status]], "'>",
    status_label(row$status, tr), "</b>",
    if (isTRUE(row$confirmed_here)) paste0(" <span style='color:#2563eb;font-size:11px'>(",
      tr$confirmed_here, ")</span>") else from_tag(row$category_from_district, tr), "<br/>",
    tr$pk_api_lab, ": <b>", fmt_api(row$pk_api, row$pk_api_years, tr), "</b>",
    from_tag(row$pk_api_from_district, tr), "<br/>",
    tr$api_lab, ": <b>", fmt_api(row$api, row$api_year, tr), "</b>",
    from_tag(row$api_from_district, tr), "<br/>",
    tr$elim_lab, ": <b>", {
      drow <- district_stats[district_stats$district_id == row$district_id, ]
      elim_label(drow$elimination_status, drow$elimination_year, tr)
    }, "</b>", from_tag(TRUE, tr),
    "</div>"
  )
}

# Within-area histogram (shared by district and subdistrict panels)
plot_risk_hist <- function(vals, tr, meanline_lab) {
  op <- par(mar = c(4, 4, 1, 1), cex = 0.9)
  on.exit(par(op))
  # bars coloured by the map palette (as in the upstream app)
  histogram <- hist(vals, breaks = 20, plot = FALSE)
  risk_palette <- leaflet::colorNumeric(palette = map_palette,
                                        domain = surface_domains$mean,
                                        na.color = "transparent")
  plot(histogram, col = risk_palette(histogram$mids), border = "#ffffff",
       main = "", xlab = tr$dist_x, ylab = tr$dist_y)
  m <- mean(vals, na.rm = TRUE)
  abline(v = m, col = "#b91c1c", lwd = 2)
  usr <- par("usr")
  text(m, usr[4] * 0.92, labels = meanline_lab,
       col = "#b91c1c", pos = 4, cex = 0.85, offset = 0.3)
}

# -----------------------------------------------------------------------------
# 5. UI
# -----------------------------------------------------------------------------
ui <- shiny::fluidPage(
  tags$head(
    tags$title("Plasmodium knowlesi transmission suitability"),
    tags$style(HTML("
      html, body, .container-fluid { height: 100%; margin: 0; overflow: hidden; }
      .container-fluid { padding: 0; }
      .app-shell {
        display: grid;
        grid-template-columns: 330px minmax(0, 1fr);
        height: 100vh; background: #f8fafc;
      }
      .sidebar {
        display: flex; flex-direction: column; gap: 18px;
        padding: 20px 20px; border-right: 1px solid #d7dee8;
        background: #ffffff; color: #17202a; overflow-y: auto; z-index: 500;
      }
      .sidebar h1 { margin: 0; font-size: 21px; font-weight: 700; line-height: 1.2; }
      .sidebar .subtitle { margin: 4px 0 0; color: #52606d; font-size: 13px; line-height: 1.45; }
      .control-label, .sidebar label { font-size: 12px; font-weight: 700; color: #52606d; text-transform: uppercase; }
      .radio-inline { text-transform: none; font-weight: 400; font-size: 13px; color: #17202a; }
      .radio label { display: flex; align-items: center; gap: 8px; min-height: 26px; font-size: 14px; color: #17202a; text-transform: none; font-weight: 400; }
      .radio input { margin: 0; }
      .map-wrap, #map { min-width: 0; height: 100%; }
      .stat-card { background: #f1f5f9; border: 1px solid #e2e8f0; border-radius: 6px; padding: 12px 14px; }
      .stat-card .dist-name { font-weight: 700; font-size: 15px; margin-bottom: 8px; }
      .stat-row { display: flex; justify-content: space-between; font-size: 13px; margin: 3px 0; }
      .stat-row .val { font-weight: 700; }
      .stat-divider { border: 0; border-top: 1px solid #cbd5e1; margin: 8px 0; }
      .muted { color: #64748b; font-size: 13px; line-height: 1.4; }
      details.methods { border: 1px solid #e2e8f0; border-radius: 6px; padding: 4px 10px; background: #fff; }
      details.methods summary { cursor: pointer; font-weight: 700; font-size: 13px; color: #334155; padding: 6px 0; }
      details.methods .methods-body { font-size: 12.5px; line-height: 1.5; color: #334155; }
      .dl-btn { display: block; width: 100%; margin: 6px 0 0; font-size: 13px; }
      .shiny-download-link.dl-btn { height: auto; line-height: 1.25; white-space: normal; text-align: center; }
      .risk-legend-control { padding: 8px 10px; background: rgba(255,255,255,0.94); border: 1px solid rgba(15,23,42,0.12); border-radius: 4px; box-shadow: 0 1px 5px rgba(15,23,42,0.22); color: #17202a; }
      .risk-legend-control .legend-title { margin-bottom: 6px; font-size: 12px; font-weight: 700; line-height: 1.25; }
      .risk-legend-control .legend-scale { width: 180px; }
      .risk-legend-control .legend-gradient { width: 180px; height: 14px; border: 1px solid rgba(15,23,42,0.18); }
      .risk-legend-control .legend-labels { position: relative; width: 180px; height: 14px; margin-top: 5px; font-size: 11px; line-height: 1; white-space: nowrap; }
      .risk-legend-control .legend-labels span { position: absolute; top: 0; }
      .risk-legend-control .legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; line-height: 1.6; white-space: nowrap; }
      .risk-legend-control .legend-swatch { display: inline-block; width: 14px; height: 12px; border: 1px solid rgba(15,23,42,0.25); flex: none; }
      .layer-toggles { margin-top: -8px; }
      .layer-toggles .checkbox { margin: 2px 0; }
      .reset-row { margin-top: -10px; font-size: 13px; }
      .status-pill { display: inline-block; padding: 1px 7px; border-radius: 10px; color: #fff; font-size: 12px; font-weight: 700; }
      .status-pill.light { color: #17202a; }
      .note-box { background: #fef9c3; border: 1px solid #fde68a; border-radius: 6px; padding: 8px 10px; font-size: 12.5px; line-height: 1.45; color: #713f12; margin-top: 8px; }
      .modal-lay p { line-height: 1.55; }
      .modal-lay .lay-lang-tag { display: inline-block; font-size: 11px; font-weight: 700; letter-spacing: 0.04em; text-transform: uppercase; color: #0f766e; background: #ccfbf1; border-radius: 4px; padding: 2px 8px; margin: 0 0 6px; }
      .modal-lay .lay-divider { border: 0; border-top: 1px solid #e2e8f0; margin: 18px 0 14px; }
      @media (max-width: 820px) {
        html, body, .container-fluid { overflow: auto; }
        .app-shell { grid-template-columns: 1fr; grid-template-rows: auto 62vh; height: auto; min-height: 100vh; }
        .sidebar { border-right: 0; border-bottom: 1px solid #d7dee8; max-height: none; }
        .map-wrap, #map { height: 62vh; }
      }
    ")),
    tags$script(src = "html2canvas.min.js"),
    tags$script(HTML("
      // Recolour / re-opacity preloaded layers without resending geometry
      $(function() {
        function pkMap() {
          var w = window.HTMLWidgets && HTMLWidgets.find('#map');
          return w && w.getMap ? w.getMap() : null;
        }
        Shiny.addCustomMessageHandler('pkSetFill', function(msg) {
          var m = pkMap(); if (!m) return;
          for (var i = 0; i < msg.ids.length; i++) {
            var l = m.layerManager.getLayer('shape', 'fill_' + msg.ids[i]);
            if (!l) continue;
            var c = msg.colors[i];
            l.setStyle({ fillColor: c || '#ffffff', fillOpacity: c ? msg.opacity : 0 });
          }
        });
        Shiny.addCustomMessageHandler('pkGroupOpacity', function(msg) {
          var m = pkMap(); if (!m) return;
          msg.groups.forEach(function(g) {
            var lg = m.layerManager.getLayerGroup(g);
            if (lg) lg.eachLayer(function(l) { if (l.setOpacity) l.setOpacity(msg.opacity); });
          });
        });
      });
    ")),
    tags$script(HTML("
      function exportCurrentMap() {
        var mapElement = document.getElementById('map');
        if (!mapElement || typeof window.html2canvas !== 'function') {
          window.alert('The map image exporter could not be loaded.');
          return false;
        }

        window.html2canvas(mapElement, {
          useCORS: true,
          allowTaint: false,
          backgroundColor: '#ffffff',
          logging: false,
          scale: Math.min(window.devicePixelRatio || 1, 2),
          onclone: function(clonedDocument) {
            var districtSvgs = clonedDocument.querySelectorAll(
              '#map .leaflet-districts-pane svg'
            );
            districtSvgs.forEach(function(svg) {
              var transform = svg.style.transform;
              if (!transform || transform === 'none') return;

              // html2canvas 1.4.1 applies transforms on SVG elements twice.
              // Transfer the transform to a regular div in the cloned document,
              // where it is applied once, without changing the live Leaflet map.
              var wrapper = clonedDocument.createElement('div');
              wrapper.style.position = 'absolute';
              wrapper.style.left = '0';
              wrapper.style.top = '0';
              wrapper.style.transform = transform;
              wrapper.style.transformOrigin = svg.style.transformOrigin || '0 0';
              svg.style.transform = 'none';
              svg.parentNode.insertBefore(wrapper, svg);
              wrapper.appendChild(svg);
            });
          }
        }).then(function(canvas) {
          canvas.toBlob(function(blob) {
            if (!blob) {
              window.alert('The map image could not be created.');
              return;
            }
            var url = window.URL.createObjectURL(blob);
            var download = document.createElement('a');
            download.href = url;
            download.download = 'knowlesi-transmission-suitability-map.png';
            document.body.appendChild(download);
            download.click();
            download.remove();
            window.setTimeout(function() {
              window.URL.revokeObjectURL(url);
            }, 1000);
          }, 'image/png');
        }).catch(function(error) {
          console.error('Map export failed:', error);
          window.alert(
            'The map image could not be created. Please wait for the map tiles to load and try again.'
          );
        });
        return false;
      }
    "))
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      # language toggle is fixed (its own labels are self-evident)
      div(
        class = "lang-toggle",
        radioButtons(
          "lang", label = NULL,
          choices = c("English" = "en", "Bahasa Indonesia" = "id"),
          selected = "en", inline = TRUE
        )
      ),
      uiOutput("title_block"),
      uiOutput("layer_control"),
      uiOutput("basemap_control"),
      sliderInput("opacity", label = NULL, min = 0.2, max = 1, value = 0.82, step = 0.02),
      uiOutput("opacity_label"),
      uiOutput("search_control"),
      uiOutput("reset_control"),
      uiOutput("subdistrict_control"),
      uiOutput("district_panel"),
      uiOutput("export_control"),
      uiOutput("methods_panel"),
      uiOutput("reopen_control")
    ),
    div(class = "map-wrap", leafletOutput("map", height = "100%"))
  )
)

# -----------------------------------------------------------------------------
# 6. Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  lang <- reactive(input$lang %||% "en")
  selected_district <- reactiveVal(NULL)
  selected_subdistrict <- reactiveVal(NULL)   # integer subdistrict_id or NULL

  # ---- entry pop-up (lay summary) -------------------------------------------
  # The entry summary is always shown bilingually (English + Bahasa Indonesia)
  # so every visitor can read it regardless of the language toggle.
  show_summary <- function() {
    showModal(modalDialog(
      title = paste0(i18n$en$modal_title, "  /  ", i18n$id$modal_title),
      div(
        class = "modal-lay",
        tags$div(class = "lay-lang-tag", "English"),
        i18n$en$lay_summary,
        tags$hr(class = "lay-divider"),
        tags$div(class = "lay-lang-tag", "Bahasa Indonesia"),
        i18n$id$lay_summary
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton(paste0(i18n$en$modal_close, " / ", i18n$id$modal_close))
      )
    ))
  }
  # show once on startup
  observeEvent(TRUE, show_summary(), once = TRUE)

  observeEvent(input$reopen_summary, show_summary())

  # ---- translated UI blocks -------------------------------------------------
  output$title_block <- renderUI({
    tr <- i18n[[lang()]]
    tagList(h1(tr$title_html), div(class = "subtitle", tr$subtitle))
  })

  output$layer_control <- renderUI({
    tr <- i18n[[lang()]]
    tagList(
      radioButtons(
        "surface", label = tr$display,
        choices = setNames(c("mean", "uncertainty", "category", "evidence", "pk_api", "api", "elimination"),
                           c(tr$layer_mean, tr$layer_uncertainty, tr$layer_category,
                             tr$layer_evidence, tr$layer_pk_api, tr$layer_api, tr$layer_elim)),
        selected = isolate(input$surface) %||% "mean"
      ),
      tags$div(
        class = "layer-toggles",
        checkboxInput("show_risk", tr$show_risk,
                      value = isolate(input$show_risk) %||% TRUE),
        if (has_subdistricts)
          checkboxInput("show_sub_all", tr$show_sub_all,
                        value = isolate(input$show_sub_all) %||% FALSE)
      )
    )
  })

  output$basemap_control <- renderUI({
    tr <- i18n[[lang()]]
    is_id <- identical(lang(), "id")
    tagList(
      radioButtons(
        "basemap", label = tr$basemap,
        choices = setNames(c("clean", "street", "sat"),
                           c(tr$basemap_clean, tr$basemap_street, tr$basemap_sat)),
        selected = isolate(input$basemap) %||% "street"
      ),
      checkboxInput(
        "labels_top",
        if (is_id) "Nama tempat & jalan di atas peta" else "Place & street labels on top",
        value = isolate(input$labels_top) %||% FALSE
      )
    )
  })

  output$opacity_label <- renderUI({
    tags$div(class = "muted", style = "margin-top:-12px;", i18n[[lang()]]$opacity)
  })

  output$search_control <- renderUI({
    tr <- i18n[[lang()]]
    # first option is a labelled blank = "show all / reset"; the x button
    # (remove_button plugin) also clears it
    choices <- c(setNames("", tr$search_all),
                 setNames(districts$district_id, districts$district_label))
    selectizeInput(
      "district_search", label = tr$search,
      choices = choices, selected = isolate(selected_district()) %||% "",
      options = list(placeholder = tr$search_ph, allowEmptyOption = TRUE,
                     plugins = list("remove_button"))
    )
  })

  # separate output so it appears/disappears as the selection changes
  output$reset_control <- renderUI({
    id <- selected_district()
    if (is.null(id) || id == "") return(NULL)
    tags$div(class = "reset-row",
             actionLink("reset_district", paste0("↺ ", i18n[[lang()]]$reset_district)))
  })

  # Subdistrict filter: only shown once a district is selected
  output$subdistrict_control <- renderUI({
    if (!has_subdistricts) return(NULL)
    id <- selected_district()
    if (is.null(id) || id == "") return(NULL)
    tr <- i18n[[lang()]]
    rows <- subdistricts_by_district[[as.integer(id)]]
    if (length(rows) == 0) return(NULL)
    choices <- c(setNames("", tr$sub_search_ph),
                 setNames(subdistricts$subdistrict_id[rows], subdistricts$name_3[rows]))
    tagList(
      selectizeInput(
        "subdistrict_search", label = tr$sub_search,
        choices = choices, selected = isolate(selected_subdistrict()) %||% "",
        options = list(placeholder = tr$sub_search_ph, allowEmptyOption = TRUE,
                       plugins = list("remove_button"))
      ),
      tags$div(class = "muted", style = "margin-top:-10px;", tr$sub_hint)
    )
  })

  output$export_control <- renderUI({
    tr <- i18n[[lang()]]
    tagList(
      tags$div(class = "control-label", tr$export),
      downloadButton("dl_list", tr$export_list, class = "dl-btn btn-sm btn-primary"),
      downloadButton("dl_csv", tr$export_csv, class = "dl-btn btn-sm"),
      if (has_subdistricts)
        downloadButton("dl_sub_csv", tr$export_sub_csv, class = "dl-btn btn-sm"),
      tags$a(
        href = "#",
        role = "button",
        class = "dl-btn btn btn-sm btn-default",
        onclick = "return exportCurrentMap();",
        tr$export_png
      )
    )
  })

  output$methods_panel <- renderUI({
    tr <- i18n[[lang()]]
    tags$details(
      class = "methods",
      tags$summary(tr$methods),
      tags$div(
        class = "methods-body", tr$methods_html, tr$defs_html,
        if (length(cat1_rows)) tags$p(
          tags$strong(paste0(tr$calib_title, ". ")),
          sprintf(tr$calib_text, length(cat1_rows), calibration[["high"]],
                  calibration[["moderate"]], calibration[["low"]],
                  calibration[["outside"]]),
          tags$br(),
          tags$span(class = "muted", style = "font-size:11.5px;",
                    paste0("Rule: ", risk_rule$rule_label))
        )
      )
    )
  })

  output$reopen_control <- renderUI({
    actionLink("reopen_summary", i18n[[lang()]]$reopen_summary)
  })

  # ---- district detail panel (values + distribution plot) -------------------
  output$district_panel <- renderUI({
    tr <- i18n[[lang()]]
    id <- selected_district()
    if (is.null(id) || id == "") {
      return(tagList(
        tags$div(class = "control-label", tr$district_panel),
        tags$div(class = "muted", tr$no_district)
      ))
    }
    id <- as.integer(id)
    sid <- selected_subdistrict()
    if (!is.null(sid)) {
      srow <- subdistrict_stats[sid, ]
      return(tagList(
        tags$div(class = "control-label", tr$sub_panel),
        tags$div(
          class = "stat-card",
          tags$div(class = "dist-name", srow$name_3),
          tags$div(class = "muted", style = "margin:-6px 0 8px;",
                   districts$district_label[id]),
          tags$div(class = "stat-row",
                   tags$span(tr$mean_risk_lab),
                   tags$span(class = "val", fmt_num(srow$mean_risk, 2))),
          tags$div(class = "stat-row",
                   tags$span(tr$uncertainty_lab),
                   tags$span(class = "val", fmt_num(srow$mean_uncertainty, 2))),
          tags$div(class = "stat-row",
                   tags$span(tr$range_sub_lab),
                   tags$span(class = "val",
                             paste0(fmt_num(srow$risk_min, 2), " - ",
                                    fmt_num(srow$risk_max, 2)))),
          tags$hr(style = "margin:8px 0;"),
          tags$div(class = "stat-row",
                   tags$span(tr$status_lab),
                   tags$span(class = paste("status-pill",
                                           if (srow$status %in% c("watch", "none")) "light"),
                             style = paste0("background:", status_colors[[srow$status]], ";"),
                             status_label(srow$status, tr))),
          if (isTRUE(srow$confirmed_here))
            tags$div(style = "font-size:11.5px;text-align:right;margin-top:-2px;color:#2563eb;",
                     paste0("(", tr$confirmed_here, ")"))
          else if (isTRUE(srow$category_from_district))
            tags$div(class = "muted", style = "font-size:11.5px;text-align:right;margin-top:-2px;",
                     paste0("(", tr$from_district, ")")),
          tags$div(class = "stat-row",
                   tags$span(tr$tier_lab),
                   tags$span(class = "val", tier_label(srow$tier, tr))),
          tags$div(class = "stat-row",
                   tags$span(tr$coverage_lab),
                   tags$span(class = "val", paste0(fmt_num(100 * srow$model_coverage, 0), "%"))),
          tags$div(class = "stat-row",
                   tags$span(tr$pk_api_lab),
                   tags$span(class = "val", HTML(paste0(
                     htmltools::htmlEscape(fmt_api(srow$pk_api, srow$pk_api_years, tr)),
                     from_tag(srow$pk_api_from_district, tr))))),
          tags$div(class = "stat-row",
                   tags$span(tr$api_lab),
                   tags$span(class = "val", HTML(paste0(
                     htmltools::htmlEscape(fmt_api(srow$api, srow$api_year, tr)),
                     from_tag(srow$api_from_district, tr)))))
        ),
        if (isTRUE(srow$model_coverage < 0.5))
          tags$div(class = "note-box", tr$outside_note),
        actionLink("sub_back", paste0("← ", tr$sub_back)),
        tags$div(class = "control-label", style = "margin-top:6px;", tr$sub_dist_title),
        if (srow$n_cells > 1) plotOutput("sub_dist_plot", height = "170px")
        else tags$div(class = "muted", tr$sub_no_cells)
      ))
    }
    row <- district_stats[district_stats$district_id == id, ]
    tagList(
      tags$div(class = "control-label", tr$district_panel),
      tags$div(
        class = "stat-card",
        tags$div(class = "dist-name", districts$district_label[id]),
        tags$div(class = "stat-row",
                 tags$span(tr$mean_risk_lab),
                 tags$span(class = "val", format(round(row$mean_risk, 2), nsmall = 2))),
        tags$div(class = "stat-row",
                 tags$span(tr$range_lab),
                 tags$span(class = "val",
                           paste0(format(round(row$risk_min, 2), nsmall = 2), " - ",
                                  format(round(row$risk_max, 2), nsmall = 2)))),
        tags$hr(class = "stat-divider"),
        tags$div(class = "stat-row",
                 tags$span(tr$uncertainty_lab),
                 tags$span(class = "val", format(round(row$mean_uncertainty, 2), nsmall = 2))),
        tags$hr(class = "stat-divider"),
        tags$div(class = "stat-row",
                 tags$span(tr$status_lab),
                 tags$span(class = paste("status-pill",
                                         if (row$status %in% c("watch", "none")) "light"),
                           style = paste0("background:", status_colors[[row$status]], ";"),
                           status_label(row$status, tr))),
        tags$div(class = "stat-row",
                 tags$span(tr$tier_lab),
                 tags$span(class = "val", tier_label(row$tier, tr))),
        tags$div(class = "stat-row",
                 tags$span(sprintf(tr$share_lab, risk_rule$high)),
                 tags$span(class = "val", paste0(fmt_num(100 * row$share_high, 0), "%"))),
        tags$div(class = "stat-row",
                 tags$span(tr$coverage_lab),
                 tags$span(class = "val", paste0(fmt_num(100 * row$model_coverage, 0), "%"))),
        tags$div(class = "stat-row",
                 tags$span(tr$pk_api_lab),
                 tags$span(class = "val", fmt_api(row$pk_api, row$pk_api_years, tr))),
        tags$div(class = "stat-row",
                 tags$span(tr$sop_stratum_lab),
                 tags$span(class = "val", stratum_label(row$sop_stratum, tr))),
        tags$div(class = "stat-row",
                 tags$span(tr$api_lab),
                 tags$span(class = "val", fmt_api(row$api, row$api_year, tr))),
        tags$div(class = "stat-row",
                 tags$span(tr$elim_lab),
                 tags$span(class = "val",
                           style = if (isTRUE(row$elimination_status == "eliminated"))
                             paste0("color:", elim_colors[["eliminated"]], ";"),
                           elim_label(row$elimination_status, row$elimination_year, tr))),
        tags$div(class = "stat-row",
                 tags$span(tr$evid_lab),
                 tags$span(class = "val", evid_label(row$evidence_basis, tr))),
        if (!is.na(row$research_sources))
          tags$div(class = "muted", style = "font-size:11.5px;text-align:right;",
                   paste0(tr$evid_sources, ": ", row$research_sources)),
        if (!is.na(row$pk_probable) || !is.na(row$pk_confirmed))
          tags$div(class = "stat-row",
                   tags$span(tr$pk_lab),
                   tags$span(class = "val", paste(
                     ifelse(is.na(c(row$pk_probable, row$pk_pcr_tested, row$pk_confirmed)),
                            "–", c(row$pk_probable, row$pk_pcr_tested, row$pk_confirmed)),
                     collapse = " / "))),
        if (!is.na(row$evidence_source))
          tags$div(class = "muted", style = "font-size:11.5px;margin-top:4px;",
                   row$evidence_source,
                   if (!is.na(row$notes) && nzchar(row$notes)) paste0(" · ", row$notes))
      ),
      if (elim_pk_flag(row))
        tags$div(class = "note-box", tr$elim_pk_note),
      if (isTRUE(row$model_coverage < 0.5))
        tags$div(class = "note-box", tr$outside_note),
      tags$div(class = "control-label", style = "margin-top:6px;", tr$dist_title),
      plotOutput("dist_plot", height = "170px")
    )
  })

  output$dist_plot <- renderPlot({
    id <- selected_district()
    req(!is.null(id) && id != "")
    tr <- i18n[[lang()]]
    vals <- district_cells[[as.character(as.integer(id))]]
    req(length(vals) > 1)
    plot_risk_hist(vals, tr, tr$dist_meanline)
  })

  output$sub_dist_plot <- renderPlot({
    sid <- selected_subdistrict()
    req(!is.null(sid))
    tr <- i18n[[lang()]]
    vals <- subdistrict_cells[[as.character(sid)]]
    req(length(vals) > 1)
    plot_risk_hist(vals, tr, tr$sub_meanline)
  })

  if (has_subdistricts) {
    output$dl_sub_csv <- downloadHandler(
      filename = function() {
        id <- selected_district()
        if (is.null(id) || id == "") "knowlesi_subdistrict_risk.csv"
        else paste0("knowlesi_subdistrict_risk_",
                    gsub("[^A-Za-z0-9]+", "_", districts$name_2[as.integer(id)]), ".csv")
      },
      content = function(file) {
        out <- subdistrict_stats
        id <- selected_district()
        # export just the selected district's subdistricts, or all if none selected
        if (!is.null(id) && id != "") out <- out[out$district_id == as.integer(id), ]
        tr <- i18n[[lang()]]
        out <- data.frame(
          subdistrict            = out$name_3,
          district               = out$name_2,
          province               = out$name_1,
          risk_list_category     = status_label(out$status, tr),
          category_from_district = ifelse(out$category_from_district & !out$confirmed_here, "yes", ""),
          confirmed_here_research = ifelse(out$confirmed_here, "yes", ""),
          map_tier               = tier_label(out$tier, tr),
          mean_risk              = round(out$mean_risk, 3),
          sd_risk                = round(out$sd_risk, 3),
          mean_uncertainty       = round(out$mean_uncertainty, 3),
          risk_min               = round(out$risk_min, 3),
          risk_max               = round(out$risk_max, 3),
          model_coverage         = round(out$model_coverage, 3),
          n_grid_cells           = out$n_cells,
          pk_incidence_per_1000  = out$pk_api,
          pk_incidence_years     = out$pk_api_years,
          pk_from_district       = ifelse(out$pk_api_from_district, "yes", ""),
          api_per_1000_kemenkes  = out$api,
          api_year               = out$api_year,
          api_from_district      = ifelse(out$api_from_district, "yes", ""),
          stringsAsFactors = FALSE
        )
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )
  }

  # ---- Risk list by area (SOP annex) ----------------------------------------
  # One row per district, sorted by category; lists the high/moderate-tier
  # kecamatan inside each district so dinkes can name puskesmas areas.
  output$dl_list <- downloadHandler(
    filename = function() paste0("knowlesi_risk_list_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      tr <- i18n[[lang()]]
      ds <- district_stats
      sub_names <- function(i, tier) {
        if (!has_subdistricts) return(NA_character_)
        s <- subdistrict_stats[subdistrict_stats$district_id == ds$district_id[i] &
                                 subdistrict_stats$tier == tier, "name_3"]
        if (length(s)) paste(sort(s), collapse = "; ") else ""
      }
      out <- data.frame(
        province            = ds$name_1,
        district            = ds$name_2,
        risk_list_category  = status_label(ds$status, tr),
        on_risk_list        = ifelse(ds$on_risk_list, "YES", "no"),
        evidence_category   = ds$evidence_category,
        map_tier            = tier_label(ds$tier, tr),
        mean_suitability    = round(ds$mean_risk, 3),
        share_area_high     = round(ds$share_high, 3),
        share_area_moderate = round(ds$share_moderate, 3),
        mean_uncertainty    = round(ds$mean_uncertainty, 3),
        model_coverage      = round(ds$model_coverage, 3),
        pk_incidence_per_1000 = ds$pk_api,
        pk_incidence_years  = ds$pk_api_years,
        sop_stratum_pk      = ds$sop_stratum,
        api_per_1000_kemenkes  = ds$api,
        api_year      = ds$api_year,
        evidence_basis      = ds$evidence_basis,
        research_sources    = ds$research_sources,
        elimination_status  = ds$elimination_status,
        elimination_year    = ds$elimination_year,
        elimination_source  = ds$elimination_source,
        pk_probable         = ds$pk_probable,
        pk_pcr_tested       = ds$pk_pcr_tested,
        pk_confirmed        = ds$pk_confirmed,
        evidence_source     = ds$evidence_source,
        notes               = ds$notes,
        subdistricts_high     = vapply(seq_len(nrow(ds)), sub_names, "", tier = "high"),
        subdistricts_moderate = vapply(seq_len(nrow(ds)), sub_names, "", tier = "moderate"),
        rule                = risk_rule$rule_label,
        generated           = format(Sys.Date()),
        stringsAsFactors = FALSE
      )
      ord <- order(match(ds$status, c("cat1", "cat2", "cat3", "watch", "outside", "none")),
                   ds$name_1, ds$name_2)
      utils::write.csv(out[ord, ], file, row.names = FALSE, na = "")
    }
  )

  # ---- CSV download ---------------------------------------------------------
  output$dl_csv <- downloadHandler(
    filename = function() "knowlesi_district_risk.csv",
    content = function(file) {
      out <- district_stats[, c("name_2", "name_1", "mean_risk", "sd_risk",
                                "mean_uncertainty", "risk_min", "risk_max", "n_cells")]
      names(out) <- c("district", "province", "mean_risk", "sd_risk",
                      "mean_uncertainty", "risk_min", "risk_max", "n_grid_cells")
      utils::write.csv(out, file, row.names = FALSE)
    }
  )

  # ---- base map -------------------------------------------------------------
  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = FALSE)) |>
      leaflet::addMapPane("risk", zIndex = 410) |>
      leaflet::addMapPane("labels", zIndex = 415) |>
      leaflet::addMapPane("districts", zIndex = 420) |>
      leaflet::addMapPane("subdistricts", zIndex = 425) |>
      leaflet::addProviderTiles(
        basemap_providers[["street"]],
        group = "basemap",
        options = leaflet::providerTileOptions(opacity = 1, zIndex = 1)
      ) |>
      leaflet::fitBounds(
        lng1 = district_bbox[["xmin"]], lat1 = district_bbox[["ymin"]],
        lng2 = district_bbox[["xmax"]], lat2 = district_bbox[["ymax"]]
      ) |>
      add_surface("mean", opacity = 0.82) |>
      add_surface("uncertainty", opacity = 0.82) |>
      add_outside_layer(opacity = 0.82) |>
      add_fill_base() |>
      add_evidence_markers("en") |>
      add_surface_legend("mean", "en") |>
      add_districts() |>
      leaflet::hideGroup(c("lyr_uncertainty", "evidence_sites"))
    m
  })

  # ---- basemap and label-layer switch ---------------------------------------
  update_map_tiles <- function() {
    basemap <- input$basemap %||% "street"
    labels_on_top <- isTRUE(input$labels_top)
    base_provider <- if (labels_on_top) {
      basemap_no_label_providers[[basemap]]
    } else {
      basemap_providers[[basemap]]
    }

    # Street/satellite look best at full opacity; clean slightly muted.
    opacity <- if (basemap == "clean") 0.9 else 1
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearGroup("basemap") |>
      leaflet::clearGroup("labels") |>
      leaflet::addProviderTiles(
        base_provider,
        group = "basemap",
        options = leaflet::providerTileOptions(opacity = opacity, zIndex = 1)
      )

    if (labels_on_top) {
      proxy <- proxy |>
        leaflet::addTiles(
          urlTemplate = labels_tile_url,
          attribution = "Labels &copy; Esri",
          group = "labels",
          options = leaflet::tileOptions(pane = "labels", zIndex = 416)
        )
    }
    proxy
  }

  # ---- redraw surface/legend on layer, opacity or language change -----------
  # Layers are preloaded in output$map; switching only shows/hides groups and
  # sends district colours (a few KB) instead of rebuilding layers.
  redraw_surface <- function() {
    surface <- input$surface %||% "mean"
    op <- input$opacity %||% 0.82
    show <- isTRUE(input$show_risk %||% TRUE)
    raster_view <- surface %in% c("mean", "uncertainty")
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::hideGroup(c("lyr_mean", "lyr_uncertainty", "lyr_outside", "evidence_sites")) |>
      leaflet::removeControl("risk_legend")
    session$sendCustomMessage("pkSetFill", list(
      ids = districts_map$district_id,
      colors = if (show && !raster_view) fill_colors(surface) else rep("", nrow(districts_map)),
      opacity = op * 0.85))
    if (!show) return(proxy)
    if (raster_view) {
      session$sendCustomMessage("pkGroupOpacity",
        list(groups = list(paste0("lyr_", surface), "lyr_outside"), opacity = op))
      proxy |> leaflet::showGroup(c(paste0("lyr_", surface), "lyr_outside")) |>
        add_surface_legend(surface, lang())
    } else if (surface == "category") {
      proxy |> add_category_legend(lang())
    } else if (surface == "evidence") {
      proxy |> leaflet::showGroup("evidence_sites") |> add_evidence_legend(lang())
    } else if (surface == "elimination") {
      proxy |> add_elim_legend(lang())
    } else {
      proxy |> add_api_legend(surface, lang())
    }
  }

  observeEvent(input$show_risk,  redraw_surface(), ignoreInit = TRUE)
  observeEvent(
    list(input$basemap, input$labels_top),
    update_map_tiles(),
    ignoreInit = TRUE
  )
  observeEvent(list(input$surface, input$opacity, input$lang), {
    redraw_surface()
  }, ignoreInit = TRUE)

  # ---- district selection: from map click -----------------------------------
  # Subdistrict shapes use layerIds "sub_<id>" so clicks can be told apart
  # from district clicks (plain numeric ids).
  # "suball_<id>" = the optional all-subdistrict boundaries layer. A click
  # there may be in a different district, so that district is selected first
  # and the subdistrict is applied once the district has changed.
  pending_subdistrict <- reactiveVal(NULL)

  observeEvent(input$map_shape_click, {
    id <- as.character(input$map_shape_click$id %||% "")
    if (id == "") return()
    if (grepl("^sub(all)?_", id)) {
      sid <- as.integer(sub("^sub(all)?_", "", id))
      did <- subdistricts$district_id[sid]
      if (!identical(as.integer(selected_district() %||% -1), did)) {
        pending_subdistrict(sid)
        selected_district(as.character(did))
        updateSelectizeInput(session, "district_search", selected = did)
      } else {
        selected_subdistrict(sid)
        updateSelectizeInput(session, "subdistrict_search", selected = sid)
      }
    } else {
      selected_district(id)
      updateSelectizeInput(session, "district_search", selected = id)
    }
  })

  # ---- district selection: from search --------------------------------------
  observeEvent(input$district_search, {
    id <- input$district_search
    if (is.null(id) || id == "") { selected_district(NULL); return() }
    selected_district(id)
  }, ignoreInit = TRUE)

  # a new district clears any subdistrict filter (or applies a pending one)
  observeEvent(selected_district(), {
    selected_subdistrict(pending_subdistrict())
    pending_subdistrict(NULL)
  }, ignoreNULL = FALSE, priority = 10)

  # ---- reset: back to all of Indonesia --------------------------------------
  observeEvent(input$reset_district, {
    selected_district(NULL)
    updateSelectizeInput(session, "district_search", selected = "")
  })
  # zoom out whenever the district filter is cleared (reset link, x, or blank)
  observeEvent(selected_district(), {
    if (is.null(selected_district())) {
      leaflet::leafletProxy("map") |>
        leaflet::flyToBounds(district_bbox[["xmin"]], district_bbox[["ymin"]],
                             district_bbox[["xmax"]], district_bbox[["ymax"]])
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ---- optional: all subdistrict boundaries in the current view -------------
  # ~7,000 polygons are too heavy to send at once, so only those intersecting
  # the visible map are drawn, and only when zoomed in far enough.
  if (has_subdistricts) {
    sub_bbox <- do.call(rbind, lapply(sf::st_geometry(subdistricts),
                                      function(g) as.numeric(sf::st_bbox(g))))
    sub_drawn <- reactiveVal(integer(0))
    view <- debounce(reactive(list(b = input$map_bounds, z = input$map_zoom)), 400)

    observe({
      show <- isTRUE(input$show_sub_all)
      v <- view()
      proxy <- leaflet::leafletProxy("map")
      if (!show || is.null(v$b) || is.null(v$z) || v$z < 7) {
        if (length(isolate(sub_drawn()))) {
          proxy |> leaflet::clearGroup("sub_all")
          sub_drawn(integer(0))
        }
        return()
      }
      b <- v$b
      ids <- which(sub_bbox[, 1] <= b$east & sub_bbox[, 3] >= b$west &
                     sub_bbox[, 2] <= b$north & sub_bbox[, 4] >= b$south)
      ids <- head(ids, 2500)
      if (identical(ids, isolate(sub_drawn()))) return()
      sub_drawn(ids)
      sd <- subdistricts[ids, ]
      proxy |>
        leaflet::clearGroup("sub_all") |>
        leaflet::addPolygons(
          data = sd, group = "sub_all",
          layerId = paste0("suball_", sd$subdistrict_id),
          fill = TRUE, fillOpacity = 0,
          color = "#475569", weight = 0.7, opacity = 0.7,
          label = ~paste0(name_3, " (", name_2, ")"),
          highlightOptions = leaflet::highlightOptions(
            color = "#0f172a", weight = 2, fillOpacity = 0.08
          ),
          options = leaflet::pathOptions(pane = "subdistricts")
        )
    })
  }

  # ---- subdistrict selection: from dropdown / back link ---------------------
  observeEvent(input$subdistrict_search, {
    sid <- input$subdistrict_search
    selected_subdistrict(if (is.null(sid) || sid == "") NULL else as.integer(sid))
  }, ignoreInit = TRUE)

  observeEvent(input$sub_back, {
    selected_subdistrict(NULL)
    updateSelectizeInput(session, "subdistrict_search", selected = "")
  })

  # ---- highlight + zoom + popup for the selected district -------------------
  observeEvent(selected_district(), {
    id <- selected_district()
    proxy <- leaflet::leafletProxy("map")
    proxy |>
      leaflet::clearGroup("selected") |>
      leaflet::clearGroup("subdistricts") |>
      leaflet::clearPopups()
    if (is.null(id) || id == "") return()
    id <- as.integer(id)
    b <- district_bounds[[id]]  # xmin, ymin, xmax, ymax
    proxy <- proxy |>
      leaflet::addPolygons(
        data = districts_map[districts_map$district_id == id, ], group = "selected",
        fill = FALSE, color = "#dc2626", weight = 3, opacity = 1,
        options = leaflet::pathOptions(pane = "districts")
      ) |>
      leaflet::flyToBounds(b[1], b[2], b[3], b[4]) |>
      leaflet::addPopups(
        lng = (b[1] + b[3]) / 2, lat = (b[2] + b[4]) / 2,
        popup = district_popup(id, lang())
      )
    # draw the district's subdistricts as clickable outlines
    if (has_subdistricts) {
      rows <- subdistricts_by_district[[id]]
      if (length(rows)) {
        sd <- subdistricts[rows, ]
        proxy |> leaflet::addPolygons(
          data = sd, group = "subdistricts",
          layerId = paste0("sub_", sd$subdistrict_id),
          fill = TRUE, fillOpacity = 0,
          color = "#334155", weight = 0.8, opacity = 0.8, dashArray = "3,3",
          label = ~subdistrict_label,
          highlightOptions = leaflet::highlightOptions(
            color = "#0f172a", weight = 2, fillOpacity = 0.08, bringToFront = TRUE
          ),
          options = leaflet::pathOptions(pane = "subdistricts")
        )
      }
    }
  }, ignoreNULL = FALSE)

  # ---- highlight + zoom + popup for the selected subdistrict ----------------
  observeEvent(selected_subdistrict(), {
    proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("selected_sub")
    sid <- selected_subdistrict()
    if (is.null(sid)) {
      # subdistrict filter cleared: zoom back to the whole district
      id <- isolate(selected_district())
      if (!is.null(id) && id != "") {
        b <- district_bounds[[as.integer(id)]]
        proxy |> leaflet::flyToBounds(b[1], b[2], b[3], b[4])
      }
      return()
    }
    proxy |> leaflet::clearPopups()
    b <- as.numeric(sf::st_bbox(subdistricts[sid, ]))
    proxy |>
      leaflet::addPolygons(
        data = subdistricts[sid, ], group = "selected_sub",
        fill = FALSE, color = "#f97316", weight = 3, opacity = 1,
        options = leaflet::pathOptions(pane = "subdistricts", interactive = FALSE)
      ) |>
      leaflet::flyToBounds(b[1], b[2], b[3], b[4]) |>
      leaflet::addPopups(
        lng = (b[1] + b[3]) / 2, lat = (b[2] + b[4]) / 2,
        popup = subdistrict_popup(sid, lang())
      )
  }, ignoreNULL = FALSE)
}

shiny::shinyApp(ui, server)
