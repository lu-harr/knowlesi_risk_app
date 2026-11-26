# =============================================================================
# make_appendix.R
# Builds the formatted Excel appendix of relative transmission suitability by
# district (kabupaten/kota) and subdistrict (kecamatan) for the Indonesian
# P. knowlesi surveillance guidelines.
#
#     Rscript make_appendix.R
#
# Uses exactly the same data and classification rules as app.R (it runs the
# app's data section), so the appendix always matches the app.
# Output: appendix/Pk_transmission_suitability_appendix_<date>.xlsx
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
})

# ---- load data + classification from app.R ----------------------------------
src <- readLines("app.R", warn = FALSE)
end <- grep("^# 3\\. Bilingual", src) - 2
suppressMessages(eval(parse(text = src[seq_len(end)])))
stopifnot(exists("district_stats"), has_subdistricts)

dir.create("appendix", showWarnings = FALSE)
out_file <- file.path("appendix", paste0("Pk_transmission_suitability_appendix_",
                                         format(Sys.Date(), "%Y%m%d"), ".xlsx"))

# ---- labels (English / Bahasa Indonesia) -------------------------------------
tier_txt <- c(high = "High / Tinggi", moderate = "Moderate / Sedang",
              low = "Low / Rendah", outside = "Outside model / Di luar model")
status_txt <- c(
  cat1    = "1 - Confirmed transmission / Penularan terkonfirmasi",
  cat2    = "2 - Probable, not confirmed / Probable, belum terkonfirmasi",
  cat3    = "3 - Receptive (map) / Reseptif (peta)",
  watch   = "Watch list / Daftar pantau",
  outside = "Outside model / Di luar model",
  none    = "Not listed / Tidak terdaftar"
)
# light fills that print well and keep black text readable
tier_fill <- c(high = "#F4A582", moderate = "#FDDBC7", low = "#F7F7F7", outside = "#D9D9D9")
basis_txt <- c(both = "MoH (SISMAL) + research / Kemenkes + penelitian",
               moh = "MoH (SISMAL) / Kemenkes", research = "Research PCR / Penelitian PCR",
               moh_probable = "MoH probable only / Hanya probable Kemenkes")
status_fill <- c(cat1 = "#D6604D", cat2 = "#F4A582", cat3 = "#C2A5CF",
                 watch = "#FFF2B3", outside = "#D9D9D9", none = "#FFFFFF")

na_if_outside <- function(x, outside) ifelse(outside, NA, x)

# ---- district table ----------------------------------------------------------
ds <- district_stats
ds_out <- ds$tier == "outside"
ds <- ds[order(ds$name_1, ds$name_2), ]
ds_out <- ds$tier == "outside"
dist_tab <- data.frame(
  "Province / Provinsi"                      = ds$name_1,
  "District / Kabupaten/Kota"                = ds$name_2,
  "Mean suitability / Rata-rata kecocokan"   = na_if_outside(ds$mean_risk, ds_out),
  "Uncertainty (variance) / Ketidakpastian"  = na_if_outside(ds$mean_uncertainty, ds_out),
  "Min"                                      = na_if_outside(ds$risk_min, ds_out),
  "Max"                                      = na_if_outside(ds$risk_max, ds_out),
  "Area >= 0.5 / Luas >= 0,5"                = na_if_outside(ds$share_high, ds_out),
  "Area >= 0.3 / Luas >= 0,3"                = na_if_outside(ds$share_moderate, ds_out),
  "Model coverage / Cakupan model"           = ds$model_coverage,
  "Map tier / Tingkat peta"                  = unname(tier_txt[ds$tier]),
  "Risk list category / Kategori daftar risiko" = unname(status_txt[ds$status]),
  "Evidence basis / Dasar bukti"             = ifelse(is.na(ds$evidence_basis), "", unname(basis_txt[ds$evidence_basis])),
  "Research sources / Sumber penelitian"     = ifelse(is.na(ds$research_sources), "", ds$research_sources),
  "Kecamatan (high) / Kecamatan (tinggi)"    = vapply(ds$district_id, function(id)
    sum(subdistrict_stats$district_id == id & subdistrict_stats$tier == "high"), 0),
  "Kecamatan (total)"                        = vapply(ds$district_id, function(id)
    sum(subdistrict_stats$district_id == id), 0),
  check.names = FALSE, stringsAsFactors = FALSE
)

# ---- subdistrict table -------------------------------------------------------
ss <- subdistrict_stats
ss <- ss[order(ss$name_1, ss$name_2, ss$name_3), ]
ss_out <- ss$tier == "outside"
sub_tab <- data.frame(
  "Province / Provinsi"                      = ss$name_1,
  "District / Kabupaten/Kota"                = ss$name_2,
  "Subdistrict / Kecamatan"                  = ss$name_3,
  "Mean suitability / Rata-rata kecocokan"   = na_if_outside(ss$mean_risk, ss_out),
  "Uncertainty (variance) / Ketidakpastian"  = na_if_outside(ss$mean_uncertainty, ss_out),
  "Min"                                      = na_if_outside(ss$risk_min, ss_out),
  "Max"                                      = na_if_outside(ss$risk_max, ss_out),
  "Model coverage / Cakupan model"           = ss$model_coverage,
  "Map tier / Tingkat peta"                  = unname(tier_txt[ss$tier]),
  "Risk list category / Kategori daftar risiko" = paste0(
    unname(status_txt[ss$status]),
    ifelse(ss$confirmed_here, " (confirmed here, research / terkonfirmasi di sini)",
           ifelse(ss$category_from_district, " (district / kab.)", ""))),
  check.names = FALSE, stringsAsFactors = FALSE
)

# ---- workbook ----------------------------------------------------------------
wb <- createWorkbook(creator = "Pk risk mapping app")
modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

st_title  <- createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#1F2933")
st_sub    <- createStyle(fontSize = 9, fontColour = "#52606D", wrapText = TRUE, valign = "top")
st_head   <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F3A5F", textDecoration = "bold",
                         wrapText = TRUE, valign = "center", halign = "center",
                         border = "TopBottomLeftRight", borderColour = "#FFFFFF")
st_num    <- createStyle(numFmt = "0.00", halign = "center")
st_pct    <- createStyle(numFmt = "0%", halign = "center")
st_int    <- createStyle(numFmt = "0", halign = "center")
st_text   <- createStyle(valign = "center")
st_na     <- createStyle(fontColour = "#7F7F7F", halign = "center")
st_band   <- createStyle(border = "bottom", borderColour = "#E5E7EB")

add_table_sheet <- function(name, tab, title, subtitle, num_cols, pct_cols, int_cols,
                            tier_col, status_col, mean_col, widths) {
  addWorksheet(wb, name, gridLines = FALSE)
  writeData(wb, name, title, startRow = 1, startCol = 1)
  addStyle(wb, name, st_title, rows = 1, cols = 1)
  writeData(wb, name, subtitle, startRow = 2, startCol = 1)
  mergeCells(wb, name, cols = 1:ncol(tab), rows = 2)
  addStyle(wb, name, st_sub, rows = 2, cols = 1)
  setRowHeights(wb, name, rows = 2, heights = 52)

  hdr <- 4; first <- hdr + 1; last <- hdr + nrow(tab); rows <- first:last
  writeData(wb, name, tab, startRow = hdr, startCol = 1, keepNA = TRUE, na.string = "–",
            headerStyle = st_head, withFilter = TRUE)
  setRowHeights(wb, name, rows = hdr, heights = 42)
  freezePane(wb, name, firstActiveRow = first, firstActiveCol = if (name == "Kecamatan") 4 else 3)

  addStyle(wb, name, st_text, rows = rows, cols = 1:ncol(tab), gridExpand = TRUE)
  addStyle(wb, name, st_band, rows = rows, cols = 1:ncol(tab), gridExpand = TRUE, stack = TRUE)
  for (c in num_cols) addStyle(wb, name, st_num, rows = rows, cols = c, stack = TRUE)
  for (c in pct_cols) addStyle(wb, name, st_pct, rows = rows, cols = c, stack = TRUE)
  for (c in int_cols) addStyle(wb, name, st_int, rows = rows, cols = c, stack = TRUE)
  # "–" for outside-model values in grey
  for (c in c(num_cols, pct_cols)) {
    na_rows <- rows[is.na(tab[[c]])]
    if (length(na_rows)) addStyle(wb, name, st_na, rows = na_rows, cols = c, stack = TRUE)
  }
  # colour scale on mean suitability (same 0-0.8 range as the map legend)
  conditionalFormatting(wb, name, cols = mean_col, rows = rows, type = "colourScale",
                        style = c("#FFFFFF", "#FDB863", "#B2182B"), rule = c(0, 0.4, 0.8))
  # category / tier fills
  for (k in names(tier_fill)) {
    r <- rows[tab[[tier_col]] == tier_txt[[k]]]
    if (length(r)) addStyle(wb, name, createStyle(fgFill = tier_fill[[k]]), rows = r,
                            cols = tier_col, stack = TRUE)
  }
  for (k in names(status_fill)) {
    r <- rows[startsWith(tab[[status_col]], status_txt[[k]])]
    if (length(r)) addStyle(wb, name, createStyle(fgFill = status_fill[[k]]), rows = r,
                            cols = status_col, stack = TRUE)
  }
  setColWidths(wb, name, cols = seq_along(widths), widths = widths)
  pageSetup(wb, name, orientation = "landscape", fitToWidth = TRUE, fitToHeight = FALSE,
            printTitleRows = hdr, paperSize = 9)
}

subtitle_common <- paste0(
  "Mean predicted relative transmission suitability (0-1, not a probability) from Tobin et al. 2024, ",
  "PLOS NTD (doi:10.1371/journal.pntd.0011570), ~5 x 5 km grid averaged within each boundary. ",
  "– = outside the model extent (no prediction; NOT low risk): predictions were restricted to areas within the range of macaque and mosquito species required for zoonotic transmission (overlap of at least one reservoir and one vector species range map). ",
  "Map tier rule: high if >= 25% of area has suitability >= 0.5; moderate if >= 25% has >= 0.3. ",
  "Risk list category: evidence (SOP Table 4) first, map adds receptive areas. Generated ",
  format(Sys.Date(), "%d %b %Y"), ".")

add_table_sheet(
  "Kabupaten-Kota", dist_tab,
  "Appendix: P. knowlesi transmission suitability by district / Lampiran: kecocokan penularan P. knowlesi per kabupaten/kota",
  subtitle_common, num_cols = 3:6, pct_cols = 7:9, int_cols = 14:15,
  tier_col = 10, status_col = 11, mean_col = 3,
  widths = c(22, 24, 13, 13, 8, 8, 11, 11, 11, 20, 44, 26, 30, 12, 11))

add_table_sheet(
  "Kecamatan", sub_tab,
  "Appendix: P. knowlesi transmission suitability by subdistrict / Lampiran: kecocokan penularan P. knowlesi per kecamatan",
  paste(subtitle_common, "Kecamatan values are area-weighted; categories marked (district / kab.) come from the district's evidence."),
  num_cols = 4:7, pct_cols = 8, int_cols = integer(0),
  tier_col = 9, status_col = 10, mean_col = 4,
  widths = c(20, 22, 24, 13, 13, 8, 8, 11, 20, 52))

# ---- notes sheet -------------------------------------------------------------
addWorksheet(wb, "Notes - Keterangan", gridLines = FALSE)
notes <- data.frame(
  Item = c("Source / Sumber", "Measure / Ukuran", "Uncertainty / Ketidakpastian",
           "Outside model / Di luar model", "Map tier / Tingkat peta",
           "Risk list category / Kategori daftar risiko", "Evidence data / Data bukti",
           "Boundaries / Batas wilayah", "Districts / Kab-Kota", "Subdistricts / Kecamatan",
           "Confirmed districts recovered by map rule"),
  Detail = c(
    "Tobin RJ et al. (2024) PLOS Negl Trop Dis. doi:10.1371/journal.pntd.0011570. Surfaces: osf.io/k5bsa",
    "Mean predicted relative transmission suitability (0-1). Relative, not a probability or incidence. / Rata-rata perkiraan kecocokan penularan relatif (0-1), bukan probabilitas atau insidens.",
    "Mean variance of bootstrapped predictions; higher = less certain. / Rata-rata varians prediksi bootstrap; makin tinggi = makin tidak pasti.",
    "The model predictions were restricted to areas within the range of macaque and mosquito species known to be required for zoonotic transmission of P. knowlesi (i.e. the overlap in range maps of at least one reservoir and one vector species). Areas outside this have no prediction and are shown as –. This is NOT low risk; use case data and local evidence. / Prediksi model dibatasi pada wilayah dalam jangkauan spesies kera dan nyamuk yang diperlukan untuk penularan zoonosis P. knowlesi (tumpang tindih peta jangkauan sekurang-kurangnya satu spesies reservoir dan satu spesies vektor). Wilayah di luar itu tidak memiliki prediksi (–); BUKAN berarti risiko rendah.",
    "High: >= 25% of area with suitability >= 0.5. Moderate: >= 25% of area >= 0.3. Low: otherwise. / Tinggi: >= 25% luas dengan kecocokan >= 0,5. Sedang: >= 25% luas >= 0,3.",
    "1 = confirmed transmission; 2 = probable, not confirmed (case evidence, draft SOP Table 4); 3 = receptive (map tier high); Watch list = map tier moderate. Evidence always overrides the map.",
    "MoH: draft Indonesian Pk surveillance SOP v5 (SISMAL 2018-2026 table and research sites). Research: published PCR-confirmed human cases (data/published_pk_records.csv; 2010-2025 papers), matched to district/kecamatan. Province-only reports (e.g. Fauziah et al. 2025) do not change districts. Extracted with AI assistance; to be verified against the papers and validated by Kemenkes.",
    "malariaAtlas admin-2 / admin-3 boundaries (version 202403).",
    as.character(nrow(dist_tab)),
    as.character(nrow(sub_tab)),
    sprintf("%d of %d confirmed districts rated high, %d moderate, %d low, %d outside model.",
            calibration[["high"]], length(cat1_rows), calibration[["moderate"]],
            calibration[["low"]], calibration[["outside"]])
  ),
  stringsAsFactors = FALSE
)
writeData(wb, "Notes - Keterangan", "Notes / Keterangan", startRow = 1)
addStyle(wb, "Notes - Keterangan", st_title, rows = 1, cols = 1)
writeData(wb, "Notes - Keterangan", notes, startRow = 3, headerStyle = st_head)
addStyle(wb, "Notes - Keterangan", createStyle(wrapText = TRUE, valign = "top"),
         rows = 4:(3 + nrow(notes)), cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Notes - Keterangan", createStyle(textDecoration = "bold", wrapText = TRUE, valign = "top"),
         rows = 4:(3 + nrow(notes)), cols = 1, stack = TRUE)
setColWidths(wb, "Notes - Keterangan", cols = 1:2, widths = c(32, 110))

# colour key
key_row <- 6 + nrow(notes)
writeData(wb, "Notes - Keterangan", "Colour key / Kunci warna", startRow = key_row)
addStyle(wb, "Notes - Keterangan", createStyle(textDecoration = "bold"), rows = key_row, cols = 1)
keys <- c(status_txt, tier_txt)
fills <- c(status_fill, tier_fill)
for (i in seq_along(keys)) {
  writeData(wb, "Notes - Keterangan", keys[[i]], startRow = key_row + i, startCol = 1)
  addStyle(wb, "Notes - Keterangan", createStyle(fgFill = fills[[i]], border = "TopBottomLeftRight",
                                                  borderColour = "#BFBFBF"),
           rows = key_row + i, cols = 1)
}
writeData(wb, "Notes - Keterangan",
          "Mean suitability column: white (0) to orange (0.4) to red (>= 0.8).",
          startRow = key_row + length(keys) + 1, startCol = 1)

worksheetOrder(wb) <- c(3, 1, 2)
saveWorkbook(wb, out_file, overwrite = TRUE)
message("Wrote ", out_file, " (", nrow(dist_tab), " districts, ", nrow(sub_tab), " subdistricts)")
