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
# =============================================================================

# note: there's a known bug with "show labels" option and "street map" base map
# but chatty g can't seem to work this one out and the open plan is TOO LOUD - LH come back here

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
    basemap_clean    = "Clean (no labels)",
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
      "transmission</strong> of <em>P. knowlesi</em> malaria. We have applied a mask ",
      "to remove predictions to areas outside of the ranges of non-human host and vector species. ",
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
    basemap_clean    = "Polos (tanpa label)",
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
      "bagi penularan</strong> malaria <em>P. knowlesi</em>. Kami menerapkan masker ",
      "untuk menghapus prediksi pada area di luar jangkauan spesies inang non-manusia dan vektor. ",
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

# Basemap providers keyed by choice value
basemap_providers <- list(
  clean  = leaflet::providers$CartoDB.Positron,
  street = leaflet::providers$OpenStreetMap.Mapnik,
  sat    = leaflet::providers$Esri.WorldImagery
)

# Use label-free variants below the raster where the provider offers one.
# Mapnik has no labels-only or no-label variant, so its original tiles remain
# below the raster while the closest transparent street labels are added above.
basemap_no_label_providers <- list(
  clean  = leaflet::providers$CartoDB.PositronNoLabels,
  street = leaflet::providers$OpenStreetMap.Mapnik,
  sat    = leaflet::providers$Esri.WorldImagery
)

label_providers <- list(
  clean  = leaflet::providers$CartoDB.PositronOnlyLabels,
  street = leaflet::providers$CartoDB.VoyagerOnlyLabels,
  sat    = leaflet::providers$CartoDB.VoyagerOnlyLabels
)

# -----------------------------------------------------------------------------
# 4. Map-building helpers
# -----------------------------------------------------------------------------
add_districts <- function(map) {
  leaflet::addPolygons(
    map,
    data = districts,
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
    group = "risk",
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

  leaflet::addControl(
    map,
    html = legend_html,
    position = "bottomright",
    layerId = "risk_legend",
    className = "risk-legend-control"
  )
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
    tr$uncertainty_lab, ": <b>", format(round(row$mean_uncertainty, 2), nsmall = 2), "</b>",
    "</div>"
  )
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
    radioButtons(
      "surface", label = tr$display,
      choices = setNames(c("mean", "uncertainty"),
                         c(tr$layer_mean, tr$layer_uncertainty)),
      selected = isolate(input$surface) %||% "mean"
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
        selected = isolate(input$basemap) %||% "clean"
      ),
      checkboxInput(
        "show_risk",
        if (is_id) "Tampilkan lapisan kecocokan penularan" else "Show transmission suitability layer",
        value = isolate(input$show_risk) %||% TRUE
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
    choices <- c(setNames("", ""), setNames(districts$district_id, districts$district_label))
    selectizeInput(
      "district_search", label = tr$search,
      choices = choices, selected = isolate(selected_district()) %||% "",
      options = list(placeholder = tr$search_ph, allowEmptyOption = TRUE)
    )
  })

  output$export_control <- renderUI({
    tr <- i18n[[lang()]]
    tagList(
      tags$div(class = "control-label", tr$export),
      downloadButton("dl_csv", tr$export_csv, class = "dl-btn btn-sm"),
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
      tags$div(class = "methods-body", tr$methods_html)
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
                 tags$span(class = "val", format(round(row$mean_uncertainty, 2), nsmall = 2)))
      ),
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
    op <- par(mar = c(4, 4, 1, 1), cex = 0.9)
    on.exit(par(op))
    histogram <- hist(vals, breaks = 20, plot = FALSE)
    risk_palette <- leaflet::colorNumeric(
      palette = map_palette,
      domain = surface_domains$mean,
      na.color = "transparent"
    )
    plot(
      histogram,
      col = risk_palette(histogram$mids),
      border = "#ffffff",
      main = "",
      xlab = tr$dist_x,
      ylab = tr$dist_y
    )
    m <- mean(vals, na.rm = TRUE)
    abline(v = m, col = "#b91c1c", lwd = 2)
    usr <- par("usr")
    text(m, usr[4] * 0.92, labels = tr$dist_meanline,
         col = "#b91c1c", pos = 4, cex = 0.85, offset = 0.3)
  })

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
      leaflet::addProviderTiles(
        basemap_providers[["clean"]],
        group = "basemap",
        options = leaflet::providerTileOptions(opacity = 0.9)
      ) |>
      leaflet::fitBounds(
        lng1 = district_bbox[["xmin"]], lat1 = district_bbox[["ymin"]],
        lng2 = district_bbox[["xmax"]], lat2 = district_bbox[["ymax"]]
      ) |>
      add_surface("mean", opacity = 0.82) |>
      add_surface_legend("mean", "en") |>
      add_districts()
    m
  })

  # ---- basemap and label-layer switch ---------------------------------------
  update_map_tiles <- function() {
    basemap <- input$basemap %||% "clean"
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
        leaflet::addProviderTiles(
          label_providers[[basemap]],
          group = "labels",
          options = leaflet::providerTileOptions(pane = "labels", zIndex = 416)
        )
    }
    proxy
  }

  # ---- redraw surface/legend on layer, opacity or language change -----------
  redraw_surface <- function() {
    surface <- input$surface %||% "mean"
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearImages() |>
      leaflet::removeControl("risk_legend")
    if (isTRUE(input$show_risk %||% TRUE)) {
      proxy <- proxy |>
        add_surface(surface, opacity = input$opacity %||% 0.82) |>
        add_surface_legend(surface, lang())
    }
    proxy
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
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!is.null(id)) {
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

  # ---- highlight + zoom + popup for the selected district -------------------
  observeEvent(selected_district(), {
    id <- selected_district()
    proxy <- leaflet::leafletProxy("map")
    proxy |> leaflet::clearGroup("selected") |> leaflet::clearPopups()
    if (is.null(id) || id == "") return()
    id <- as.integer(id)
    b <- district_bounds[[id]]  # xmin, ymin, xmax, ymax
    proxy |>
      leaflet::addPolygons(
        data = districts[id, ], group = "selected",
        fill = FALSE, color = "#dc2626", weight = 3, opacity = 1,
        options = leaflet::pathOptions(pane = "districts")
      ) |>
      leaflet::flyToBounds(b[1], b[2], b[3], b[4]) |>
      leaflet::addPopups(
        lng = (b[1] + b[3]) / 2, lat = (b[2] + b[4]) / 2,
        popup = district_popup(id, lang())
      )
  }, ignoreNULL = FALSE)
}

shiny::shinyApp(ui, server)
