library(shiny)
library(leaflet)
library(terra)
library(sf)

message("test")

risk_path <- "data/risk_masked.tif"
districts_path <- "data/idn_districts.shp"

required_files <- c(risk_path, districts_path)
missing_files <- required_files[!file.exists(required_files)]
# if (length(missing_files) > 0) {
#   stop(
#     "Missing required data file(s): ",
#     paste(missing_files, collapse = ", "),
#     call. = FALSE
#   )
# }

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
district_bbox <- sf::st_bbox(districts)

surface_layers <- list(
  mean = risk[["prediction_mean"]],
  uncertainty = risk[["prediction_variance"]]
)

surface_domains <- lapply(surface_layers, function(x) {
  domain <- as.numeric(terra::global(x, range, na.rm = TRUE)[1, ])
  names(domain) <- c("min", "max")
  domain
})

# Leaflet projects rasters to Web Mercator before display. Doing it once at
# startup keeps switching between layers smooth.
surface_layers <- lapply(
  surface_layers,
  leaflet::projectRasterForLeaflet,
  method = "bilinear"
)

map_palette <- viridisLite::viridis(256)

surface_meta <- list(
  mean = list(
    label = "Mean risk",
    legend_title = "Mean predicted relative risk",
    palette = map_palette,
    legend_breaks = seq(0, 0.8, by = 0.2)
  ),
  uncertainty = list(
    label = "Risk uncertainty",
    legend_title = "Prediction variance",
    palette = map_palette,
    legend_breaks = seq(0, 3, by = 1)
  )
)

add_districts <- function(map) {
  leaflet::addPolygons(
    map,
    data = districts,
    group = "Districts",
    fill = FALSE,
    color = "#1f2933",
    opacity = 0.9,
    weight = 1,
    smoothFactor = 0.4,
    label = ~paste(name_2, name_1, sep = ", "),
    highlightOptions = leaflet::highlightOptions(
      color = "#0f172a",
      weight = 2,
      bringToFront = TRUE
    ),
    options = leaflet::pathOptions(pane = "districts")
  )
}

add_surface <- function(map, surface) {
  meta <- surface_meta[[surface]]
  domain <- surface_domains[[surface]]
  palette <- leaflet::colorNumeric(
    palette = meta$palette,
    domain = domain,
    na.color = "transparent"
  )

  map |>
    leaflet::addRasterImage(
      x = surface_layers[[surface]],
      colors = palette,
      opacity = 0.82,
      group = meta$label,
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

add_surface_legend <- function(map, surface) {
  meta <- surface_meta[[surface]]
  legend_labels <- format_legend_labels(meta$legend_breaks)
  legend_gradient <- paste(
    rev(meta$palette),
    collapse = ", "
  )

  legend_html <- tags$div(
    tags$div(class = "legend-title", meta$legend_title),
    tags$div(
      class = "legend-scale",
      tags$div(
        class = "legend-gradient",
        style = paste0(
          "background: linear-gradient(to bottom, ",
          legend_gradient,
          ");"
        )
      ),
      tags$div(
        class = "legend-labels",
        lapply(rev(legend_labels), tags$span)
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

ui <- shiny::fluidPage(
  tags$head(
    tags$title("Plasmodium knowlesi transmission suitability"),
    tags$style(
      HTML(
        "
        html, body, .container-fluid {
          height: 100%;
          margin: 0;
          overflow: hidden;
        }

        .container-fluid {
          padding: 0;
        }

        .author-summary-overlay {
          position: fixed;
          inset: 0;
          z-index: 2000;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 24px;
          background: rgba(15, 23, 42, 0.56);
        }

        .author-summary-dialog {
          width: min(640px, 100%);
          max-height: calc(100vh - 48px);
          overflow-y: auto;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: 0 24px 60px rgba(15, 23, 42, 0.3);
          color: #17202a;
        }

        .author-summary-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 16px;
          padding: 22px 24px 16px;
          border-bottom: 1px solid #e2e8f0;
        }

        .author-summary-header h2 {
          margin: 0;
          font-size: 22px;
          font-weight: 700;
          line-height: 1.25;
        }

        .author-summary-body {
          padding: 20px 24px;
          font-size: 16px;
          line-height: 1.65;
        }

        .author-summary-body p {
          margin: 0;
        }

        .author-summary-body p + p {
          margin-top: 1em;
        }

        .author-summary-footer {
          display: flex;
          justify-content: flex-end;
          padding: 0 24px 22px;
        }

        .author-summary-dismiss {
          padding: 10px 18px;
          border: 0;
          border-radius: 5px;
          background: #2563eb;
          color: #ffffff;
          font-size: 15px;
          font-weight: 600;
        }

        .author-summary-dismiss:hover,
        .author-summary-dismiss:focus {
          background: #1d4ed8;
          outline: 3px solid rgba(37, 99, 235, 0.25);
          outline-offset: 2px;
        }

        .app-shell {
          display: grid;
          grid-template-columns: 300px minmax(0, 1fr);
          height: 100vh;
          background: #f8fafc;
        }

        .sidebar {
          display: flex;
          flex-direction: column;
          gap: 22px;
          padding: 24px 22px;
          border-right: 1px solid #d7dee8;
          background: #ffffff;
          color: #17202a;
          z-index: 500;
        }

        .sidebar h1 {
          margin: 0;
          font-size: 22px;
          font-weight: 700;
          line-height: 1.2;
        }

        .sidebar .subtitle {
          margin: -14px 0 0;
          color: #52606d;
          font-size: 13px;
          line-height: 1.45;
        }

        .control-label {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0;
          margin-bottom: 10px;
          text-transform: uppercase;
          color: #52606d;
        }

        .radio {
          margin: 0 0 10px;
        }

        .radio label {
          display: flex;
          align-items: center;
          gap: 8px;
          min-height: 28px;
          font-size: 15px;
          color: #17202a;
        }

        .radio input {
          margin: 0;
        }

        .map-wrap,
        #map {
          min-width: 0;
          height: 100%;
        }

        .leaflet-control-attribution,
        .leaflet-control-layers,
        .leaflet-control-scale,
        .leaflet-control {
          font-size: 11px;
        }

        .leaflet-control .legend {
          line-height: 1.3;
        }

        .risk-legend-control {
          padding: 8px 10px;
          background: rgba(255, 255, 255, 0.94);
          border: 1px solid rgba(15, 23, 42, 0.12);
          border-radius: 4px;
          box-shadow: 0 1px 5px rgba(15, 23, 42, 0.22);
          color: #17202a;
        }

        .risk-legend-control .legend-title {
          margin-bottom: 6px;
          font-size: 12px;
          font-weight: 700;
          line-height: 1.25;
        }

        .risk-legend-control .legend-scale {
          display: grid;
          grid-template-columns: 18px auto;
          column-gap: 8px;
          align-items: stretch;
        }

        .risk-legend-control .legend-gradient {
          width: 18px;
          height: 96px;
          border: 1px solid rgba(15, 23, 42, 0.18);
        }

        .risk-legend-control .legend-labels {
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          height: 96px;
          font-size: 11px;
          line-height: 1;
          white-space: nowrap;
        }

        @media (max-width: 760px) {
          .author-summary-overlay {
            padding: 14px;
          }

          .author-summary-dialog {
            max-height: calc(100vh - 28px);
          }

          .author-summary-header,
          .author-summary-body {
            padding-left: 18px;
            padding-right: 18px;
          }

          .author-summary-body {
            font-size: 15px;
          }

          .author-summary-footer {
            padding-right: 18px;
            padding-bottom: 18px;
          }

          .app-shell {
            grid-template-columns: 1fr;
            grid-template-rows: auto minmax(0, 1fr);
          }

          .sidebar {
            gap: 12px;
            padding: 14px 16px;
            border-right: 0;
            border-bottom: 1px solid #d7dee8;
          }

          .sidebar h1 {
            font-size: 18px;
          }

          .sidebar .subtitle {
            margin: -4px 0 0;
          }

          .shiny-options-group {
            display: flex;
            flex-wrap: wrap;
            gap: 10px 18px;
          }

          .radio {
            margin: 0;
          }
        }
        "
      )
    )
  ),
  tags$script(
    HTML(
      "
      function dismissAuthorSummary() {
        var modal = document.getElementById('author-summary-modal');
        if (modal) {
          modal.style.display = 'none';
        }
      }

      document.addEventListener('keydown', function(event) {
        if (event.key === 'Escape') {
          dismissAuthorSummary();
        }
      });
      "
    )
  ),
  div(
    id = "author-summary-modal",
    class = "author-summary-overlay",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "author-summary-title",
    div(
      class = "author-summary-dialog",
      div(
        class = "author-summary-header",
        h2(id = "author-summary-title", "Author summary")
      ),
      div(
        class = "author-summary-body",
        
        p(
          "To predict spatial variation in ", tags$em("P. knowlesi"), " transmission risk, 
          we built a ", tags$strong("boosted regression tree model"), " to learn associations between 
          remotely-sensed environmental data, including ", tags$strong("elevation"), ", ", tags$strong("land cover"), ", 
          and annually-varying ", tags$strong("forest disturbance"), ", ", 
          tags$strong("temperature"), " and ", tags$strong("wetness"), " indices, 
          and locations where ", tags$em("P. knowlesi"), " malaria was identified."
        ),
        p(
          "Our predictions are therefore of ", tags$strong("environmental suitability for transmission"), " 
          of ", tags$em("P. knowlesi"), " malaria. We have applied a mask to remove areas outside of the 
          ranges of non-human host and vector species. The uncertainty surface describes 
          ", tags$strong("variance in bootstrapped predictions"), " from the model: we fitted the model to 
          slightly different samples of the ", tags$em("P. knowlesi"), " malaria infection dataset to 
          evaluate model sensitivity to environmentally-outlying infections."
        ),
        p(
          "Read more about the model in Tobin et al., 2024:", 
          HTML('<a href="https://doi.org/10.1371/journal.pntd.0011570">https://doi.org/10.1371/journal.pntd.0011570</a>')
        )
      ),
      div(
        class = "author-summary-footer",
        tags$button(
          type = "button",
          class = "author-summary-dismiss",
          onclick = "dismissAuthorSummary()",
          autofocus = NA,
          "Enter app"
        )
      )
    )
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      h1(tags$em("Plasmodium knowlesi"), " risk"),
      div(class = "subtitle", "Tobin et al., 2024, PLOS Negl. Trop. Dis."),
      radioButtons(
        inputId = "surface",
        label = "Display",
        choices = c(
          "Mean predicted transmission suitability" = "mean",
          "Prediction variance" = "uncertainty"
        ),
        selected = "mean"
      )
    ),
    div(class = "map-wrap", leafletOutput("map", height = "100%"))
  )
)

server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      leaflet::addMapPane("risk", zIndex = 410) |>
      leaflet::addMapPane("districts", zIndex = 420) |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = leaflet::providerTileOptions(opacity = 0.75)
      ) |>
      leaflet::fitBounds(
        lng1 = district_bbox[["xmin"]],
        lat1 = district_bbox[["ymin"]],
        lng2 = district_bbox[["xmax"]],
        lat2 = district_bbox[["ymax"]]
      ) |>
      add_surface("mean") |>
      add_surface_legend("mean") |>
      add_districts()
  })

  observeEvent(input$surface, {
    leaflet::leafletProxy("map") |>
      leaflet::clearImages() |>
      leaflet::removeControl("risk_legend") |>
      leaflet::clearShapes() |>
      add_surface(input$surface) |>
      add_surface_legend(input$surface) |>
      add_districts()
  }, ignoreInit = TRUE)
}

shiny::shinyApp(ui, server)
