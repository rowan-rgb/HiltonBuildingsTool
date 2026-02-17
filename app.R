# ------------------------------------------------------------
# app.R — Open Buildings Temporal V1 — Single ROI
#
# Adds UMN Functional Areas button + click-to-select (like Wards)
# - Toggle "Wards" (existing)
# - Toggle "UMN Areas" (NEW)
# - Toggle "Draw" (existing)
#
# Notes:
# - You provided a .shx path; sf::st_read needs the .shp.
#   This code automatically swaps .shx -> .shp if needed.
# ------------------------------------------------------------

# Sys.setenv(
#   RETICULATE_PYTHON = "/Users/rowandavies/Library/Caches/org.R-project.R/R/reticulate/uv/cache/archive-v0/QuaHw5TiV-JHOMAB6Uq3P/bin/python"
# )

# --- Python / Earth Engine setup (Reticulate + rgee) ---
library(reticulate)
library(rgee)

# Create the env only if it doesn't exist yet
if (!virtualenv_exists("r-reticulate")) {
  virtualenv_create("r-reticulate")
}

# Force reticulate to use this env for the session
use_virtualenv("r-reticulate", required = TRUE)

# Install Earth Engine API only if missing
if (!py_module_available("ee")) {
  py_install("earthengine-api", pip = TRUE)
}

# Confirm what Python reticulate is using + confirm ee import
py_config()
py_run_string("import ee; print('ee version:', ee.__version__)")

# Initialise Earth Engine (will prompt auth if not already authenticated)
ee_Initialize(project = "ccee-486908")

# --- R packages for the Shiny app ---
library(shiny)
# (then your other libraries: leaflet, sf, plotly, etc.)





library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)
library(geojsonsf)
library(htmlwidgets)
library(plotly)

temporal_col <- ee$ImageCollection("GOOGLE/Research/open-buildings-temporal/v1")
v3_fc <- ee$FeatureCollection("GOOGLE/Research/open-buildings/v3/polygons")

YEARS <- 2016:2023
FIRST_YEAR <- 2016
LAST_YEAR  <- 2023
PRES_THRESH <- 0.5
SCALE_M <- 10

# ---- local shapefiles ----
wards_path <- "data/wards/Municipal_Wards_2021.shp"
UMN_function_areas_path <- "data/umn/Analysis regions/UMN_Functional_Areas_1.shp"

ui <- fluidPage(
  titlePanel("Open Buildings Temporal V1 — Single ROI"),
  tags$head(
    tags$style(HTML("
      .leaflet-draw { display: none; }
      .rangeRow { display:flex; align-items:center; justify-content:space-between; gap:12px; }
      .rangeRight { text-align:right; white-space:nowrap; }
      .smallHelp { font-size: 12px; color:#666; margin-top:4px; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleDrawControls', function(msg) {
        var el = document.querySelector('.leaflet-draw');
        if (!el) return;
        el.style.display = msg.show ? 'block' : 'none';
      });
    "))
  ),
  
  fluidRow(
    column(
      width = 8,
      tags$div(
        style = "background:#f5f5f5; padding:12px; border-radius:6px; margin-bottom:10px;",
        h4("Instructions"),
        tags$ol(
          tags$li("Choose an area for analysis: Draw ROI, Wards, or UMN Areas."),
          tags$li(paste0("Histogram shows Temporal V1 ", LAST_YEAR, " polygon sizes (m²).")),
          tags$li("Slider filters ALL overlays live; graphs reflect only the selected size range."),
          tags$li("Graphs compute automatically when you select/draw an ROI.")
        )
      ),
      tags$div(
        style = "display:flex; gap:10px; margin-bottom:10px;",
        actionButton("toggle_wards", "Wards"),
        actionButton("toggle_umn", "UMN Areas"),
        actionButton("toggle_draw", "Draw")
      ),
      leafletOutput("map", height = 700)
    ),
    
    column(
      width = 4,
      tags$div(
        style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin-bottom:6px;",
        strong(textOutput("roi_area_m2")),
        downloadButton("download_csv", "Export CSV")
      ),
      
      tags$div(
        style = "display:flex; gap:10px; align-items:flex-end; margin-bottom:6px;",
        sliderInput("hist_bins", "Histogram bins", min = 20, max = 250, value = 120, step = 5, width = "100%")
      ),
      
      plotlyOutput("size_hist", height = 260),
      uiOutput("size_slider_ui"),
      br(),
      
      # Auto-updating (computed when ROI is chosen)
      plotOutput("ts_plot", height = 420),
      
      br(),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  
  # Safe fallback (replaces %||%)
  coalesce <- function(x, fallback) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) fallback else x
  }
  
  # ---------------------------
  # Map
  # ---------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      setView(lng = 30.31, lat = -29.56, zoom = 12) %>%
      addDrawToolbar(
        targetGroup = "draw",
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = drawRectangleOptions(),
        circleOptions = FALSE,
        markerOptions = FALSE,
        polylineOptions = FALSE,
        editOptions = editToolbarOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("OSM", "Esri Imagery"),
        overlayGroups = c(
          "ROI",
          "Wards",
          "UMN Areas",
          paste0("Buildings ", FIRST_YEAR, " (Temporal)"),
          paste0("Buildings ", LAST_YEAR,  " (Temporal)"),
          "Buildings V3 (polygons)"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map.on(L.Draw.Event.CREATED, function(e) { map.removeLayer(e.layer); });
          map.on(L.Draw.Event.EDITED, function(e) {
            e.layers.eachLayer(function(layer){ map.removeLayer(layer); });
          });
        }
      ")
  })
  
  observe({
    session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
  })
  
  # ---------------------------
  # Status
  # ---------------------------
  status <- reactiveVal("Choose a mode: Draw ROI, Wards, or UMN Areas…")
  output$status <- renderText(status())
  
  # ---------------------------
  # State
  # ---------------------------
  rv <- reactiveValues(
    roi_sf = NULL,
    roi_area_m2 = NULL,
    roi_geojson = NULL,
    poly_first = NULL,
    poly_last = NULL,
    v3_sf = NULL,
    v3_count = NA_real_,
    poly_last_areas_m2 = NULL,
    year_area_list = NULL
  )
  
  # interaction modes
  wards_on <- reactiveVal(FALSE)
  umn_on <- reactiveVal(FALSE)
  draw_on <- reactiveVal(FALSE)
  
  # ---------------------------
  # Load Wards
  # ---------------------------
  wards_sf <- tryCatch({
    w <- sf::st_read(wards_path, quiet = TRUE)
    w <- sf::st_zm(w, drop = TRUE, what = "ZM")
    w <- sf::st_make_valid(w)
    w <- sf::st_cast(w, "MULTIPOLYGON", warn = FALSE)
    sf::st_transform(w, 4326)
  }, error = function(e) {
    message("ERROR reading wards shapefile: ", conditionMessage(e))
    NULL
  })
  
  ward_id_col <- NULL
  if (!is.null(wards_sf) && nrow(wards_sf) > 0) {
    candidates <- c("WARD", "WARD_NO", "WARDNO", "WARD_NUM", "WARD_ID", "ID", "Name", "NAME")
    ward_id_col <- candidates[candidates %in% names(wards_sf)][1]
    if (is.na(ward_id_col) || is.null(ward_id_col)) {
      wards_sf$WARD_ID <- seq_len(nrow(wards_sf))
      ward_id_col <- "WARD_ID"
    }
    wards_sf$WARD_ID_LEAFLET <- as.character(wards_sf[[ward_id_col]])
  }
  
  # ---------------------------
  # Load UMN functional areas
  # ---------------------------
  umn_path_use <- UMN_function_areas_path
  if (grepl("\\.shx$", umn_path_use, ignore.case = TRUE)) {
    umn_path_use <- sub("\\.shx$", ".shp", umn_path_use, ignore.case = TRUE)
  }
  
  umn_sf <- tryCatch({
    u <- sf::st_read(umn_path_use, quiet = TRUE)
    u <- sf::st_zm(u, drop = TRUE, what = "ZM")
    u <- sf::st_make_valid(u)
    u <- sf::st_cast(u, "MULTIPOLYGON", warn = FALSE)
    sf::st_transform(u, 4326)
  }, error = function(e) {
    message("ERROR reading UMN areas shapefile: ", conditionMessage(e))
    NULL
  })
  
  umn_id_col <- NULL
  if (!is.null(umn_sf) && nrow(umn_sf) > 0) {
    candidates <- c("AREA", "AREA_ID", "ID", "NAME", "Name", "REGION", "REGION_ID",
                    "ZONE", "ZONE_ID", "FUNCTION", "FUNC_AREA")
    umn_id_col <- candidates[candidates %in% names(umn_sf)][1]
    if (is.na(umn_id_col) || is.null(umn_id_col)) {
      umn_sf$UMN_ID <- seq_len(nrow(umn_sf))
      umn_id_col <- "UMN_ID"
    }
    umn_sf$UMN_ID_LEAFLET <- as.character(umn_sf[[umn_id_col]])
  }
  
  # ---------------------------
  # Helpers
  # ---------------------------
  fmt_int <- function(x) {
    if (is.null(x) || length(x) == 0) return("—")
    x <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(x)) return("—")
    format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  fmt_num0 <- function(x) {
    if (!is.finite(x)) return("—")
    format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  feature_to_sf <- function(feature) {
    gj <- jsonlite::toJSON(feature, auto_unbox = TRUE)
    roi_sf <- geojsonsf::geojson_sf(gj)
    sf::st_as_sf(sf::st_set_crs(roi_sf, 4326))
  }
  
  prep_roi_sf <- function(x, simplify_m = 5) {
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    x <- sf::st_make_valid(x)
    g <- sf::st_union(x)
    g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
    if (is.numeric(simplify_m) && simplify_m > 0) {
      g_m <- sf::st_transform(g, 3857)
      g_m <- sf::st_simplify(g_m, dTolerance = simplify_m, preserveTopology = TRUE)
      g <- sf::st_transform(g_m, 4326)
    }
    sf::st_as_sf(sf::st_sfc(g, crs = 4326))
  }
  
  clear_map_overlays <- function() {
    leafletProxy("map") %>%
      clearGroup("ROI") %>%
      clearGroup("Wards") %>%
      clearGroup("UMN Areas") %>%
      clearGroup(paste0("Buildings ", FIRST_YEAR, " (Temporal)")) %>%
      clearGroup(paste0("Buildings ", LAST_YEAR,  " (Temporal)")) %>%
      clearGroup("Buildings V3 (polygons)")
  }
  
  draw_roi <- function(roi_sf) {
    leafletProxy("map") %>%
      clearGroup("ROI") %>%
      addPolygons(data = roi_sf, group = "ROI", color = "black", weight = 3, fillOpacity = 0.12)
  }
  
  draw_polys <- function(poly_sf, group, color, fillOpacity, weight, fillColor = NULL) {
    leafletProxy("map") %>% clearGroup(group)
    if (is.null(poly_sf) || nrow(poly_sf) == 0) return()
    leafletProxy("map") %>%
      addPolygons(
        data = poly_sf,
        group = group,
        color = color,
        weight = weight,
        fillColor = if (is.null(fillColor)) color else fillColor,
        fillOpacity = fillOpacity
      )
  }
  
  # selection draws into the SAME group (no "selected only" groups)
  draw_wards_all <- function() {
    leafletProxy("map") %>% clearGroup("Wards")
    if (is.null(wards_sf) || nrow(wards_sf) == 0) return()
    leafletProxy("map") %>%
      addPolygons(
        data = wards_sf,
        group = "Wards",
        layerId = ~WARD_ID_LEAFLET,
        color = "#FF7A00", weight = 3, opacity = 0.9,
        fillColor = "#FF7A00", fillOpacity = 0.12,
        label = ~paste0("Ward: ", WARD_ID_LEAFLET)
      )
  }
  
  draw_umn_all <- function() {
    leafletProxy("map") %>% clearGroup("UMN Areas")
    if (is.null(umn_sf) || nrow(umn_sf) == 0) return()
    leafletProxy("map") %>%
      addPolygons(
        data = umn_sf,
        group = "UMN Areas",
        layerId = ~UMN_ID_LEAFLET,
        color = "#7B61FF", weight = 3, opacity = 0.9,
        fillColor = "#7B61FF", fillOpacity = 0.12,
        label = ~paste0("UMN: ", UMN_ID_LEAFLET)
      )
  }
  
  add_area_m2 <- function(sfobj) {
    if (is.null(sfobj) || nrow(sfobj) == 0) return(sfobj)
    if ("area_m2" %in% names(sfobj)) return(sfobj)
    m <- tryCatch(sf::st_transform(sfobj, 3857), error = function(e) NULL)
    if (is.null(m)) {
      sfobj$area_m2 <- NA_real_
      return(sfobj)
    }
    sfobj$area_m2 <- as.numeric(sf::st_area(m))
    sfobj
  }
  
  filter_by_range <- function(sfobj, rng) {
    if (is.null(sfobj) || nrow(sfobj) == 0) return(sfobj)
    sfobj <- add_area_m2(sfobj)
    if (is.null(rng) || length(rng) != 2) return(sfobj)
    keep <- is.finite(sfobj$area_m2) & sfobj$area_m2 >= rng[1] & sfobj$area_m2 <= rng[2]
    sfobj[keep, ]
  }
  
  clamp <- function(x, lo, hi) {
    x <- as.numeric(x)
    if (!is.finite(x)) return(lo)
    max(lo, min(hi, x))
  }
  
  # ---------------------------
  # Earth Engine helpers
  # ---------------------------
  epoch_s_for_year <- function(year) {
    as.numeric(as.POSIXct(sprintf("%d-06-30 00:00:00", year), tz = "America/Los_Angeles")) %/% 1
  }
  
  year_mosaic <- function(year) {
    epoch_s <- epoch_s_for_year(year)
    temporal_col$filter(ee$Filter$eq("inference_time_epoch_s", epoch_s))$mosaic()
  }
  
  compute_presence_polygons <- function(roi_sf, year, thresh) {
    roi_ee <- sf_as_ee(roi_sf)$geometry()
    mosaic_img <- year_mosaic(year)
    pres <- mosaic_img$select("building_presence")$unmask(0)
    mask <- pres$gt(thresh)$selfMask()
    vec <- tryCatch(
      mask$reduceToVectors(
        geometry = roi_ee, scale = 4,
        geometryType = "polygon",
        eightConnected = TRUE,
        labelProperty = "mask",
        maxPixels = 1e13,
        tileScale = 4,
        bestEffort = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(vec)) return(NULL)
    vec_limited <- vec$limit(5000)
    tryCatch(ee_as_sf(vec_limited), error = function(e) NULL)
  }
  
  compute_v3_in_roi <- function(roi_sf) {
    roi_ee <- sf_as_ee(roi_sf)$geometry()
    fc_roi <- v3_fc$filterBounds(roi_ee)
    n <- tryCatch(fc_roi$size()$getInfo(), error = function(e) NA_real_)
    fc_disp <- fc_roi$limit(5000)
    v3_sf2 <- tryCatch(ee_as_sf(fc_disp), error = function(e) NULL)
    list(count = n, sf = v3_sf2)
  }
  
  # ---------------------------
  # Buildings in range + slider UI (+ min-size text input)
  # ---------------------------
  buildings_in_range <- reactive({
    a <- rv$poly_last_areas_m2
    rng <- input$size_range
    if (is.null(a) || length(a) == 0) return(0L)
    if (is.null(rng) || length(rng) != 2) return(length(a))
    sum(is.finite(a) & a >= rng[1] & a <= rng[2])
  })
  
  output$buildings_in_range_txt <- renderText(fmt_num0(buildings_in_range()))
  
  output$size_slider_ui <- renderUI({
    a <- rv$poly_last_areas_m2
    if (is.null(a) || length(a) == 0) return(NULL)
    
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) return(NULL)
    
    min_a <- floor(min(a))
    max_a <- ceiling(max(a))
    
    cur <- isolate(input$size_range)
    cur_max <- if (!is.null(cur) && length(cur) == 2 && all(is.finite(cur))) cur[2] else max_a
    
    min_user <- isolate(input$min_size_m2)
    if (is.null(min_user) || !is.finite(min_user)) min_user <- min_a
    min_user <- clamp(min_user, min_a, max_a)
    
    cur_max <- clamp(cur_max, min_user, max_a)
    
    tags$div(
      numericInput(
        "min_size_m2",
        "Minimum building size (m²)",
        value = min_user,
        min = min_a, max = max_a, step = 1
      ),
      tags$div(
        class = "rangeRow",
        tags$div(
          style = "flex:1;",
          sliderInput(
            "size_range",
            "Building polygon size range (m²)",
            min = min_user,
            max = max_a,
            value = c(min_user, cur_max),
            step = 1
          )
        ),
        tags$div(
          class = "rangeRight",
          tags$strong("Buildings in range:"),
          tags$div(textOutput("buildings_in_range_txt"))
        )
      )
    )
  })
  
  observeEvent(input$min_size_m2, {
    a <- rv$poly_last_areas_m2
    if (is.null(a) || length(a) == 0) return()
    
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) return()
    
    min_a <- floor(min(a))
    max_a <- ceiling(max(a))
    
    min_user <- clamp(input$min_size_m2, min_a, max_a)
    
    cur <- input$size_range
    cur_max <- if (!is.null(cur) && length(cur) == 2 && is.finite(cur[2])) cur[2] else max_a
    cur_max <- clamp(cur_max, min_user, max_a)
    
    updateSliderInput(
      session, "size_range",
      min = min_user,
      max = max_a,
      value = c(min_user, cur_max)
    )
  }, ignoreInit = TRUE)
  
  # ---------------------------
  # Histogram (plotly) — NO ZOOM / PAN
  # ---------------------------
  output$size_hist <- renderPlotly({
    a <- rv$poly_last_areas_m2
    
    if (is.null(a) || length(a) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no polygon areas"),
          xaxis = list(title = "Polygon area (m²)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no valid areas"),
          xaxis = list(title = "Polygon area (m²)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    nb <- as.numeric(coalesce(input$hist_bins, 120))
    if (!is.finite(nb) || nb <= 0) nb <- 120
    
    xmin <- min(a)
    xmax <- max(a)
    binw <- max(1, (xmax - xmin) / nb)
    
    rng <- input$size_range
    shapes <- list()
    if (!is.null(rng) && length(rng) == 2) {
      shapes <- list(
        list(type = "line", x0 = rng[1], x1 = rng[1], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2)),
        list(type = "line", x0 = rng[2], x1 = rng[2], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2))
      )
    }
    
    p <- plot_ly(
      x = a,
      type = "histogram",
      autobinx = FALSE,
      xbins = list(start = xmin, end = xmax, size = binw)
    ) %>%
      layout(
        title = paste0("Temporal V1 ", LAST_YEAR, ": polygon areas"),
        xaxis = list(title = "Polygon area (m²)", fixedrange = TRUE),
        yaxis = list(title = "Count", fixedrange = TRUE),
        shapes = shapes,
        bargap = 0.02
      )
    
    config(
      p,
      staticPlot = TRUE,
      displayModeBar = FALSE
    )
  })
  
  # ---------------------------
  # Live filtering of ALL datasets on the map as slider moves
  # ---------------------------
  observeEvent(input$size_range, {
    req(rv$roi_sf)
    rng <- input$size_range
    
    p_first <- filter_by_range(rv$poly_first, rng)
    p_last  <- filter_by_range(rv$poly_last,  rng)
    p_v3    <- filter_by_range(rv$v3_sf,      rng)
    
    draw_polys(
      p_first,
      group = paste0("Buildings ", FIRST_YEAR, " (Temporal)"),
      color = "black",
      fillOpacity = 0.35,
      weight = 0.5,
      fillColor = "black"
    )
    
    draw_polys(
      p_last,
      group = paste0("Buildings ", LAST_YEAR, " (Temporal)"),
      color = "blue",
      fillOpacity = 0.0,
      weight = 2
    )
    
    draw_polys(
      p_v3,
      group = "Buildings V3 (polygons)",
      color = "red",
      fillOpacity = 0.0,
      weight = 1.2
    )
  }, ignoreInit = TRUE)
  
  # ---------------------------
  # Time-series computation (auto, whenever ROI changes)
  # ---------------------------
  compute_year_area_list <- function(roi_sf) {
    out <- vector("list", length(YEARS))
    names(out) <- as.character(YEARS)
    for (i in seq_along(YEARS)) {
      y <- YEARS[i]
      p <- compute_presence_polygons(roi_sf, y, PRES_THRESH)
      if (is.null(p) || nrow(p) == 0) {
        out[[i]] <- numeric(0)
        next
      }
      p <- add_area_m2(p)
      a <- p$area_m2
      a <- a[is.finite(a) & a > 0]
      out[[i]] <- a
    }
    out
  }
  
  ts_filtered <- reactive({
    req(rv$year_area_list, rv$roi_area_m2)
    rng <- input$size_range
    if (is.null(rng) || length(rng) != 2) return(NULL)
    
    roi_m2 <- rv$roi_area_m2
    if (!is.finite(roi_m2) || roi_m2 <= 0) return(NULL)
    
    yrs <- as.integer(names(rv$year_area_list))
    yrs <- yrs[order(yrs)]
    
    rows <- lapply(yrs, function(y) {
      a <- rv$year_area_list[[as.character(y)]]
      a_sel <- a[a >= rng[1] & a <= rng[2]]
      cnt <- length(a_sel)
      cover <- if (length(a_sel) > 0) 100 * (sum(a_sel) / roi_m2) else 0
      data.frame(year = y, building_count = cnt, cover_pct = cover)
    })
    
    do.call(rbind, rows)
  })
  
  output$ts_plot <- renderPlot({
    req(rv$roi_sf)  # ROI must exist
    
    ts <- ts_filtered()
    if (is.null(ts) || nrow(ts) == 0) {
      plot.new()
      title("No time-series data yet (select/draw an ROI).")
      return()
    }
    
    ts <- ts[order(ts$year), ]
    years <- ts$year
    b <- ts$building_count
    cvr <- ts$cover_pct
    
    # V3 point for 2023 (filtered by current size range, if possible)
    rng <- input$size_range
    v3_count_2023 <- NA_real_
    if (!is.null(rv$v3_sf) && nrow(rv$v3_sf) > 0 && !is.null(rng) && length(rng) == 2) {
      v3_tmp <- add_area_m2(rv$v3_sf)
      v3_keep <- is.finite(v3_tmp$area_m2) & v3_tmp$area_m2 >= rng[1] & v3_tmp$area_m2 <= rng[2]
      v3_count_2023 <- sum(v3_keep, na.rm = TRUE)
    } else if (is.finite(rv$v3_count)) {
      # fallback: total V3 count in ROI (unfiltered)
      v3_count_2023 <- rv$v3_count
    }
    
    par(mar = c(6, 4.5, 5, 5))
    y1max <- max(
      1,
      max(b, na.rm = TRUE),
      if (is.finite(v3_count_2023)) v3_count_2023 else NA_real_,
      na.rm = TRUE
    ) * 1.35
    
    mids <- barplot(
      height = b,
      names.arg = years,
      las = 1,
      ylab = "Building count (filtered by size range)",
      main = "Filtered buildings: count (bars) + % land covered (line)",
      ylim = c(0, y1max),
      border = NA
    )
    
    text(mids, b + 0.03 * y1max, labels = format(round(b, 0), big.mark = ","), cex = 0.82)
    
    # --- V3 point in RED at year 2023 ---
    if (is.finite(v3_count_2023)) {
      idx_2023 <- which(years == 2023)
      if (length(idx_2023) == 1) {
        x2023 <- mids[idx_2023]
        points(x2023, v3_count_2023, pch = 16, col = "red")
        text(
          x2023,
          v3_count_2023 + 0.04 * y1max,
          labels = paste0("V3: ", fmt_num0(v3_count_2023)),
          cex = 0.82,
          col = "red"
        )
      }
    }
    
    par(new = TRUE)
    y2lim <- range(cvr, na.rm = TRUE)
    if (!is.finite(y2lim[1]) || !is.finite(y2lim[2])) y2lim <- c(0, 1)
    if (diff(y2lim) == 0) y2lim <- y2lim + c(-0.5, 0.5)
    
    plot(mids, cvr, type = "o", pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = y2lim)
    axis(4)
    mtext("% land covered (filtered)", side = 4, line = 3)
    
    text(mids, cvr + 0.04 * diff(y2lim), labels = paste0(round(cvr, 1), "%"), cex = 0.82)
    
    legend(
      "topleft",
      legend = c("Filtered count (bars)", "Filtered % cover (line)", "V3 count in 2023 (point)"),
      pch = c(15, 1, 16),
      col = c("black", "black", "red"),
      pt.cex = c(1.5, 1, 1),
      bty = "n"
    )
  })
  
  # ---------------------------
  # Main pipeline for ROI/Area (AUTO computes time series)
  # ---------------------------
  run_pipeline_for_roi <- function(roi_sf, label = "ROI") {
    status(paste0(label, " received. Computing polygons, histogram, and time-series…"))
    
    rv$roi_sf <- roi_sf
    rv$roi_geojson <- geojsonsf::sf_geojson(roi_sf)
    
    roi_ee <- sf_as_ee(roi_sf)$geometry()
    rv$roi_area_m2 <- tryCatch(roi_ee$area(1)$getInfo(), error = function(e) NA_real_)
    
    draw_roi(roi_sf)
    
    rv$year_area_list <- NULL
    
    withProgress(message = "Computing ROI datasets…", value = 0, {
      incProgress(0.10)
      
      rv$poly_first <- add_area_m2(compute_presence_polygons(roi_sf, FIRST_YEAR, PRES_THRESH))
      incProgress(0.15)
      
      rv$poly_last  <- add_area_m2(compute_presence_polygons(roi_sf, LAST_YEAR,  PRES_THRESH))
      incProgress(0.15)
      
      rv$poly_last_areas_m2 <- NULL
      if (!is.null(rv$poly_last) && nrow(rv$poly_last) > 0) {
        a <- rv$poly_last$area_m2
        a <- a[is.finite(a) & a > 0]
        if (length(a) > 0) rv$poly_last_areas_m2 <- a
      }
      
      v3 <- compute_v3_in_roi(roi_sf)
      rv$v3_count <- v3$count
      rv$v3_sf <- add_area_m2(v3$sf)
      incProgress(0.10)
      
      # Draw initial (unfiltered until slider is available)
      draw_polys(rv$poly_first, paste0("Buildings ", FIRST_YEAR, " (Temporal)"), "black", 0.35, 0.5, "black")
      draw_polys(rv$poly_last,  paste0("Buildings ", LAST_YEAR,  " (Temporal)"), "blue",  0.0,  2)
      draw_polys(rv$v3_sf, "Buildings V3 (polygons)", "red", 0.0, 1.2)
      incProgress(0.10)
      
      # AUTO time-series
      incProgress(0.05)
      tryCatch({
        rv$year_area_list <- compute_year_area_list(roi_sf)
      }, error = function(e) {
        rv$year_area_list <- NULL
        status(paste0("Error computing yearly polygons: ", conditionMessage(e)))
      })
      incProgress(0.35)
    })
    
    if (!is.null(rv$year_area_list)) {
      status("Done. Adjust the size slider to filter map + graphs live.")
    }
  }
  
  # ---------------------------
  # Mode toggles
  # ---------------------------
  observeEvent(input$toggle_wards, {
    if (is.null(wards_sf) || nrow(wards_sf) == 0) {
      status("Could not load wards shapefile (check path / permissions).")
      return()
    }
    
    if (!wards_on()) {
      wards_on(TRUE)
      umn_on(FALSE)
      
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      leafletProxy("map") %>% clearGroup("UMN Areas")
      
      draw_wards_all()
      status("Wards shown. Click a ward to set ROI (other ROI shapes will be removed).")
    } else {
      wards_on(FALSE)
      leafletProxy("map") %>% clearGroup("Wards")
      status("Wards hidden.")
    }
  })
  
  observeEvent(input$toggle_umn, {
    if (is.null(umn_sf) || nrow(umn_sf) == 0) {
      status("Could not load UMN areas shapefile (check path / permissions, and ensure .shp exists).")
      return()
    }
    
    if (!umn_on()) {
      umn_on(TRUE)
      wards_on(FALSE)
      
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      leafletProxy("map") %>% clearGroup("Wards")
      
      draw_umn_all()
      status("UMN Areas shown. Click an area to set ROI (other ROI shapes will be removed).")
    } else {
      umn_on(FALSE)
      leafletProxy("map") %>% clearGroup("UMN Areas")
      status("UMN Areas hidden.")
    }
  })
  
  observeEvent(input$toggle_draw, {
    draw_on(!draw_on())
    
    if (isTRUE(draw_on())) {
      wards_on(FALSE)
      umn_on(FALSE)
      leafletProxy("map") %>% clearGroup("Wards") %>% clearGroup("UMN Areas")
    }
    
    session$sendCustomMessage("toggleDrawControls", list(show = draw_on()))
    status(if (draw_on()) "Draw mode ON. Draw a polygon/rectangle ROI." else "Draw mode OFF.")
  })
  
  # ---------------------------
  # Shape click handler (Wards OR UMN)
  # Removes all other ROI shapes when a selection is made.
  # ---------------------------
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    if (is.null(click$group)) return()
    
    if (isTRUE(wards_on()) && click$group == "Wards") {
      ward_poly <- wards_sf[wards_sf$WARD_ID_LEAFLET == as.character(click$id), ]
      if (nrow(ward_poly) == 0) return()
      
      # Remove all other ROI shapes/layers first
      clear_map_overlays()
      wards_on(FALSE)
      umn_on(FALSE)
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      
      status(paste0("Ward ", click$id, " selected. Computing…"))
      
      bb <- sf::st_bbox(ward_poly)
      leafletProxy("map") %>% fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
      
      tryCatch({
        ward_roi <- prep_roi_sf(ward_poly, simplify_m = 5)
        run_pipeline_for_roi(ward_roi, label = paste0("Ward ", click$id))
      }, error = function(e) {
        status(paste0("EE error on ward click: ", conditionMessage(e)))
      })
      return()
    }
    
    if (isTRUE(umn_on()) && click$group == "UMN Areas") {
      u_poly <- umn_sf[umn_sf$UMN_ID_LEAFLET == as.character(click$id), ]
      if (nrow(u_poly) == 0) return()
      
      # Remove all other ROI shapes/layers first
      clear_map_overlays()
      wards_on(FALSE)
      umn_on(FALSE)
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      
      status(paste0("UMN area ", click$id, " selected. Computing…"))
      
      bb <- sf::st_bbox(u_poly)
      leafletProxy("map") %>% fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
      
      tryCatch({
        u_roi <- prep_roi_sf(u_poly, simplify_m = 5)
        run_pipeline_for_roi(u_roi, label = paste0("UMN ", click$id))
      }, error = function(e) {
        status(paste0("EE error on UMN click: ", conditionMessage(e)))
      })
      return()
    }
  })
  
  # ---------------------------
  # ROI draw handler
  # Removes all other ROI shapes when user draws.
  # ---------------------------
  observeEvent(input$map_draw_new_feature, {
    req(input$map_draw_new_feature)
    if (!isTRUE(draw_on())) return()
    
    clear_map_overlays()
    wards_on(FALSE)
    umn_on(FALSE)
    
    roi_sf_raw <- feature_to_sf(input$map_draw_new_feature)
    roi_sf <- prep_roi_sf(roi_sf_raw, simplify_m = 0)
    run_pipeline_for_roi(roi_sf, label = "ROI")
  })
  
  # Trash/delete reset
  observeEvent(input$map_draw_deleted_features, {
    clear_map_overlays()
    wards_on(FALSE)
    umn_on(FALSE)
    draw_on(FALSE)
    session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
    
    rv$roi_sf <- NULL
    rv$roi_area_m2 <- NULL
    rv$roi_geojson <- NULL
    rv$poly_first <- NULL
    rv$poly_last <- NULL
    rv$v3_sf <- NULL
    rv$v3_count <- NA_real_
    rv$poly_last_areas_m2 <- NULL
    rv$year_area_list <- NULL
    
    status("Cleared. Choose a mode: Draw ROI, Wards, or UMN Areas.")
  })
  
  # ---------------------------
  # Output: ROI area
  # ---------------------------
  output$roi_area_m2 <- renderText({
    if (is.null(rv$roi_area_m2) || !is.finite(rv$roi_area_m2)) "ROI area (m²): —"
    else paste0("ROI area (m²): ", fmt_int(rv$roi_area_m2))
  })
  
  # ---------------------------
  # CSV Export
  # ---------------------------
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("open_buildings_filtered_timeseries_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$roi_sf)
      
      rng <- input$size_range
      if (is.null(rng) || length(rng) != 2) rng <- c(NA_real_, NA_real_)
      
      ts <- ts_filtered()
      if (is.null(ts)) ts <- data.frame(year = YEARS, building_count = NA_integer_, cover_pct = NA_real_)
      
      out <- ts
      out$roi_area_m2 <- rv$roi_area_m2
      out$roi_geojson <- rv$roi_geojson
      out$size_min_m2 <- rng[1]
      out$size_max_m2 <- rng[2]
      out$v3_building_count_total_in_roi <- rv$v3_count
      out$presence_threshold <- PRES_THRESH
      
      out <- out[, c(
        "roi_area_m2", "roi_geojson", "size_min_m2", "size_max_m2",
        "year", "building_count", "cover_pct",
        "v3_building_count_total_in_roi", "presence_threshold"
      )]
      
      utils::write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

