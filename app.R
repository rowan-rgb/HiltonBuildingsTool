# ------------------------------------------------------------
# app.R — Hilton Buildings Tool
#
# - NO Google Earth Engine / rgee / reticulate
# - Loads local GeoJSON on-demand (lazy) so the map renders immediately
# - Clips buildings to ROI locally (sf)
# - Keeps Wards / UMN Areas / Draw modes + live size filtering + plots + CSV export
# ------------------------------------------------------------

# install.packages(c(
#   "shiny",
#   "leaflet",
#   "leaflet.extras",
#   "sf",
#   "jsonlite",
#   "geojsonsf",
#   "htmlwidgets",
#   "plotly",
#   "openxlsx"
# ))

#install.packages("leaflet.extras")

#install.packages("remotes")
#remotes::install_github("trafficonese/leaflet.extras")

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)
library(geojsonsf)
library(htmlwidgets)
library(plotly)
library(openxlsx)

# ---------------------------
# Local buildings data paths
# ---------------------------
TEMPORAL_DIR <- "Data/V1 Temporal_large"
V3_DIR       <- "Data/V3 Shapefile_large"

# TEMPORAL_FILES <- list(
#   "2016" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2016_wards_roi_thr0.5_2026_02_17_11_39_40.geojson"),
#   "2017" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2017_wards_roi_thr0.5_2026_02_17_11_39_41.geojson"),
#   "2018" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2018_wards_roi_thr0.5_2026_02_17_11_39_42.geojson"),
#   "2019" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2019_wards_roi_thr0.5_2026_02_17_11_39_43.geojson"),
#   "2020" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2020_wards_roi_thr0.5_2026_02_17_11_39_44.geojson"),
#   "2021" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2021_wards_roi_thr0.5_2026_02_17_11_39_45.geojson"),
#   "2022" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2022_wards_roi_thr0.5_2026_02_17_11_39_46.geojson"),
#   "2023" = file.path(TEMPORAL_DIR, "openbuildings_temporal_v1_2023_wards_roi_thr0.5_2026_02_17_11_39_47.geojson")
# )

TEMPORAL_FILES <- list(
  "2016" = file.path(TEMPORAL_DIR, "V1_2016_UMN_Functional_Areas_3_withHeight.geojson"),
  "2017" = file.path(TEMPORAL_DIR, "V1_2017_UMN_Functional_Areas_3_withHeight.geojson"),
  "2018" = file.path(TEMPORAL_DIR, "V1_2018_UMN_Functional_Areas_3_withHeight.geojson"),
  "2019" = file.path(TEMPORAL_DIR, "V1_2019_UMN_Functional_Areas_3_withHeight.geojson"),
  "2020" = file.path(TEMPORAL_DIR, "V1_2020_UMN_Functional_Areas_3_withHeight.geojson"),
  "2021" = file.path(TEMPORAL_DIR, "V1_2021_UMN_Functional_Areas_3_withHeight.geojson"),
  "2022" = file.path(TEMPORAL_DIR, "V1_2022_UMN_Functional_Areas_3_withHeight.geojson"),
  "2023" = file.path(TEMPORAL_DIR, "V1_2023_UMN_Functional_Areas_3_withHeight.geojson")
)

V3_FILE <- file.path(V3_DIR, "V3_2023_UMN_Functional_Areas_3.geojson")

YEARS <- 2016:2023
FIRST_YEAR <- 2016
LAST_YEAR  <- 2023

# informational only (your Temporal files already include thr0.5)
PRES_THRESH <- 0.5

# ---- local shapefiles ----
wards_path <- "Data/Wards/Municipal_Wards_2021.shp"

UMN_function_areas_path <- "Data/Regions/UMN_Functional_Areas_7e.shp"

# app_dir <- normalizePath(getwd(), winslash = "/")  # if you always run app from its folder
# wards_path <- file.path(app_dir, "Data/Wards/Municipal_Wards_2021.shp")
# UMN_function_areas_path <- file.path(app_dir, "Data/Regions/UMN_Functional_Areas_3.shp")
# 
# message("getwd() = ", getwd())
# message("wards_path = ", wards_path)
# message("exists(wards_path) = ", file.exists(wards_path))

ui <- fluidPage(
  titlePanel("Open Buildings Temporal V1 — Single ROI (LOCAL)"),
  tags$head(
    tags$style(
      HTML("
        .leaflet-draw { display: none; }
        .rangeRow { display:flex; align-items:center; justify-content:space-between; gap:12px; }
        .rangeRight { text-align:right; white-space:nowrap; }
        .smallHelp { font-size: 12px; color:#666; margin-top:4px; }

        /* Separate histogram panels so they don't blend */
        .histoCard {
          background: #ffffff;
          border: 1px solid #e6e6e6;
          border-radius: 6px;
          padding: 10px;
          margin-bottom: 16px;
        }
        .histoSpacer { height: 10px; }
      ")
    ),
    tags$script(
      HTML("
        Shiny.addCustomMessageHandler('toggleDrawControls', function(msg) {
          var el = document.querySelector('.leaflet-draw');
          if (!el) return;
          el.style.display = msg.show ? 'block' : 'none';
        });
      ")
    )
  ),
  
  # Top row: Map + right-side panels
  fluidRow(
    column(
      width = 8,
      tags$div(
        style = "background:#f5f5f5; padding:12px; border-radius:6px; margin-bottom:10px;",
        h4("Instructions"),
        tags$ol(
          tags$li("Choose an area for analysis: Draw ROI, Wards, or UMN Areas."),
          tags$li(paste0("Histograms show Temporal V1 ", LAST_YEAR, " polygon sizes (m²) and heights (m).")),
          tags$li("Size + height sliders filter ALL overlays live; histograms reflect the current filters."),
          tags$li("Graphs compute automatically when you select/draw an ROI.")
        )
      ),
      tags$div(
        style = "display:flex; gap:10px; margin-bottom:10px;",
        actionButton("toggle_wards", "Wards"),
        actionButton("toggle_umn", "Functional Areas"),
        actionButton("toggle_draw", "Draw")
      ),
      
      leafletOutput("map", height = 900)
    ),
    
    column(
      width = 4,
      tags$div(
        style = "display:flex; align-items:center; justify-content:space-between; gap:10px; margin-bottom:6px;",
        strong(textOutput("roi_area_m2")),
        downloadButton("download_csv",      "Summary XLSX"),
        downloadButton("download_full_csv", "Full XLSX")
      ),
      
      # --- Size histogram + size range slider (WITH manual min input) ---
      tags$div(
        class = "histoCard",
        plotlyOutput("size_hist", height = 260),
        div(class = "histoSpacer"),
        uiOutput("size_slider_ui")
      ),
      
      # --- Height histogram + height range slider (WITH manual min input) ---
      tags$div(
        class = "histoCard",
        plotlyOutput("height_hist", height = 260),
        div(class = "histoSpacer"),
        uiOutput("height_slider_ui")
      ),
      
      verbatimTextOutput("status")
    )
  ),
  
  # Bar graph under the map, full width
  fluidRow(
    column(
      width = 12,
      br(),
      plotOutput("ts_plot", height = 420)
    )
  )
)

server <- function(input, output, session) {
  
  # ---------------------------
  # Status
  # ---------------------------
  status <- reactiveVal("Choose a mode: Draw ROI, Wards, or UMN Areas…")
  output$status <- renderText(status())
  
  # ---------------------------
  # Helpers
  # ---------------------------
  coalesce <- function(x, fallback) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) fallback else x
  }
  
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
  
  fmt_scaled_m2 <- function(x, digits = 1) {
    if (!is.finite(x)) return("—")
    ax <- abs(x)
    if (ax >= 1e6) {
      paste0(format(round(x / 1e6, digits), nsmall = digits, trim = TRUE), " M m²")
    } else if (ax >= 1e3) {
      paste0(format(round(x / 1e3, digits), nsmall = digits, trim = TRUE), " K m²")
    } else {
      paste0(format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE), " m²")
    }
  }
  
  clamp <- function(x, lo, hi) {
    x <- as.numeric(x)
    if (!is.finite(x)) return(lo)
    max(lo, min(hi, x))
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
  
  roi_area_m2_local <- function(roi_sf) {
    if (is.null(roi_sf) || nrow(roi_sf) == 0) return(NA_real_)
    roi_m <- tryCatch(sf::st_transform(roi_sf, 3857), error = function(e) NULL)
    if (is.null(roi_m)) return(NA_real_)
    as.numeric(sf::st_area(sf::st_union(roi_m)))
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
    if (is.null(rng) || length(rng) != 2 || any(!is.finite(rng))) return(sfobj)
    
    keep <- is.finite(sfobj$area_m2) &
      sfobj$area_m2 >= rng[1] &
      sfobj$area_m2 <= rng[2]
    
    sfobj[keep, , drop = FALSE]
  }
  
  # robust height extraction
  extract_height_m <- function(
    sfobj,
    prefer = c("height_mean_m", "height_median_m", "height_p95_m")
  ) {
    if (is.null(sfobj) || nrow(sfobj) == 0) return(numeric(0))
    
    candidates <- c(
      prefer,
      "height_mean", "height_median", "height_p95",
      "height", "HEIGHT",
      "building_height", "BUILDING_HEIGHT",
      "bldg_height", "BLDG_HEIGHT",
      "bldg_hgt", "BLDG_HGT",
      "hgt", "HGT",
      "height_m", "HEIGHT_M",
      "roof_height", "ROOF_HEIGHT",
      "mean_height", "MEAN_HEIGHT",
      "mean", "median", "p95"
    )
    
    hcol <- candidates[candidates %in% names(sfobj)][1]
    if (is.na(hcol) || is.null(hcol)) return(numeric(0))
    
    h <- suppressWarnings(as.numeric(sfobj[[hcol]]))
    h <- h[is.finite(h)]
    h <- h[h > 0]
    h
  }
  
  # filter sf by height range
  filter_by_height <- function(sfobj, hrng) {
    if (is.null(sfobj) || nrow(sfobj) == 0) return(sfobj)
    if (is.null(hrng) || length(hrng) != 2 || any(!is.finite(hrng))) return(sfobj)
    
    candidates <- c(
      "height_mean_m", "height_median_m", "height_p95_m",
      "height_mean", "height_median", "height_p95",
      "height", "HEIGHT",
      "building_height", "BUILDING_HEIGHT",
      "bldg_height", "BLDG_HEIGHT",
      "bldg_hgt", "BLDG_HGT",
      "hgt", "HGT",
      "height_m", "HEIGHT_M",
      "roof_height", "ROOF_HEIGHT",
      "mean_height", "MEAN_HEIGHT",
      "mean", "median", "p95"
    )
    
    hcol <- candidates[candidates %in% names(sfobj)][1]
    if (is.na(hcol) || is.null(hcol)) return(sfobj)  # if no height field, do not filter
    
    h <- suppressWarnings(as.numeric(sfobj[[hcol]]))
    keep <- is.finite(h) & h > 0 & h >= hrng[1] & h <= hrng[2]
    sfobj[keep, , drop = FALSE]
  }
  
  clip_to_roi <- function(sfobj, roi_sf) {
    if (is.null(sfobj) || nrow(sfobj) == 0 || is.null(roi_sf) || nrow(roi_sf) == 0) {
      return(sfobj[0, , drop = FALSE])
    }
    
    if (is.na(sf::st_crs(sfobj))) sf::st_crs(sfobj) <- 4326
    if (sf::st_crs(sfobj) != sf::st_crs(roi_sf)) {
      sfobj <- sf::st_transform(sfobj, sf::st_crs(roi_sf))
    }
    
    roi_union <- sf::st_union(sf::st_make_valid(roi_sf))
    
    idx <- sf::st_intersects(sfobj, roi_union, sparse = FALSE)
    sfobj <- sfobj[idx, , drop = FALSE]
    if (nrow(sfobj) == 0) return(sfobj)
    
    out <- tryCatch(sf::st_intersection(sfobj, roi_union), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) return(sfobj[0, , drop = FALSE])
    
    sf::st_make_valid(out)
  }
  
  read_geojson_safe <- function(path) {
    tryCatch({
      x <- sf::st_read(path, quiet = TRUE)
      x <- sf::st_zm(x, drop = TRUE, what = "ZM")
      x <- sf::st_make_valid(x)
      if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
      sf::st_transform(x, 4326)
    }, error = function(e) {
      message("ERROR reading: ", path, " :: ", conditionMessage(e))
      NULL
    })
  }
  
  # ---------------------------
  # Lazy-load cache for big GeoJSONs
  # ---------------------------
  cache <- reactiveValues(
    temporal = list(),
    v3 = NULL
  )
  
  get_temporal_layer <- function(year) {
    y <- as.character(year)
    if (!is.null(cache$temporal[[y]])) return(cache$temporal[[y]])
    
    path <- TEMPORAL_FILES[[y]]
    if (is.null(path) || !file.exists(path)) {
      message("Temporal file missing for year ", y, ": ", path)
      return(NULL)
    }
    
    x <- read_geojson_safe(path)
    cache$temporal[[y]] <- x
    x
  }
  
  get_v3_layer <- function() {
    if (!is.null(cache$v3)) return(cache$v3)
    if (!file.exists(V3_FILE)) {
      message("V3 file missing: ", V3_FILE)
      return(NULL)
    }
    
    cache$v3 <- read_geojson_safe(V3_FILE)
    cache$v3
  }
  
  compute_temporal_in_roi <- function(roi_sf, year) {
    layer <- get_temporal_layer(year)
    if (is.null(layer) || nrow(layer) == 0) return(NULL)
    clip_to_roi(layer, roi_sf)
  }
  
  compute_v3_in_roi <- function(roi_sf) {
    layer <- get_v3_layer()
    if (is.null(layer) || nrow(layer) == 0) return(list(count = NA_real_, sf = NULL))
    v <- clip_to_roi(layer, roi_sf)
    list(count = nrow(v), sf = v)
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
    candidates <- c(
      "AREA", "AREA_ID", "ID", "NAME", "Name", "REGION", "REGION_ID",
      "ZONE", "ZONE_ID", "FUNCTION", "FUNC_AREA"
    )
    umn_id_col <- candidates[candidates %in% names(umn_sf)][1]
    if (is.na(umn_id_col) || is.null(umn_id_col)) {
      umn_sf$UMN_ID <- seq_len(nrow(umn_sf))
      umn_id_col <- "UMN_ID"
    }
    umn_sf$UMN_ID_LEAFLET <- as.character(umn_sf[[umn_id_col]])
  }
  
  # ---------------------------
  # Map draw helpers
  # ---------------------------
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
      addPolygons(
        data = roi_sf,
        group = "ROI",
        color = "black",
        weight = 3,
        fillOpacity = 0.12
      )
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
  
  draw_wards_all <- function() {
    leafletProxy("map") %>% clearGroup("Wards")
    if (is.null(wards_sf) || nrow(wards_sf) == 0) return()
    
    leafletProxy("map") %>%
      addPolygons(
        data = wards_sf,
        group = "Wards",
        layerId = ~WARD_ID_LEAFLET,
        color = "#FF7A00",
        weight = 3,
        opacity = 0.9,
        fillColor = "#FF7A00",
        fillOpacity = 0.12,
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
        color = "#7B61FF",
        weight = 3,
        opacity = 0.9,
        fillColor = "#7B61FF",
        fillOpacity = 0.12,
        label = ~paste0("UMN: ", UMN_ID_LEAFLET)
      )
  }
  
  # ---------------------------
  # Size slider UI (WITH manual min)
  # ---------------------------
  buildings_in_size_range <- reactive({
    a <- rv$poly_last_areas_m2
    rng <- input$size_range
    
    if (is.null(a) || length(a) == 0) return(0L)
    if (is.null(rng) || length(rng) != 2) return(length(a))
    
    sum(is.finite(a) & a >= rng[1] & a <= rng[2])
  })
  
  output$buildings_in_size_range_txt <- renderText(fmt_num0(buildings_in_size_range()))
  
  output$size_slider_ui <- renderUI({
    a <- rv$poly_last_areas_m2
    if (is.null(a) || length(a) == 0) return(NULL)
    
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) return(NULL)
    
    min_a <- floor(min(a))
    max_a <- ceiling(max(a))
    
    min_user <- isolate(input$min_size_m2)
    if (is.null(min_user) || !is.finite(min_user)) min_user <- min_a
    min_user <- clamp(min_user, min_a, max_a)
    
    max_user <- isolate(input$max_size_m2)
    if (is.null(max_user) || !is.finite(max_user)) max_user <- max_a
    max_user <- clamp(max_user, min_user, max_a)
    
    tags$div(
      numericInput(
        "min_size_m2",
        "Minimum building size (m\u00b2)",
        value = min_user,
        min = min_a, max = max_a,
        step = 1
      ),
      numericInput(
        "max_size_m2",
        "Maximum building size (m\u00b2)",
        value = max_user,
        min = min_a, max = max_a,
        step = 1
      ),
      tags$div(
        class = "rangeRow",
        tags$div(
          style = "flex:1;",
          sliderInput(
            "size_range",
            "Building polygon size range (m\u00b2)",
            min = min_a,
            max = max_a,
            value = c(min_user, max_user),
            step = 1
          )
        ),
        tags$div(
          class = "rangeRight",
          tags$strong("Buildings in range:"),
          tags$div(textOutput("buildings_in_size_range_txt"))
        )
      )
    )
  })
  
  # Sync slider when min size typed
  observeEvent(input$min_size_m2, {
    a <- rv$poly_last_areas_m2
    if (is.null(a) || length(a) == 0) return()
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) return()
    min_a <- floor(min(a)); max_a <- ceiling(max(a))
    
    min_user <- clamp(input$min_size_m2, min_a, max_a)
    cur <- input$size_range
    cur_max <- if (!is.null(cur) && length(cur) == 2 && is.finite(cur[2])) cur[2] else max_a
    cur_max <- clamp(cur_max, min_user, max_a)
    
    updateSliderInput(session, "size_range", min = min_a, max = max_a, value = c(min_user, cur_max))
  }, ignoreInit = TRUE)
  
  # Sync slider when max size typed
  observeEvent(input$max_size_m2, {
    a <- rv$poly_last_areas_m2
    if (is.null(a) || length(a) == 0) return()
    a <- a[is.finite(a) & a > 0]
    if (length(a) == 0) return()
    min_a <- floor(min(a)); max_a <- ceiling(max(a))
    
    max_user <- clamp(input$max_size_m2, min_a, max_a)
    cur <- input$size_range
    cur_min <- if (!is.null(cur) && length(cur) == 2 && is.finite(cur[1])) cur[1] else min_a
    cur_min <- clamp(cur_min, min_a, max_user)
    
    updateSliderInput(session, "size_range", min = min_a, max = max_a, value = c(cur_min, max_user))
  }, ignoreInit = TRUE)
  
  # ---------------------------
  # Height slider UI (WITH manual min) — range computed from LAST_YEAR heights filtered by current size_range
  # ---------------------------
  heights_last_filtered_by_size <- reactive({
    p_last <- rv$poly_last
    if (is.null(p_last) || nrow(p_last) == 0) return(numeric(0))
    
    srng <- input$size_range
    if (!is.null(srng) && length(srng) == 2 && all(is.finite(srng))) {
      p_last <- filter_by_range(p_last, srng)
    }
    
    h <- extract_height_m(p_last)
    h <- h[is.finite(h) & h > 0]
    h
  })
  
  buildings_in_height_range <- reactive({
    h <- heights_last_filtered_by_size()
    hrng <- input$height_range
    
    if (length(h) == 0) return(0L)
    if (is.null(hrng) || length(hrng) != 2 || any(!is.finite(hrng))) return(length(h))
    
    sum(h >= hrng[1] & h <= hrng[2], na.rm = TRUE)
  })
  
  output$buildings_in_height_range_txt <- renderText(fmt_num0(buildings_in_height_range()))
  
  output$height_slider_ui <- renderUI({
    h <- heights_last_filtered_by_size()
    if (length(h) == 0) return(NULL)
    
    min_h <- floor(min(h))
    max_h <- ceiling(max(h))
    if (!is.finite(min_h) || !is.finite(max_h) || min_h >= max_h) return(NULL)
    
    min_user <- isolate(input$min_height_m)
    if (is.null(min_user) || !is.finite(min_user)) min_user <- min_h
    min_user <- clamp(min_user, min_h, max_h)
    
    max_user <- isolate(input$max_height_m)
    if (is.null(max_user) || !is.finite(max_user)) max_user <- max_h
    max_user <- clamp(max_user, min_user, max_h)
    
    tags$div(
      numericInput(
        "min_height_m",
        "Minimum building height (m)",
        value = min_user,
        min = min_h, max = max_h,
        step = 0.5
      ),
      numericInput(
        "max_height_m",
        "Maximum building height (m)",
        value = max_user,
        min = min_h, max = max_h,
        step = 0.5
      ),
      tags$div(
        class = "rangeRow",
        tags$div(
          style = "flex:1;",
          sliderInput(
            "height_range",
            "Building height range (m)",
            min = min_h,
            max = max_h,
            value = c(min_user, max_user),
            step = 0.5
          )
        ),
        tags$div(
          class = "rangeRight",
          tags$strong("Buildings in range:"),
          tags$div(textOutput("buildings_in_height_range_txt"))
        )
      )
    )
  })
  
  # Sync slider when min height typed
  observeEvent(input$min_height_m, {
    h <- heights_last_filtered_by_size()
    if (length(h) == 0) return()
    min_h <- floor(min(h)); max_h <- ceiling(max(h))
    
    min_user <- clamp(input$min_height_m, min_h, max_h)
    cur <- input$height_range
    cur_max <- if (!is.null(cur) && length(cur) == 2 && is.finite(cur[2])) cur[2] else max_h
    cur_max <- clamp(cur_max, min_user, max_h)
    
    updateSliderInput(session, "height_range", min = min_h, max = max_h, value = c(min_user, cur_max))
  }, ignoreInit = TRUE)
  
  # Sync slider when max height typed
  observeEvent(input$max_height_m, {
    h <- heights_last_filtered_by_size()
    if (length(h) == 0) return()
    min_h <- floor(min(h)); max_h <- ceiling(max(h))
    
    max_user <- clamp(input$max_height_m, min_h, max_h)
    cur <- input$height_range
    cur_min <- if (!is.null(cur) && length(cur) == 2 && is.finite(cur[1])) cur[1] else min_h
    cur_min <- clamp(cur_min, min_h, max_user)
    
    updateSliderInput(session, "height_range", min = min_h, max = max_h, value = c(cur_min, max_user))
  }, ignoreInit = TRUE)
  
  # ---------------------------
  # Histograms (fixed bin counts; bin sliders removed)
  # ---------------------------
  NB_SIZE  <- 120
  NB_HEIGHT <- 120
  
  output$size_hist <- renderPlotly({
    p_last <- rv$poly_last
    if (is.null(p_last) || nrow(p_last) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no polygons (size)"),
          xaxis = list(title = "Polygon area (m²)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    # Apply HEIGHT filter to size histogram so it reflects current filters
    hrng <- input$height_range
    if (!is.null(hrng) && length(hrng) == 2 && all(is.finite(hrng))) {
      p_last <- filter_by_height(p_last, hrng)
    }
    
    p_last <- add_area_m2(p_last)
    a <- p_last$area_m2
    a <- a[is.finite(a) & a > 0]
    
    if (length(a) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no valid areas (after height filter)"),
          xaxis = list(title = "Polygon area (m²)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    xmin <- min(a)
    xmax <- max(a)
    binw <- max(1, (xmax - xmin) / NB_SIZE)
    
    srng <- input$size_range
    shapes <- list()
    if (!is.null(srng) && length(srng) == 2 && all(is.finite(srng))) {
      shapes <- list(
        list(type = "line", x0 = srng[1], x1 = srng[1], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2)),
        list(type = "line", x0 = srng[2], x1 = srng[2], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2))
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
    
    config(p, staticPlot = TRUE, displayModeBar = FALSE)
  })
  
  output$height_hist <- renderPlotly({
    p_last <- rv$poly_last
    if (is.null(p_last) || nrow(p_last) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no polygons (height)"),
          xaxis = list(title = "Building height (m)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    # Apply SIZE filter to height histogram so it reflects current filters
    srng <- input$size_range
    if (!is.null(srng) && length(srng) == 2 && all(is.finite(srng))) {
      p_last <- filter_by_range(p_last, srng)
    }
    
    h <- extract_height_m(p_last)
    if (length(h) == 0) {
      p <- plot_ly(x = numeric(0), type = "histogram") %>%
        layout(
          title = paste0("Temporal V1 ", LAST_YEAR, ": no valid building heights (after size filter)"),
          xaxis = list(title = "Building height (m)"),
          yaxis = list(title = "Count")
        )
      return(config(p, staticPlot = TRUE))
    }
    
    xmin <- min(h)
    xmax <- max(h)
    binw <- (xmax - xmin) / NB_HEIGHT
    if (!is.finite(binw) || binw <= 0) binw <- 1
    
    hrng <- input$height_range
    shapes <- list()
    if (!is.null(hrng) && length(hrng) == 2 && all(is.finite(hrng))) {
      shapes <- list(
        list(type = "line", x0 = hrng[1], x1 = hrng[1], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2)),
        list(type = "line", x0 = hrng[2], x1 = hrng[2], y0 = 0, y1 = 1, yref = "paper", line = list(width = 2))
      )
    }
    
    p <- plot_ly(
      x = h,
      type = "histogram",
      autobinx = FALSE,
      xbins = list(start = xmin, end = xmax, size = binw)
    ) %>%
      layout(
        title = paste0("Temporal V1 ", LAST_YEAR, ": building heights"),
        xaxis = list(title = "Building height (m)", fixedrange = TRUE),
        yaxis = list(title = "Count", fixedrange = TRUE),
        shapes = shapes,
        bargap = 0.02
      )
    
    config(p, staticPlot = TRUE, displayModeBar = FALSE)
  })
  
  # ---------------------------
  # Live filtering of ALL datasets on the map as sliders move (SIZE + HEIGHT combined)
  # ---------------------------
  observeEvent(list(input$size_range, input$height_range), {
    req(rv$roi_sf)
    
    srng <- input$size_range
    hrng <- input$height_range
    
    p_first <- rv$poly_first
    p_last  <- rv$poly_last
    p_v3    <- rv$v3_sf
    
    # apply size filter first
    if (!is.null(srng) && length(srng) == 2 && all(is.finite(srng))) {
      p_first <- filter_by_range(p_first, srng)
      p_last  <- filter_by_range(p_last,  srng)
      p_v3    <- filter_by_range(p_v3,    srng)
    }
    
    # then apply height filter (Temporal layers)
    if (!is.null(hrng) && length(hrng) == 2 && all(is.finite(hrng))) {
      p_first <- filter_by_height(p_first, hrng)
      p_last  <- filter_by_height(p_last,  hrng)
      # V3 likely lacks heights, so we do not height-filter it
    }
    
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
  # Time-series computation (filtered by SIZE only, like your original logic)
  # ---------------------------
  compute_year_area_list <- function(roi_sf) {
    out <- vector("list", length(YEARS))
    names(out) <- as.character(YEARS)
    
    for (i in seq_along(YEARS)) {
      y <- YEARS[i]
      p <- compute_temporal_in_roi(roi_sf, y)
      
      if (is.null(p) || nrow(p) == 0) {
        # Store an empty data frame so downstream code can always expect the same shape
        out[[i]] <- data.frame(area_m2 = numeric(0), height_m = numeric(0))
        next
      }
      
      p <- add_area_m2(p)
      
      # Extract a per-row height vector (NA where no valid height exists)
      height_col_candidates <- c(
        "height_mean_m", "height_median_m", "height_p95_m",
        "height_mean", "height_median", "height_p95",
        "height", "HEIGHT", "building_height", "BUILDING_HEIGHT",
        "bldg_height", "BLDG_HEIGHT", "bldg_hgt", "BLDG_HGT",
        "hgt", "HGT", "height_m", "HEIGHT_M",
        "roof_height", "ROOF_HEIGHT", "mean_height", "MEAN_HEIGHT",
        "mean", "median", "p95"
      )
      hcol <- height_col_candidates[height_col_candidates %in% names(p)][1]
      h_raw <- if (!is.na(hcol) && !is.null(hcol)) {
        suppressWarnings(as.numeric(p[[hcol]]))
      } else {
        rep(NA_real_, nrow(p))
      }
      
      df <- data.frame(
        area_m2  = p$area_m2,
        height_m = h_raw,
        stringsAsFactors = FALSE
      )
      # Only keep rows with a valid positive area; height may be NA
      df <- df[is.finite(df$area_m2) & df$area_m2 > 0, , drop = FALSE]
      out[[i]] <- df
    }
    
    out
  }
  
  ts_filtered <- reactive({
    req(rv$year_area_list, rv$roi_area_m2)
    srng <- input$size_range
    if (is.null(srng) || length(srng) != 2 || any(!is.finite(srng))) return(NULL)
    
    roi_m2 <- rv$roi_area_m2
    if (!is.finite(roi_m2) || roi_m2 <= 0) return(NULL)
    
    yrs <- as.integer(names(rv$year_area_list))
    yrs <- yrs[order(yrs)]
    
    rows <- lapply(yrs, function(y) {
      df <- rv$year_area_list[[as.character(y)]]
      # Support both old (numeric vector) and new (data frame) formats
      a <- if (is.data.frame(df)) df$area_m2 else df
      a_sel <- a[is.finite(a) & a >= srng[1] & a <= srng[2]]
      total_area <- sum(a_sel, na.rm = TRUE)
      cnt <- length(a_sel)
      cover <- if (length(a_sel) > 0) 100 * (total_area / roi_m2) else 0
      
      data.frame(
        year = y,
        building_count = cnt,
        total_built_area_m2 = total_area,
        cover_pct = cover
      )
    })
    
    do.call(rbind, rows)
  })
  
  
  output$ts_plot <- renderPlot({
    req(rv$roi_sf)
    
    ts <- ts_filtered()
    if (is.null(ts) || nrow(ts) == 0) {
      plot.new()
      title("No time-series data yet (select/draw an ROI).")
      return()
    }
    
    ts <- ts[order(ts$year), ]
    years <- ts$year
    b <- ts$building_count
    area_m2 <- ts$total_built_area_m2
        
    srng <- input$size_range
    v3_count_2023 <- NA_real_
    v3_area_m2_2023 <- NA_real_
    
    if (!is.null(rv$v3_sf) && nrow(rv$v3_sf) > 0 && !is.null(srng) && length(srng) == 2) {
      v3_tmp <- add_area_m2(rv$v3_sf)
      v3_keep <- is.finite(v3_tmp$area_m2) &
        v3_tmp$area_m2 >= srng[1] &
        v3_tmp$area_m2 <= srng[2]
      
      v3_count_2023 <- sum(v3_keep, na.rm = TRUE)
      v3_area_m2_2023 <- sum(v3_tmp$area_m2[v3_keep], na.rm = TRUE)
    } else if (is.finite(rv$v3_count)) {
      v3_count_2023 <- rv$v3_count
    }
    
    # ---- scaling for right axis (area in K / M m²)
    area_max <- max(
      area_m2,
      if (is.finite(v3_area_m2_2023)) v3_area_m2_2023 else NA_real_,
      na.rm = TRUE
    )
    
    if (!is.finite(area_max) || area_max <= 0) {
      area_scale <- 1
      area_unit <- "Built area (m²)"
    } else if (area_max >= 1e6) {
      area_scale <- 1e6
      area_unit <- "Built area (M m²)"
    } else if (area_max >= 1e3) {
      area_scale <- 1e3
      area_unit <- "Built area (K m²)"
    } else {
      area_scale <- 1
      area_unit <- "Built area (m²)"
    }
    
    area_scaled <- area_m2 / area_scale
    v3_area_scaled_2023 <- if (is.finite(v3_area_m2_2023)) v3_area_m2_2023 / area_scale else NA_real_
    
    # ---- colours
    col_count <- "#2C7FB8"
    col_area  <- "#F28E2B"
        col_v3    <- "#D7263D"
    bg_panel  <- "#F8FAFC"
    grid_col  <- "#D9E2EC"
    
    # ---- left and right axis limits
    y1max <- max(
      1,
      max(b, na.rm = TRUE),
      if (is.finite(v3_count_2023)) v3_count_2023 else NA_real_,
      na.rm = TRUE
    ) * 1.30
    
    y2max <- max(
      1,
      max(area_scaled, na.rm = TRUE),
      if (is.finite(v3_area_scaled_2023)) v3_area_scaled_2023 else NA_real_,
      na.rm = TRUE
    ) * 1.26
    
    # ---- layout
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(
      mar = c(7.5, 5, 5.8, 5.2),
      oma = c(0, 0, 0, 0),
      xaxs = "i",
      yaxs = "i",
      bty = "l"
    )
    
    # ---- x positions with more spacing between years
    n <- length(years)
    x_mid <- seq(1, by = 1.35, length.out = n)
    offset <- 0.28
    count_x <- x_mid - offset
    area_x  <- x_mid + offset
    bar_w   <- 0.34
    
    xlim_all <- c(min(x_mid) - 0.9, max(x_mid) + 1.25)
    
    # ---- base plot for building count axis
    plot(
      NA, NA,
      xlim = xlim_all,
      ylim = c(0, y1max),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "Building count",
      main = "Filtered buildings by year"
    )
    
    usr <- par("usr")
    
    # panel background + grid
    rect(usr[1], usr[3], usr[2], usr[4], col = bg_panel, border = NA)
    abline(h = pretty(c(0, y1max), n = 6), col = grid_col, lty = "dashed", lwd = 0.8)
    
    # ---- building count bars (left axis)
    for (i in seq_along(b)) {
      rect(
        xleft   = count_x[i] - bar_w / 2,
        ybottom = 0,
        xright  = count_x[i] + bar_w / 2,
        ytop    = b[i],
        col     = adjustcolor(col_count, alpha.f = 0.90),
        border  = NA
      )
    }
    
    axis(2, las = 1, col.axis = "#334E68", col = NA)
    axis(1, at = x_mid, labels = years, las = 1, tick = FALSE, line = -0.5, cex.axis = 0.95)
    mtext("Year", side = 1, line = 5.4)
    
    # ---- overlay right axis for built area + cover line
    par(new = TRUE)
    plot(
      NA, NA,
      xlim = xlim_all,
      ylim = c(0, y2max),
      xaxt = "n", yaxt = "n",
      xlab = "", ylab = ""
    )
    
    # ---- area bars (right axis)
    for (i in seq_along(area_scaled)) {
      rect(
        xleft   = area_x[i] - bar_w / 2,
        ybottom = 0,
        xright  = area_x[i] + bar_w / 2,
        ytop    = area_scaled[i],
        col     = adjustcolor(col_area, alpha.f = 0.85),
        border  = NA
      )
    }
    
    axis(4, las = 1, col.axis = "#7C4A03", col = NA)
    mtext(area_unit, side = 4, line = 3.2, col = "#7C4A03")
    
    # ---- labels on area bars
    text(
      x = area_x,
      y = area_scaled + 0.025 * y2max,
      labels = vapply(area_m2, fmt_scaled_m2, character(1), digits = 1),
      cex = 0.74,
      col = "#7C4A03"
    )
    
    # ---- V3 point for 2023: count on left axis
    if (is.finite(v3_count_2023)) {
      idx_2023 <- which(years == 2023)
      if (length(idx_2023) == 1) {
        par(new = TRUE)
        plot(
          NA, NA,
          xlim = xlim_all,
          ylim = c(0, y1max),
          xaxt = "n", yaxt = "n",
          xlab = "", ylab = ""
        )
        points(
          count_x[idx_2023], v3_count_2023,
          pch = 21, bg = col_v3, col = "white", cex = 1.35
        )
        text(
          count_x[idx_2023],
          v3_count_2023 + 0.045 * y1max,
          labels = paste0("V3 count: ", fmt_num0(v3_count_2023)),
          cex = 0.76,
          col = col_v3,
          font = 2
        )
      }
    }
    
    # ---- V3 built area on right axis
    if (is.finite(v3_area_scaled_2023)) {
      idx_2023 <- which(years == 2023)
      if (length(idx_2023) == 1) {
        par(new = TRUE)
        plot(
          NA, NA,
          xlim = xlim_all,
          ylim = c(0, y2max),
          xaxt = "n", yaxt = "n",
          xlab = "", ylab = ""
        )
        points(
          area_x[idx_2023], v3_area_scaled_2023,
          pch = 23, bg = col_v3, col = "white", cex = 1.25
        )
        text(
          area_x[idx_2023],
          v3_area_scaled_2023 + 0.045 * y2max,
          labels = paste0("V3 area: ", fmt_scaled_m2(v3_area_m2_2023, digits = 1)),
          cex = 0.74,
          col = col_v3,
          font = 2
        )
      }
    }
    
    # ---- subtle baseline
    par(new = TRUE)
    plot(
      NA, NA,
      xlim = xlim_all,
      ylim = c(0, y1max),
      xaxt = "n", yaxt = "n",
      xlab = "", ylab = ""
    )
    abline(h = 0, col = "#9FB3C8", lwd = 1)
    
    # ---- building-count labels drawn LAST so they stay visible
    valid_b <- is.finite(b)
    text(
      x = count_x[valid_b],
      y = b[valid_b] + 0.025 * y1max,
      labels = format(round(b[valid_b], 0), big.mark = ","),
      cex = 0.78,
      col = "#1F2933",
      font = 2
    )
    
    # ---- legend moved outside plot area
    legend(
      "top",
      inset = c(0, -0.10),
      xpd = NA,
      horiz = TRUE,
      bty = "n",
      cex = 0.88,
      pt.cex = c(1.3, 1.3, 1.1, 1.1),
      pch = c(15, 15, 21, 23),
      pt.bg = c(col_count, col_area, col_v3, col_v3),
      col = c(col_count, col_area, "white", "white"),
      lty = c(0, 0, 0, 0),
      lwd = c(0, 0, 0, 0),
      legend = c(
        "Building count",
        paste0("Built area (", sub("Built area \\(|\\)", "", area_unit), ")"),
        "V3 count in 2023",
        "V3 built area in 2023"
      )
    )
    
    # ---- subtitle
    mtext(
      "Counts on left axis, built area on right axis",
      side = 3, line = 0.8, cex = 0.86, col = "#486581"
    )
  })  # ---------------------------
  # Main pipeline for ROI
  # ---------------------------
  run_pipeline_for_roi <- function(roi_sf, label = "ROI") {
    status(paste0(label, " received. Computing polygons, histograms, and time-series…"))
    
    rv$roi_sf <- roi_sf
    rv$roi_geojson <- geojsonsf::sf_geojson(roi_sf)
    rv$roi_area_m2 <- roi_area_m2_local(roi_sf)
    
    draw_roi(roi_sf)
    
    rv$year_area_list <- NULL
    
    withProgress(message = "Computing ROI datasets…", value = 0, {
      incProgress(0.10)
      
      rv$poly_first <- add_area_m2(compute_temporal_in_roi(roi_sf, FIRST_YEAR))
      incProgress(0.15)
      
      rv$poly_last <- add_area_m2(compute_temporal_in_roi(roi_sf, LAST_YEAR))
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
      
      # initial draw (apply current filters if already set)
      srng <- input$size_range
      hrng <- input$height_range
      
      p_first <- rv$poly_first
      p_last  <- rv$poly_last
      p_v3    <- rv$v3_sf
      
      if (!is.null(srng) && length(srng) == 2 && all(is.finite(srng))) {
        p_first <- filter_by_range(p_first, srng)
        p_last  <- filter_by_range(p_last,  srng)
        p_v3    <- filter_by_range(p_v3,    srng)
      }
      if (!is.null(hrng) && length(hrng) == 2 && all(is.finite(hrng))) {
        p_first <- filter_by_height(p_first, hrng)
        p_last  <- filter_by_height(p_last,  hrng)
      }
      
      draw_polys(p_first, paste0("Buildings ", FIRST_YEAR, " (Temporal)"), "black", 0.35, 0.5, "black")
      draw_polys(p_last,  paste0("Buildings ", LAST_YEAR,  " (Temporal)"), "blue",  0.0,  2)
      draw_polys(p_v3, "Buildings V3 (polygons)", "red", 0.0, 1.2)
      
      incProgress(0.10)
      
      incProgress(0.05)
      tryCatch({
        rv$year_area_list <- compute_year_area_list(roi_sf)
      }, error = function(e) {
        rv$year_area_list <- NULL
        status(paste0("Error computing yearly polygons: ", conditionMessage(e)))
      })
      incProgress(0.35)
    })
    
    status("Done. Adjust the size/height sliders to filter map + histograms live.")
  }
  
  # ---------------------------
  # Toggle modes
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
  # Shape click handler (Wards / UMN)
  # ---------------------------
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    if (is.null(click$group)) return()
    
    if (isTRUE(wards_on()) && click$group == "Wards") {
      ward_poly <- wards_sf[wards_sf$WARD_ID_LEAFLET == as.character(click$id), ]
      if (nrow(ward_poly) == 0) return()
      
      clear_map_overlays()
      wards_on(FALSE)
      umn_on(FALSE)
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      
      status(paste0("Ward ", click$id, " selected. Computing…"))
      
      bb <- sf::st_bbox(ward_poly)
      leafletProxy("map") %>% fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
      
      ward_roi <- prep_roi_sf(ward_poly, simplify_m = 5)
      run_pipeline_for_roi(ward_roi, label = paste0("Ward ", click$id))
      return()
    }
    
    if (isTRUE(umn_on()) && click$group == "UMN Areas") {
      u_poly <- umn_sf[umn_sf$UMN_ID_LEAFLET == as.character(click$id), ]
      if (nrow(u_poly) == 0) return()
      
      clear_map_overlays()
      wards_on(FALSE)
      umn_on(FALSE)
      draw_on(FALSE)
      session$sendCustomMessage("toggleDrawControls", list(show = FALSE))
      
      status(paste0("UMN area ", click$id, " selected. Computing…"))
      
      bb <- sf::st_bbox(u_poly)
      leafletProxy("map") %>% fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
      
      u_roi <- prep_roi_sf(u_poly, simplify_m = 5)
      run_pipeline_for_roi(u_roi, label = paste0("UMN ", click$id))
      return()
    }
  })
  
  # ---------------------------
  # ROI draw handler
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
    if (is.null(rv$roi_area_m2) || !is.finite(rv$roi_area_m2)) {
      "ROI area (m²): —"
    } else {
      paste0("ROI area (m²): ", fmt_int(rv$roi_area_m2))
    }
  })
  
  # ---------------------------
  # Summary XLSX Export — single sheet, annual time-series
  # ---------------------------
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("open_buildings_summary_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$roi_sf)
      
      srng <- input$size_range
      if (is.null(srng) || length(srng) != 2) srng <- c(NA_real_, NA_real_)
      
      ts <- ts_filtered()
      if (is.null(ts)) {
        ts <- data.frame(
          year = YEARS,
          building_count = NA_integer_,
          total_built_area_m2 = NA_real_,
          cover_pct = NA_real_
        )
      }
      
      out <- ts
      out$roi_area_m2 <- rv$roi_area_m2
      out$roi_geojson <- rv$roi_geojson
      out$size_min_m2 <- srng[1]
      out$size_max_m2 <- srng[2]
      out$v3_building_count_total_in_roi <- rv$v3_count
      out$presence_threshold <- PRES_THRESH
      
      out <- out[, c(
        "roi_area_m2", "roi_geojson", "size_min_m2", "size_max_m2",
        "year", "building_count", "total_built_area_m2", "cover_pct",
        "v3_building_count_total_in_roi", "presence_threshold"
      )]
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", out)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------
  # Full XLSX Export
  #   Sheet 1 "Buildings"  — all temporal records, no filters
  #   Sheet 2 "V3_control" — V3 2023 records, no filters
  # ---------------------------
  output$download_full_csv <- downloadHandler(
    filename = function() {
      paste0("open_buildings_full_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$roi_sf, rv$year_area_list)
      
      # ---- Sheet 1: temporal building records ----
      yrs <- sort(as.integer(names(rv$year_area_list)))
      
      record_rows <- lapply(yrs, function(y) {
        df <- rv$year_area_list[[as.character(y)]]
        if (is.null(df) || (is.data.frame(df) && nrow(df) == 0)) return(NULL)
        
        a <- if (is.data.frame(df)) df$area_m2 else df
        h <- if (is.data.frame(df)) df$height_m else rep(NA_real_, length(a))
        
        keep <- is.finite(a) & a > 0
        a <- a[keep]; h <- h[keep]
        if (length(a) == 0) return(NULL)
        
        data.frame(
          unique_id = paste0(y, "_", seq_along(a)),
          year      = y,
          area_m2   = a,
          height_m  = h,
          stringsAsFactors = FALSE
        )
      })
      
      record_rows <- Filter(Negate(is.null), record_rows)
      buildings <- if (length(record_rows) > 0) {
        do.call(rbind, record_rows)
      } else {
        data.frame(unique_id = character(), year = integer(),
                   area_m2 = numeric(), height_m = numeric())
      }
      
      # ---- Sheet 2: V3 control records ----
      v3_records <- data.frame(
        unique_id = character(), year = integer(),
        area_m2 = numeric(), height_m = numeric(),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(rv$v3_sf) && nrow(rv$v3_sf) > 0) {
        v3 <- rv$v3_sf  # area_m2 already added by add_area_m2()
        
        a <- v3$area_m2
        height_col_candidates <- c(
          "height_mean_m", "height_median_m", "height_p95_m",
          "height_mean", "height_median", "height_p95",
          "height", "HEIGHT", "building_height", "BUILDING_HEIGHT",
          "bldg_height", "BLDG_HEIGHT", "bldg_hgt", "BLDG_HGT",
          "hgt", "HGT", "height_m", "HEIGHT_M",
          "roof_height", "ROOF_HEIGHT", "mean_height", "MEAN_HEIGHT",
          "mean", "median", "p95"
        )
        hcol <- height_col_candidates[height_col_candidates %in% names(v3)][1]
        h_vec <- if (!is.na(hcol) && !is.null(hcol)) {
          suppressWarnings(as.numeric(v3[[hcol]]))
        } else {
          rep(NA_real_, nrow(v3))
        }
        
        keep <- is.finite(a) & a > 0
        a     <- a[keep]
        h_vec <- h_vec[keep]
        
        if (length(a) > 0) {
          v3_records <- data.frame(
            unique_id = paste0("V3_2023_", seq_along(a)),
            year      = 2023L,
            area_m2   = a,
            height_m  = h_vec,
            stringsAsFactors = FALSE
          )
        }
      }
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Buildings")
      openxlsx::writeData(wb, "Buildings", buildings)
      openxlsx::addWorksheet(wb, "V3_control")
      openxlsx::writeData(wb, "V3_control", v3_records)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
