################################################################################
# Google Earth Engine (GEE) downloads in RStudio using rgee + reticulate
# ---------------------------------------------------------------------
# This script:
#   1) Forces R/reticulate to use a specific Python that has the EE API - 
#      this is specific to my computer requirements!
#   2) Authenticates + initialises my Earth Engine session (this is the
#      temperamental bit: you may need to re-authenticate fairly often).

#      Note: ccee-486908 is my project area and we are using my authenitcation key too!!

#   3) Builds a Region of Interest (ROI) from a ward shapefile.
#   4) Exports Open Buildings v3 polygons for the ROI to Google Drive.
#   5) Exports Open Buildings temporal v1 building footprints (2016–2023)
#      as polygons, one year at a time, to Google Drive.
#
# Where outputs go:
#   Google Drive -> folder "EE_exports" (created if it doesn't exist).
#
# Notes about your environment being temperamental:
#   - rgee uses the Python Earth Engine API under the hood (via reticulate).
#   - If your auth token expires, if Google revokes it, or if you accidentally
#     switch Python environments, you’ll get auth errors and must run
#     ee_Authenticate() again.
#   - Keep the Python path stable across sessions; changing it is a common
#     cause of “credential missing/expired” prompts.
################################################################################


################################################################################
# 1) Force a known Python (reticulate)
################################################################################

# RETICULATE_PYTHON pins reticulate to a single Python interpreter.
# This matters because EE credentials + installed modules are tied to the Python
# environment. If reticulate silently uses a different Python, rgee may fail
# with auth errors or missing packages.
Sys.setenv(
  RETICULATE_PYTHON = "/Users/rowandavies/Library/Caches/org.R-project.R/R/reticulate/uv/cache/archive-v0/QuaHw5TiV-JHOMAB6Uq3P/bin/python"
)

library(reticulate)
library(rgee)

# IMPORTANT:
# If RETICULATE_PYTHON is set, do NOT call use_virtualenv() / use_condaenv().
# Those calls will be ignored and can create confusing “it says it’s using X
# but it’s actually using Y” situations.
#
# Example (do NOT do this here):
# use_virtualenv("r-reticulate", required = TRUE)


################################################################################
# 2) Optional SSL fix (only if needed)
################################################################################

# Occasionally macOS + Python environments fail SSL verification during EE auth:
#   CERTIFICATE_VERIFY_FAILED
#
# This block does NOT install anything. It only:
#   - checks if certifi exists in the current Python, and
#   - if yes, tells Python/requests to use certifi's CA bundle for SSL.
#
# NOTE: We use reticulate::import() rather than py_eval("import ...") because
# py_eval() expects a single Python *expression* (imports are statements).
if (py_module_available("certifi")) {
  certifi  <- import("certifi")
  ca_path  <- certifi$where()
  Sys.setenv(SSL_CERT_FILE = ca_path, REQUESTS_CA_BUNDLE = ca_path)
}

################################################################################
# 3) Sanity checks (recommended)
################################################################################

# Print the Python configuration reticulate is actually using.
# If this path changes between sessions, expect to re-authenticate.
py_config()

# Confirm Earth Engine API can be imported and show its version.
py_run_string("import ee; print('ee version:', ee.__version__)")


################################################################################
# 4) Authenticate + initialise Earth Engine (THIS IS THE TEMPERAMENTAL PART)
################################################################################

# ee_Authenticate():
# - Opens a browser URL for Google OAuth
# - You paste a verification code back into the R console
# - Credentials are cached locally for re-use
#
# When you may need to re-run ee_Authenticate():
# - token expires / refresh token invalidated
# - you changed Python environments (even accidentally)
# - you cleared R/reticulate caches or EE credentials
# - Google security/account changes revoke access
#
# If you get "credential expired" or "please authorise" errors:
#   ee_Authenticate()
#   ee_Initialize(project = "ccee-486908")
ee_Authenticate()

# ee_Initialize() connects rgee to your EE account + project.
# The project is used for quota/billing context depending on your EE setup.
ee_Initialize(project = "ccee-486908")


################################################################################
# 5) Read + prepare ROI from shapefile (sf)
################################################################################

library(sf)

# Read your ward boundaries (local data).
wards <- st_read("data/wards/Municipal_Wards_2021.shp", quiet = TRUE)

# Earth Engine expects lon/lat coordinates (WGS84 / EPSG:4326).
wards <- st_transform(wards, 4326)

# Drop Z and M dimensions if present (EE conversion can fail with Z/M geometries).
wards <- st_zm(wards, drop = TRUE, what = "ZM")

# Fix invalid polygons (common in administrative boundary data).
wards <- st_make_valid(wards)

# Ensure consistent geometry type. EE generally expects polygons/multipolygons.
wards <- st_cast(wards, "MULTIPOLYGON", warn = FALSE)

# Merge all wards into one ROI geometry (single multipolygon).
wards_union <- st_union(wards)

# Quick check of object class.
class(wards_union)

# Convert sf geometry to an EE object.
# This makes it usable in filterBounds(), reduceToVectors(), etc.
roi_ee <- sf_as_ee(wards_union)


################################################################################
# 6) Export Open Buildings v3 polygons (vector dataset)
################################################################################

# This FeatureCollection contains building footprint polygons.
# We filter to anything intersecting the ROI.
v3_fc <- ee$FeatureCollection("GOOGLE/Research/open-buildings/v3/polygons")$
  filterBounds(roi_ee)

# Export to Google Drive as GeoJSON.
# - description: becomes the task name (and often filename-like label)
# - folder: Drive folder name where EE will put the output
task_v3 <- ee_table_to_drive(
  collection  = v3_fc,
  description = "openbuildings_v3_wards_roi",
  folder      = "EE_exports",
  fileFormat  = "GeoJSON"
)
task_v3$start()


################################################################################
# 7) Export Open Buildings Temporal v1 (raster -> polygons per year)
################################################################################

# The temporal dataset is an ImageCollection with an "inference_time_epoch_s"
# property indicating the model inference timestamp.
#
# Your method:
#   - pick a representative date per year (June 30)
#   - convert it to epoch seconds
#   - filter the collection for that epoch
#   - mosaic (in case multiple images exist)
#   - threshold building_presence
#   - vectorise the mask -> polygons
#   - export each year as GeoJSON
epoch_s_for_year <- function(year) {
  as.numeric(as.POSIXct(sprintf("%d-06-30 00:00:00", year),
                        tz = "America/Los_Angeles")) %/% 1
}

temporal_col <- ee$ImageCollection("GOOGLE/Research/open-buildings-temporal/v1")

YEARS  <- 2016:2023
THRESH <- 0.5  # building presence threshold (0–1). Higher = fewer polygons.

for (y in YEARS) {
  
  epoch <- epoch_s_for_year(y)
  
  # Select the image matching the epoch; mosaic is robust if multiple tiles exist.
  img <- temporal_col$
    filter(ee$Filter$eq("inference_time_epoch_s", epoch))$
    mosaic()
  
  # Create a binary mask of likely building pixels and hide zeros.
  pres_mask <- img$
    select("building_presence")$
    unmask(0)$
    gt(THRESH)$
    selfMask()
  
  # Convert masked pixels to vector polygons.
  # Key parameters:
  # - geometry: limits vectorisation to ROI
  # - scale: spatial resolution in metres (smaller = more detail + heavier compute)
  # - maxPixels/tileScale/bestEffort: help avoid memory/time failures on big areas
  vec <- pres_mask$reduceToVectors(
    geometry       = roi_ee,
    scale          = 4,            # match your app settings
    geometryType   = "polygon",
    eightConnected = TRUE,
    labelProperty  = "mask",
    maxPixels      = 1e13,
    tileScale      = 4,
    bestEffort     = TRUE
  )
  
  # Export each year's polygons to Drive.
  task <- ee_table_to_drive(
    collection  = vec,
    description = paste0("openbuildings_temporal_v1_", y, "_wards_roi_thr", THRESH),
    folder      = "EE_exports",
    fileFormat  = "GeoJSON"
  )
  
  task$start()
}


################################################################################
# 8) Monitor + manage tasks
################################################################################

# Shows task status (RUNNING / COMPLETED / FAILED etc).
ee_monitoring()

# Interactive-ish task management (list/cancel/inspect), depending on rgee version.
ee_manage_task()


################################################################################
# Troubleshooting quick notes
################################################################################
# If EE asks you to re-authorise again:
#   ee_Authenticate()
#   ee_Initialize(project = "ccee-486908")
#
# If auth keeps breaking, double-check you are always using the same Python:
#   py_config()
#
# If you see SSL certificate errors again, keep the certifi block enabled.
################################################################################