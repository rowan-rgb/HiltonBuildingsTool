# Hilton Buildings Tool — Shiny App

An interactive R Shiny application for exploring Google Open Buildings Temporal data within user-defined regions of interest (ROI). Users can draw areas or select predefined boundaries and analyse building counts, sizes, and land coverage over time.

This version of the app uses **pre-downloaded Open Buildings Temporal V1 and V3 datasets for ward areas**. All Google Earth Engine access and initialisation code has been removed. No Earth Engine setup, authentication, or Python environment is required.

---

## Features

- Interactive ROI selection
  - Draw custom polygon
  - Select municipal wards
  - Select UMN functional areas
- Open Buildings Temporal V1 and V3 datasets loaded from local files
- Building size histogram with live filtering
- Time-series of building counts and land coverage
- Export filtered results to CSV
- Multi-layer interactive map overlays

---

## Technology Stack

- R Shiny — application framework
- Leaflet — interactive mapping
- Plotly — charts
- sf — spatial data handling
- dplyr / readr — data processing
- Local datasets — Open Buildings Temporal V1 & V3 (pre-downloaded)
- Local shapefiles — wards and UMN functional areas included in the repository

No Google Earth Engine, rgee, reticulate, or Python dependencies are required.

---

## Running the App Locally

### Prerequisites

Install:

- R (recommended version 4.2 or newer)
- RStudio (recommended)

Check R version:

```r
R.version.string
```

---

### Get the Code

Clone with Git:

```bash
git clone https://github.com/rowan-rgb/HiltonBuildingsTool.git
cd HiltonBuildingsTool
```

Or without git:

1. Open the GitHub repository page
2. Click Code → Download ZIP
3. Unzip the folder
4. Open the folder in RStudio

---

### Install Required R Packages

Run in R:

```r
install.packages(c(
  "shiny",
  "leaflet",
  "leaflet.extras",
  "sf",
  "jsonlite",
  "geojsonsf",
  "htmlwidgets",
  "plotly",
  "dplyr",
  "readr"
))
```

If the sf package fails on macOS, run in Terminal:

```bash
xcode-select --install
```

Then reinstall sf:

```r
install.packages("sf")
```

---

### Confirm Boundary Shapefiles (Included)

The repository already contains the required shapefiles.

Expected paths:

```
data/wards/Municipal_Wards_2021.shp
data/umn/Analysis regions/UMN_Functional_Areas_1.shp
```

Each shapefile must include companion files in the same folder (.shp, .shx, .dbf, usually .prj).

Quick check in R:

```r
file.exists("data/wards/Municipal_Wards_2021.shp")
file.exists("data/umn/Analysis regions/UMN_Functional_Areas_1.shp")
```

Both should return TRUE.

---

### Confirm Open Buildings Data (Included)

The repository includes pre-downloaded Open Buildings Temporal V1 and V3 data for ward areas. These datasets are read directly by the app.

- No online data access required
- No authentication required
- No API setup required

Do not rename or move these files unless you also update the paths in app.R.

---

### Start the App

In RStudio:

1. Open app.R
2. Click Run App

Or run from the R console:

```r
shiny::runApp("app.R")
```

You should see:

```
Listening on http://127.0.0.1:XXXX
```

Open that address in your browser.

---

## Troubleshooting

### Shapefile errors

- Ensure .shp, .shx, and .dbf files are present
- Confirm folder structure matches the repository paths
- Do not change relative paths in app.R

### Data not loading

- Check that Open Buildings V1 and V3 data files are present in the expected folders
- Confirm filenames have not been changed
- Restart R and rerun the app

### sf package install problems

macOS:

```bash
xcode-select --install
```

Then reinstall:

```r
install.packages("sf")
```

Windows:

Install RTools if prompted during package installation.

---

## Collaboration Notes

- All file paths are relative to the project directory
- Boundary shapefiles are versioned with the repository
- Open Buildings V1 & V3 ward datasets are stored locally in the repo
- No Google Earth Engine or Python setup is required
- No path editing should be required if the repository structure is unchanged

---

## Version Note

This README reflects the local-data version of the app where Earth Engine access has been removed and replaced with pre-downloaded datasets for ward-scale analysis.
