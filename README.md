# 🏢 Hilton Buildings Tool — Shiny App

An interactive **R Shiny** application for exploring Google Open Buildings Temporal data within user-defined regions of interest (ROI). Users can draw areas or select predefined boundaries and analyse building counts, sizes, and land coverage over time.

---

## ✨ Features

- 🗺️ Interactive ROI selection
  - Draw custom polygon
  - Select municipal wards
  - Select UMN functional areas
- 🏗️ Google Open Buildings Temporal V1 integration
- 📊 Building size histogram with live filtering
- 📈 Time-series of building counts and land coverage
- 📥 Export filtered results to CSV
- 🧭 Multi-layer interactive map overlays

---

## ⚙️ Technology Stack

- **R Shiny** — application framework
- **Leaflet** — interactive mapping
- **Plotly** — charts
- **sf** — spatial data handling
- **Google Earth Engine** via:
  - `rgee` (R)
  - `earthengine-api` (Python through `reticulate`)
- Local shapefiles included in the repository

---

# 🚀 Running the App Locally

## 1️⃣ Prerequisites

Install the following:

- R (recommended version 4.2 or newer)
- RStudio (recommended)
- Python 3 (recommended version 3.10 or newer)

Check Python in Terminal:

    python3 --version

---

## 2️⃣ Get the Code

Clone with Git:

    git clone https://github.com/rowan-rgb/HiltonBuildingsTool.git
    cd HiltonBuildingsTool

Alternative without git:

- Open the GitHub repository page
- Click **Code → Download ZIP**
- Unzip the folder
- Open the folder in RStudio

---

## 3️⃣ Install Required R Packages

Run in R:

    install.packages(c(
      "shiny",
      "leaflet",
      "leaflet.extras",
      "sf",
      "jsonlite",
      "geojsonsf",
      "htmlwidgets",
      "plotly",
      "reticulate",
      "rgee"
    ))

If the **sf** package fails on macOS, run in Terminal:

    xcode-select --install

Then reinstall `sf`.

---

## 4️⃣ Confirm Shapefiles (Already Included)

This repository already contains the required shapefiles.

Expected paths:

- data/wards/Municipal_Wards_2021.shp
- data/umn/Analysis regions/UMN_Functional_Areas_1.shp

Each shapefile must include companion files in the same folder (`.shx`, `.dbf`, usually `.prj`).

Quick check in R:

    file.exists("data/wards/Municipal_Wards_2021.shp")
    file.exists("data/umn/Analysis regions/UMN_Functional_Areas_1.shp")

Both should return `TRUE`.

---

## 5️⃣ Google Earth Engine Setup (One-Time)

You must have Google Earth Engine enabled for your Google account.

Run once in R:

    library(rgee)
    ee_Initialize()

A browser window will open — log in and authorise access.

---

## 6️⃣ Python + Earth Engine API (Automatic)

The app automatically manages Python using **reticulate**.

On first run it will:

- create a virtual environment named `r-reticulate`
- install the Python package `earthengine-api` if missing
- bind R to that environment

The first startup may take a few minutes while dependencies install.

---

## 7️⃣ Start the App

In RStudio:

- Open `app.R`
- Click **Run App**

Or run in R:

    shiny::runApp("app.R")

You should see:

    Listening on http://127.0.0.1:XXXX

Open that address in your browser.

---

# 🛠️ Troubleshooting

## Shapefile errors

- Ensure `.shp`, `.shx`, and `.dbf` files are present
- Confirm folder structure matches the repo paths
- Do not change the relative paths in `app.R`

---

## Earth Engine authentication problems

Re-run:

    library(rgee)
    ee_Initialize()

---

## Reticulate / Python issues

Check which Python R is using:

    library(reticulate)
    py_config()

Rebuild the environment if needed:

    virtualenv_remove("r-reticulate")

Restart R and run the app again.

---

# 👥 Collaboration Notes

- All file paths are **relative to the project directory**
- Shapefiles are versioned with the repository
- Python environment is created automatically per machine
- First run is slower due to Python + Earth Engine setup
- No path editing should be required if the repo structure is unchanged
