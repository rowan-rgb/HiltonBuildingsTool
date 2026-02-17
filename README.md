Hilton Buildings Tool — Shiny App

This repository contains a single-file R Shiny application (app.R) for exploring Google Open Buildings Temporal data within user-selected regions of interest.

The app uses:

Shiny + Leaflet + Plotly for the interface and visualisation

Google Earth Engine via rgee (R) and earthengine-api (Python through reticulate)

Local shapefiles included in this repository under the data/ folder

Running the App Locally
1️⃣ Prerequisites

Please install the following on your computer:

R (recommended: version 4.2 or newer)

RStudio (recommended)

Python 3 (recommended: version 3.10 or newer)

Check Python in Terminal:

python3 --version

2️⃣ Clone the Repository

Using Terminal:

git clone https://github.com/rowan-rgb/HiltonBuildingsTool.git
cd HiltonBuildingsTool


Alternative (no git):

Open the GitHub repo page

Click Code → Download ZIP

Unzip the folder

Open the folder in RStudio

3️⃣ Install Required R Packages

Open R or RStudio and run:

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


If the sf package fails to install on macOS, run:

xcode-select --install


Then reinstall sf.

4️⃣ Confirm Shapefiles Exist

The required shapefiles are already included in this repository.

Expected paths:

data/wards/Municipal_Wards_2021.shp
data/umn/Analysis regions/UMN_Functional_Areas_1.shp


A shapefile consists of multiple companion files. Each .shp should have at least:

.shp

.shx

.dbf

usually .prj

Quick check in R:

file.exists("data/wards/Municipal_Wards_2021.shp")
file.exists("data/umn/Analysis regions/UMN_Functional_Areas_1.shp")


Both should return:

TRUE

5️⃣ Google Earth Engine Setup (One-Time Authentication)

You must have a Google account with Earth Engine access.

Run once in R:

library(rgee)
ee_Initialize()


A browser window will open. Log in and complete the Earth Engine authorisation process.

6️⃣ Python + Earth Engine API (Automatic)

The app automatically manages Python using reticulate.

On first run, app.R will automatically:

create a Python virtual environment named r-reticulate (if missing)

install the Python package earthengine-api (if missing)

use this environment for Earth Engine access

⚠️ The first startup may take a few minutes while Python packages install.

7️⃣ Run the App

In RStudio:

Open app.R

Click Run App

Or run from the console:

shiny::runApp("app.R")


You should see:

Listening on http://127.0.0.1:XXXX


Open that URL in your browser to use the app.

Troubleshooting
Shapefile read errors

Confirm the .shp, .shx, and .dbf files are present

Confirm the folder structure matches the paths above

Earth Engine authentication issues

Re-run:

library(rgee)
ee_Initialize()


and complete login again.
