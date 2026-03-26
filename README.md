# Hilton Buildings Tool Shiny App

An interactive R Shiny application for exploring Google Open Buildings Temporal data within user-defined regions of interest (ROI). Users can draw areas or select predefined boundaries and analyse building counts, sizes, heights, and land coverage over time.

This version of the app uses **pre-downloaded Open Buildings Temporal V1 and V3 datasets**, processed locally and loaded on demand. No Google Earth Engine, authentication, or Python environment is required.

---

## Features

- **Interactive ROI selection**
  - Draw custom polygon or rectangle
  - Select municipal wards
  - Select UMN functional areas  

- **Local data processing (no cloud dependencies)**
  - GeoJSON files are loaded lazily (on demand) for performance  
  - All clipping and analysis performed locally using `sf`

- **Building analysis tools**
  - Building size histogram (mÂ²)
  - Building height histogram (m)
  - Interactive **size and height sliders** for filtering
  - Live filtering applied to:
    - Map layers  
    - Histograms  
    - Time-series outputs  

- **Time-series analytics (2016â€“2023)**
  - Building counts  
  - Total built area  
  - Percentage land coverage  

- **Multi-layer interactive map**
  - Temporal V1 (first and last year)
  - V3 dataset overlay (2023)
  - ROI and boundary layers  

- **Data export**
  - Export filtered time-series results to CSV  

- **Area-specific data support**
  - Ward-level and functional area shapefiles included and used directly  

---

## Data Notes

### Boundary Data

The application uses **area-specific shapefiles included in the repository**:

- Municipal ward boundaries  
- UMN functional areas  

These are used for:
- ROI selection  
- Structuring and clipping building datasets  

---

### Open Buildings Data (V1 vs V3)

#### Data Structure

- **Temporal V1 dataset (2016â€“2023)**
  - Stored as yearly GeoJSON files  
  - Includes building polygons with:
    - Footprint geometry  
    - Derived area (calculated in-app if not present)  
    - Height attributes (where available)

- **V3 dataset (2023 snapshot)**
  - Single GeoJSON layer  
  - Improved building delineation  

---

### Building Extraction Characteristics

- Building sizes and counts in the **V1 dataset** are derived from raster-based detection outputs.  
  - Closely spaced buildings may be merged  
  - This can lead to undercounting in dense areas  

- The **V3 dataset** improves building separation  
  - Better at identifying distinct structures  
  - More suitable for dense urban analysis  

---

### Limitations (Both Datasets)

- No classification of building type  
  - Cannot distinguish residential, commercial, or informal structures  
- Smaller structures may be included:
  - Sheds  
  - Outbuildings  
  - Small non-residential features  

This can lead to:
- Over-counting in some contexts  
- Inclusion of buildings not relevant to specific analyses  

---

### User-Controlled Filtering

To address these limitations, the app provides:

- **Building size filters (mÂ²)**
- **Building height filters (m)**

These allow users to:

- Remove small or irrelevant structures  
- Focus on buildings of interest  
- Improve comparability between datasets  

Filtering is applied **live across all outputs**.

---

## Technology Stack

- R Shiny â€” application framework  
- Leaflet + leaflet.extras â€” interactive mapping and drawing tools  
- Plotly â€” histograms and interactive charts  
- sf â€” spatial processing (clipping, geometry operations)  
- geojsonsf / jsonlite â€” GeoJSON handling  
- dplyr / readr â€” data processing  

---

## Project Structure

```
HiltonBuildingsTool/
â”‚
â”œâ”€â”€ app.R
â”œâ”€â”€ Data/
â”‚   â”œâ”€â”€ V1 Temporal_large/
â”‚   â”œâ”€â”€ V3 Shapefile_large/
â”‚   â”œâ”€â”€ Wards/
â”‚   â””â”€â”€ Regions/
â””â”€â”€ README.md
```

---

## Running the App Locally

### Prerequisites

- R (â‰¥ 4.2 recommended)  
- RStudio (recommended)

### Install Required Packages

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

### Run the App

```r
shiny::runApp("app.R")
```

---

## Version Note

This README reflects the fully local, lazy-loaded version of the Hilton Buildings Tool.
