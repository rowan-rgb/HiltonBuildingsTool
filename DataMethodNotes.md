# 📚 Data Method Notes — Open Buildings (Pre-Downloaded Data)

## How Building Counts and Sizes Are Computed

This app uses **Google Open Buildings Temporal V1 and Open Buildings V3 datasets that have been pre-downloaded and stored locally** in the repository for ward areas.

No Google Earth Engine (GEE) access is used at runtime. All processing is performed locally on the prepared datasets.

Google’s ML models originally detect buildings from satellite imagery and provide either:

- per-pixel building presence probabilities (Temporal V1), or  
- vector footprint polygons (V3).

The datasets included in this project are derived from those sources and prepared in advance for ward-scale analysis.

---

## Temporal V1 Processing (Pre-Derived)

For Temporal V1, the original workflow (performed offline before packaging with the app) was:

- Select the yearly building-presence mosaic
- Use the `building_presence` probability band
- Apply a probability threshold (default = 0.5)
- Convert the thresholded raster to polygons
- Treat each polygon as one detected building object
- Compute polygon area in m² after reprojection to a metric CRS

These derived polygons are what the app reads from the local dataset.

They are used for:

- building counts  
- size histograms  
- size filtering  
- land coverage %  

They are **derived shapes**, not original Google footprint vectors, and are now stored locally rather than generated at runtime.

---

## Scale and Resolution Effects

Raster-to-polygon conversion depends on processing scale (metres per pixel). In the original preprocessing workflow, a working scale equivalent to ~10 m resolution was used.

Scale choice affects results:

**Smaller (finer) scale**

- more detailed polygon boundaries  
- more small buildings/fragments detected  
- higher counts possible  

**Larger (coarser) scale**

- smoother polygons  
- small buildings may merge or disappear  
- lower counts  

Building counts and sizes should therefore be interpreted as **scale-dependent estimates**, not exact cadastral footprints.

Because preprocessing has already been completed, results are now stable and reproducible across users of this app.

---

## Temporal V1 vs V3 Datasets

### Temporal V1 — Open Buildings Temporal

- Multi-year dataset  
- Originally raster probability layers  
- Enables time-series analysis  
- Required threshold + vectorisation (performed offline)  
- Polygons are pre-derived and stored locally in this project  

### V3 — Open Buildings V3 Polygons

- Single release dataset  
- Precomputed building footprint polygons  
- No time dimension  
- Included locally as a comparison/reference layer  

Counts differ between V1 and V3 because the datasets use different models, formats, and post-processing pipelines.

---

## Data Access and Reproducibility

- All Open Buildings data used by the app is **pre-downloaded and stored locally**
- No Google Earth Engine calls are made by the app
- No API keys or cloud credentials are required
- No authentication steps are needed
- All users run the same prepared datasets
- Results are reproducible across machines given the same repository contents

Heavy cloud computation was performed only during the original offline data preparation stage, not during app execution.
