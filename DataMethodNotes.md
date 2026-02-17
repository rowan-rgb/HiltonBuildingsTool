# 📚 Data Method Notes — Google Earth Engine & Open Buildings

## How Building Counts and Sizes Are Computed

This app uses Google Open Buildings datasets via **Google Earth Engine (GEE)**.

Google’s ML models detect buildings from satellite imagery and provide either:
- per-pixel **building presence probabilities** (Temporal V1), or
- **vector footprint polygons** (V3).

For **Temporal V1**, this app:

1. Selects the yearly building-presence mosaic.
2. Uses the `building_presence` band (probability of building per pixel).
3. Applies a threshold (default = 0.5).
4. Converts the thresholded raster to polygons using Earth Engine `reduceToVectors`.
5. Treats each polygon as one detected building object.
6. Computes polygon area in m² after reprojection to a metric CRS.

These derived polygons are used for:
- building counts
- size histograms
- size filtering
- land coverage %

They are runtime-derived shapes, not original Google footprint vectors.

---

## Effect of Scale and the `SCALE_M` Variable

Raster-to-polygon conversion in Earth Engine depends on processing scale (metres per pixel).

The code defines a `SCALE_M` variable to represent a working scale in metres, and polygon extraction uses a fine vectorisation scale internally. Scale choice affects results:

**Smaller (finer) scale**
- more detailed polygon boundaries
- more small buildings/fragments detected
- higher counts possible
- slower processing

**Larger (coarser) scale**
- smoother polygons
- small buildings may merge or disappear
- lower counts
- faster processing

Building counts and sizes should therefore be interpreted as **scale-dependent estimates**, not exact cadastral footprints.

---

## Temporal V1 vs V3 Datasets

### Temporal V1 — `open-buildings-temporal/v1`
- Multi-year dataset
- Raster probability layers
- Enables time-series analysis
- Requires threshold + vectorisation
- Polygons are derived at runtime

### V3 — `open-buildings/v3/polygons`
- Single release dataset
- Precomputed building footprint polygons
- No time dimension
- Used here as a reference comparison layer

Counts differ because the datasets use different models, formats, and post-processing.

---

## Earth Engine Project & Authentication

The app runs through **Google Earth Engine** using the project ID set in `ee_Initialize()`.

- No API keys are stored in this repo
- Authentication happens locally via your Google account
- Each user must run `ee_Initialize()` once
- Users must have Earth Engine access enabled
- Users must be authorised for the configured GEE project (or change it)

All heavy computation runs on Earth Engine servers under the authenticated user’s project.
