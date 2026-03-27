# Tiled Rasterization Plan

## Goal

Make polygon-to-raster conversion more robust for very large source databases and large output rasters by processing the raster in spatial tiles rather than as one monolithic job.

Primary objective:

- avoid RAM pressure
- reduce restart cost when a long job fails
- keep all heavy steps disk-backed

## Current Baseline

The new materialized rasterization path in [R/raterize_sf_gdal.R](R/raterize_sf_gdal.R) already improves robustness by:

- materializing the source vector layer to a temporary GeoPackage on disk
- materializing small lookup tables on disk
- rasterizing numeric, character, date, and burn fields through GDAL
- avoiding large in-memory `sf` objects and long SQL mapping expressions

This is a good baseline, but the whole extent is still processed in one pass.

## Proposed Tiled Approach

1. Derive the global raster grid from `reference` or from explicit `te` and `tr`.
2. Split the full extent into rectangular tiles.
3. For each tile:
   - compute that tile extent
   - rasterize the requested fields only for that extent
   - write one raster per tile, or one multiband tile raster
4. Build a final mosaic using either:
   - a VRT, or
   - a final merge step into a single raster if needed

## Why Tiling Helps

- limits per-job memory and GDAL cache pressure
- reduces temporary file growth per step
- makes failures restartable at tile level
- makes progress observable
- enables future parallelization if desired

## Recommended Design

### 1. Tile Grid Helper

Create a helper that returns a table of tile extents:

- `tile_id`
- `xmin`
- `ymin`
- `xmax`
- `ymax`
- optional row and column indices

The tiling unit should be expressed in raster cells, not only map units.

Recommended parameters:

- `tile_ncol`
- `tile_nrow`

This keeps every tile aligned with the final raster grid.

### 2. Reuse the Materialized Vector Source

Do not rebuild the temporary GeoPackage for every tile.

Preferred flow:

1. materialize the source vector once
2. reuse that same temporary GeoPackage for all tile rasterization calls

This avoids repeated expensive vector translation.

### 3. Rasterize One Tile at a Time

For each tile, pass tile-specific `-te` values into GDAL rasterization.

That means each tile writes only a limited spatial window.

### 4. Output Strategy

Two good output modes:

#### Mode A: VRT-first

- write tile rasters
- build one VRT per layer or one multiband VRT stack
- stop there if downstream tools can consume VRT

Pros:

- fastest final assembly
- avoids another full raster rewrite

Cons:

- output is not a single self-contained raster

#### Mode B: Final merged raster

- write tile rasters
- mosaic tiles into a final raster per layer
- stack layers into the final product

Pros:

- produces a normal final raster file

Cons:

- more IO

## Failure Recovery

Store tile outputs with deterministic names, for example:

- `layername_tile_0001.tif`
- `layername_tile_0002.tif`

Then a rerun can:

- skip completed tiles
- rerun only missing or failed tiles

Useful controls:

- `overwrite_tiles = FALSE`
- `overwrite_final = FALSE`

## Suggested Function Shape

Possible future functions:

- `make_raster_tiles()`
- `rasterize_sf_gdal_materialized_tiled()`
- `build_tile_vrt()`
- `merge_tile_rasters()`

Likely high-level API:

```r
rasterize_sf_gdal_materialized_tiled(
  src_datasource,
  dst_dir,
  layer = NULL,
  reference = NULL,
  numeric_attributes = NULL,
  character_attributes = NULL,
  date_attributes = NULL,
  factor_conv_list = NULL,
  burn = NULL,
  tile_ncol = 2048,
  tile_nrow = 2048,
  build_vrt = TRUE,
  build_final_raster = FALSE,
  overwrite_tiles = FALSE,
  verbose = TRUE
)
```

## Important Implementation Notes

### Keep Field Pruning

The materialized source should continue to copy only needed fields, not `SELECT *`.

### Preserve Grid Alignment

All tile extents must align exactly with the reference raster grid.

### Keep Temporary Data on Fast Disk

Large runs will create significant temporary IO. Prefer a fast local SSD temp directory.

### Consider GDAL Cache Settings

For very large runs, a future enhancement could expose GDAL config options such as cache size.

## Recommended Order of Implementation

1. Add tile grid helper.
2. Add per-tile rasterization loop using the existing materialized source.
3. Add resumable tile naming and skip logic.
4. Add VRT build step.
5. Optionally add final full-raster merge step.

## Success Criteria

The tiled workflow is successful if it:

- runs on datasets larger than RAM
- completes without loading the full source vector into R memory
- can resume after interruption
- produces either a usable VRT or a final raster identical in grid and values to the non-tiled path