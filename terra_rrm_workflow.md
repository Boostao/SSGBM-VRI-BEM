# Terra RRM Workflow

This document explains how to use the new raster-native terra workflow in `SSGBM-VRI-BEM`.

The terra path is separate from the legacy polygon workflow. It assumes your rasters are already aligned on disk and that any derived support rasters you need, such as `wl_pct`, `MEAN_SLOPE`, `ABOVE_ELEV_THOLD`, or `PROJ_AGE_1`, have already been created.

## What is available

The new public entry points are:

- `terra_rrm_create_RRM_ecosystem_from_rasters()`
- `terra_rrm_prepare_ecosystem_stack()`
- `terra_rrm_create_RRM_ecosystem()`
- `terra_rrm_create_RRM_ecosystem_moose()`
- `terra_rrm_create_RRM_ecosystem_bear()`

Use them in one of two ways:

1. Disk-first workflow: start from raster file paths.
2. Advanced workflow: start from an already assembled `SpatRaster` stack.

## Option 1: Disk-first workflow

Use `terra_rrm_create_RRM_ecosystem_from_rasters()` when you already have aligned raster files on disk.

### Required raster inputs

- `vri_dsn`: aligned VRI raster
- `bem_dsn`: aligned BEM raster
- `rivers_dsn`: aligned rivers raster
- `wetlands_dsn`: aligned wetlands raster

### Optional raster inputs

- `lakes_dsn`: aligned lakes raster for the small-lakes stage
- `ccb_dsn`: aligned cutblock raster
- `elevation_dsn`: aligned elevation raster

### Required supporting tables

- `rules_xl`: BEU rules workbook or in-memory rules table
- `beu_bec_csv`: allowed BEC/BEU lookup CSV
- `beu_wetland_update_csv`: wetland lookup CSV
- `unique_ecosystem`: unique ecosystem CSV

### Example

```r
library(SSGBM.VRI.BEM)

moose_rrm <- terra_rrm_create_RRM_ecosystem_from_rasters(
  vri_dsn = "rasters/vri.tif",
  bem_dsn = "rasters/bem.tif",
  rivers_dsn = "rasters/rivers.tif",
  wetlands_dsn = "rasters/wetlands.tif",
  lakes_dsn = "rasters/lakes.tif",
  ccb_dsn = "rasters/ccb.tif",
  rules_xl = "tables/beu_rules.xlsx",
  beu_bec_csv = "tables/Allowed_BEC_BEUs_NE_ALL.csv",
  beu_wetland_update_csv = "tables/beu_wetland_updates.csv",
  unique_ecosystem = "tables/Skeena_VRIBEM_LUT.csv",
  most_recent_harvest_year = 2024,
  kind = "moose"
)
```

### What it does internally

1. Reads and stacks the aligned rasters.
2. Reads the wetland and unique ecosystem support tables.
3. Runs the terra preparation stages in order.
4. Returns the requested moose and/or bear summary tables.

## Option 2: Start from an existing `SpatRaster`

Use this path when you already have a combined terra stack in memory or on disk.

### Step 1: Prepare the stack

Call `terra_rrm_prepare_ecosystem_stack()`.

```r
prepared <- terra_rrm_prepare_ecosystem_stack(
  x = input_stack,
  buc = data.table::fread("tables/beu_wetland_updates.csv"),
  beu_bec = data.table::fread("tables/Allowed_BEC_BEUs_NE_ALL.csv"),
  rules_dt = "tables/beu_rules.xlsx",
  unique_ecosystem_dt = read_unique_ecosystem_dt("tables/Skeena_VRIBEM_LUT.csv"),
  most_recent_harvest_year = 2024
)
```

This runs the staged terra corrections and enrichments in the current implementation order.

### Step 2: Create the requested summaries

Call `terra_rrm_create_RRM_ecosystem()` if you want preparation plus export in one call from an already assembled stack.

```r
outputs <- terra_rrm_create_RRM_ecosystem(
  x = input_stack,
  buc = data.table::fread("tables/beu_wetland_updates.csv"),
  beu_bec = data.table::fread("tables/Allowed_BEC_BEUs_NE_ALL.csv"),
  rules_dt = "tables/beu_rules.xlsx",
  unique_ecosystem_dt = read_unique_ecosystem_dt("tables/Skeena_VRIBEM_LUT.csv"),
  most_recent_harvest_year = 2024,
  kind = c("moose", "bear"),
  return_stack = TRUE
)
```

If `kind` has length 1 and `return_stack = FALSE`, the function returns a single summary table.

If you request both summaries or set `return_stack = TRUE`, the function returns a named list.

## Current step order

`terra_rrm_prepare_ecosystem_stack()` currently applies these stages:

1. `terra_rrm_correct_bem_from_vri()`
2. river adjacency update from the aligned `rivers` raster
3. `terra_rrm_correct_small_lakes()` if lake processing is enabled
4. `terra_rrm_correct_bem_from_wetlands()`
5. `terra_rrm_correct_bem_from_wetlands_riparian_stage()`
6. `terra_rrm_apply_rules()`
7. `terra_rrm_calc_forest_age_class()`
8. `terra_rrm_merge_unique_ecosystem_fields()`
9. `terra_rrm_find_crown_area_dominant_values()`

After that, export uses:

1. `terra_rrm_create_RRM_ecosystem_moose()`
2. `terra_rrm_create_RRM_ecosystem_bear()`

## How to use each stage conceptually

### VRI correction stage

Purpose: apply the raster-native BEM corrections driven by VRI attributes, including stand updates and allowed BEC/BEU corrections.

Key expectation: your stack already contains the BEM/VRI fields required by the stage.

Supporting table: `beu_bec` or `beu_bec_csv` for the allowed BEC/BEU correction lookup.

### Small-lakes stage

Purpose: classify connected lake patches into `OW`, `LS`, or `LL` using raster patch area.

Key expectation: you provide an aligned lake raster and set `apply_small_lakes = TRUE`.

### Wetland correction stage

Purpose: convert wetland percentages into BEU wetland transitions using the wetland lookup CSV.

Key expectation: the stack already contains `wl_pct`.

### Riparian wetland stage

Purpose: apply the riparian reassignment that follows the main wetland correction step.

Key expectation: the stack includes `SITE_M3A`, `MEAN_SLOPE`, and `BGC_ZONE`.

### Rules stage

Purpose: apply the BEU rules workbook, including tree-rule expressions.

Key expectation: rule input columns must exist in the aligned raster stack.

### Forest age stage

Purpose: update `PROJ_AGE_1` from disturbance layers and compute `VRI_AGE_CL_STS` and `VRI_AGE_CL_STD`.

Key expectation: the stack contains `PROJ_AGE_1` and any disturbance-year layers you want to use.

### Unique ecosystem merge stage

Purpose: join lookup attributes from the unique ecosystem table and derive `STRCT_S1:3` and `STAND_A1:3`.

Key expectation: the unique ecosystem CSV has already been formatted with `read_unique_ecosystem_dt()`.

### Crown dominance stage

Purpose: populate the component crown layers used by the export stage.

Key expectation: the stack contains `CROWN_ALL`, `FORESTED_1:3`, and `STRCT_S1:3`.

### Moose and bear export stage

Purpose: summarize the prepared raster stack into RRM-ready area tables.

Use:

```r
moose <- terra_rrm_create_RRM_ecosystem_moose(prepared)
bear <- terra_rrm_create_RRM_ecosystem_bear(prepared)
```

The bear export differs by including the `Salmon` grouping key.

## Practical recommendations

1. Use `terra_rrm_create_RRM_ecosystem_from_rasters()` if your inputs already exist as aligned rasters on disk.
2. Use `terra_rrm_prepare_ecosystem_stack()` when you want to inspect or save the intermediate prepared stack before export.
3. Keep layer names stable. The terra path relies on expected layer names.
4. Make sure categorical rasters have the correct level tables when you create them outside the package.
5. Supply the same support tables as the legacy workflow, especially the allowed BEC/BEU lookup and wetland lookup tables.