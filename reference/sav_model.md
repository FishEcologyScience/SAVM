# Apply SAV prediction models

Apply SAV prediction models to predict SAV cover and presence/absence,
with optional post-hoc processing.

## Usage

``` r
sav_model(
  dat,
  method_pa = "rf",
  method_cover = method_pa,
  pa_threshold = 0.5,
  depth = "depth",
  fetch = "fetch",
  substrate = "substrate",
  secchi = "secchi",
  limitation = "limitation",
  vmax_par = list(intercept = 1.4, slope = 1.33)
)
```

## Arguments

- dat:

  `data.frame`\|`sf`  
  A `data.frame` or `sf` object containing depth and fetch data
  (required), and optionally secchi, substrate, and limitation columns
  for post-hoc adjustments. Additional columns will be ignored.

- method_pa:

  `character`  
  Statistical method for presence/absence model. One of `"rf"` (Random
  Forest), `"gam"` (Generalized Additive Model), or `"lmm"` (Linear
  Mixed Model). Default is `"rf"`.

- method_cover:

  `character`  
  Statistical method for cover model. One of `"rf"` (Random Forest),
  `"gam"` (Generalized Additive Model), or `"lmm"` (Linear Mixed Model).
  Default is the same as `method_pa`.

- pa_threshold:

  `numeric`  
  Probability threshold (0-1) for converting presence/absence
  predictions to binary values. Values below the threshold are
  classified as absent (0), values at or above as present (1). Default
  is 0.5.

- depth:

  `character` (required)  
  Name of the column containing depth data (in meters). Default is
  `"depth"`.

- fetch:

  `character` (required)  
  Name of the column containing fetch data (in kilometers). Default is
  `"fetch"`.

- substrate:

  `character` (optional)  
  Name of the column containing substrate limitation data (optional).
  Binary indicator (0 = no limitation, 1 = limited) for substrate
  constraints. Default is `"substrate"`. Set to `NULL` to disable
  substrate-based post-hoc adjustments.

- secchi:

  `character` (optional)  
  Name of the column containing Secchi depth data in meters (optional).
  Used to calculate maximum colonization depth (Vmax) via the Chambers
  and Kalff equation. Default is `"secchi"`. Set to `NULL` to disable
  Vmax calculations.

- limitation:

  `character` (optional)  
  Name of the column containing user-supplied limitation data
  (optional). Binary indicator (0 = no limitation, 1 = limited) for
  additional constraints. Default is `"limitation"`. Set to `NULL` to
  disable user-supplied limitation adjustments.

- vmax_par:

  `named list` (required `secchi`)  
  Named list with `intercept` and `slope` parameters for the Chambers
  and Kalff (1985) equation to compute maximum depth of plant
  colonization (Vmax). Default is `list(intercept = 1.40, slope = 1.33)`
  (Model A: Quebec + international lakes). See *Details* for Model B
  parameters.

## Value

A data frame (or `sf` object) containing the input columns along with
model predictions. Column names are standardized to `depth_m` and
`fetch_km`.

The following prediction columns are always returned:

- `pa_prob`: Raw presence/absence probability predictions (0-1).

- `pa_pred`: Binary presence/absence classification based on
  `pa_threshold`.

- `cover_pred`: Raw cover predictions (percent, 0-100).

- `pa_post_hoc`: Presence/absence after post-hoc treatment (0 if any
  limitation).

- `cover_post_hoc`: Cover after post-hoc treatment (0 if any
  limitation).

If Secchi depth data is provided, two additional columns are returned:

- `vmax`: Maximum colonization depth calculated via Chambers & Kalff
  equation.

- `limitation_secchi`: Logical indicating light limitation (`TRUE` if
  depth exceeds vmax).

## Statistical Methods

The function applies two types of models: one for predicting
presence/absence of SAV and another for predicting SAV cover. Three
statistical methods are available:

- **Random Forest (`"rf"`)**: Non-parametric ensemble method, robust to
  non-linear relationships. Best for complex patterns.

- **Generalized Additive Models (`"gam"`)**: Semi-parametric method with
  smooth functions. Good balance between interpretability and
  flexibility.

- **Linear Mixed Models (`"lmm"`)**: Parametric approach accounting for
  hierarchical data structure. Useful for grouped data.

You can specify different methods for presence/absence and cover
predictions using the `method_pa` and `method_cover` parameters. For
further details about the models, see Croft-White et al. (2022).

## Column Specification

**Required predictors (depth and fetch)**: Both depth and fetch data are
mandatory. By default, the function looks for columns named `"depth"`
and `"fetch"`. If these columns are not found, the function will attempt
to auto-detect columns using case-insensitive matching for 'depth_m',
'depth', 'fetch_km', or 'fetch'. If columns cannot be found, an error is
thrown. To use custom column names, explicitly specify them via the
`depth` and `fetch` parameters.

**Optional predictors (substrate, secchi, limitation)**: These columns
are optional for post-hoc adjustments. By default, the function looks
for columns with these exact names. If a column is not found in the
data, that post-hoc adjustment is skipped. To use custom column names,
explicitly specify them via the respective parameters. To explicitly
disable a post-hoc adjustment even if the column exists, set the
parameter to `NULL`.

## Post-hoc Treatment

Post-hoc adjustments refine raw predictions by setting them to 0
wherever limitations are detected:

- **substrate**: Substrate limitation (e.g., unsuitable bottom
  substrate).

- **limitation**: User-supplied limitation data.

- **limitation_secchi**: Light limitation calculated from Secchi depth.

These columns are treated as binary indicators: any value \> 0 indicates
a limiting condition. When limitations are present, both `pa_post_hoc`
and `cover_post_hoc` are set to 0.

## Vmax Calculation

When Secchi depth data is provided, maximum colonization depth (Vmax) is
calculated using the Chambers and Kalff (1985) equation. The `vmax_par`
parameter controls the regression coefficients:

- **Model A** (default): `list(intercept = 1.40, slope = 1.33)` -
  Quebec + international lakes

- **Model B**: `list(intercept = 1.32, slope = 1.14)` - Quebec lakes
  only

Sites where depth exceeds Vmax are flagged as light-limited via the
`limitation_secchi` column.

## References

- Croft-White, M.V., Tang, R., Gardner Costa, J., Doka, S.E., and
  Midwood, J. D. 2022. Modelling submerged aquatic vegetation presence
  and percent cover to support the development of a freshwater fish
  habitat management tool. Can. Tech. Rep. Fish. Aquat. Sci. 3497: vi +
  30 p.

- Chambers, P.A., and Kalff, J. 1985. Depth Distribution and Biomass of
  Submerged aquatic macrophyte communities in relation to secchi depth.
  Can. J. Fish. Aquat. Sci. 42: 701–709

## Examples

``` r
# \donttest{
# Basic usage with auto-detection of column names
sav_model(data.frame(depth = c(5, 10), fetch = c(1, 2)))
#> ! No column available for post-hoc treatment.
#>   depth_m fetch_km pa_prob pa_pred cover_pred pa_post_hoc cover_post_hoc
#> 1       5        1   0.984       1   88.38661           1       88.38661
#> 2      10        2   0.000       0    0.00000           0        0.00000

# Using different methods for PA and cover
sav_model(
  data.frame(depth_m = c(5, 10), fetch_km = c(1, 2)),
  method_pa = "rf",
  method_cover = "gam"
)
#> ! No column available for post-hoc treatment.
#>   depth_m fetch_km pa_prob pa_pred cover_pred pa_post_hoc cover_post_hoc
#> 1       5        1   0.984       1        100           1            100
#> 2      10        2   0.000       0          0           0              0

# Adjusting PA threshold
sav_model(
  data.frame(depth = c(5, 10), fetch = c(1, 2)),
  pa_threshold = 0.7 # More conservative predictions
)
#> ! No column available for post-hoc treatment.
#>   depth_m fetch_km pa_prob pa_pred cover_pred pa_post_hoc cover_post_hoc
#> 1       5        1   0.984       1   88.38661           1       88.38661
#> 2      10        2   0.000       0    0.00000           0        0.00000

# Using post-hoc treatment with Secchi and substrate data
sav_model(
  dat = data.frame(
    depth = c(5, 10, 5),
    fetch = c(1, 2, 10),
    secchi = c(1, 10, 10),
    substrate = c(1, 1, 0)
  )
)
#> ℹ Using substrate and secchi for post-hoc treatment.
#>   depth_m fetch_km substrate secchi limitation_secchi     vmax pa_prob pa_pred
#> 1       5        1      TRUE      1              TRUE  1.96000   0.984       1
#> 2      10        2      TRUE     10             FALSE 19.91335   0.000       0
#> 3       5       10     FALSE     10             FALSE 19.91335   0.000       0
#>   cover_pred pa_post_hoc cover_post_hoc
#> 1   88.38661           0              0
#> 2    0.00000           0              0
#> 3    0.00000           0              0

# Using custom column names
sav_model(
  dat = data.frame(
    water_depth = c(5, 10),
    wave_fetch = c(1, 2)
  ),
  depth = "water_depth",
  fetch = "wave_fetch"
)
#> ! No column available for post-hoc treatment.
#>   depth_m fetch_km pa_prob pa_pred cover_pred pa_post_hoc cover_post_hoc
#> 1       5        1   0.984       1   88.38661           1       88.38661
#> 2      10        2   0.000       0    0.00000           0        0.00000

# Disabling post-hoc adjustments by setting to NULL
sav_model(
  dat = data.frame(
    depth = c(5, 10),
    fetch = c(1, 2),
    substrate = c(1, 0) # substrate column exists but will be ignored
  ),
  substrate = NULL # explicitly disable substrate adjustments
)
#> ! No column available for post-hoc treatment.
#>   depth_m fetch_km pa_prob pa_pred cover_pred pa_post_hoc cover_post_hoc
#> 1       5        1   0.984       1   88.38661           1       88.38661
#> 2      10        2   0.000       0    0.00000           0        0.00000

# Using Model B Vmax parameters (Quebec lakes only)
sav_model(
  dat = data.frame(depth = c(5, 10), fetch = c(1, 2), secchi = c(2, 3)),
  vmax_par = list(intercept = 1.32, slope = 1.14)
)
#> ℹ Using secchi for post-hoc treatment.
#>   depth_m fetch_km secchi limitation_secchi     vmax pa_prob pa_pred cover_pred
#> 1       5        1      2              TRUE 4.452892   0.984       1   88.38661
#> 2      10        2      3              TRUE 6.617334   0.000       0    0.00000
#>   pa_post_hoc cover_post_hoc
#> 1           0              0
#> 2           0              0
# }
```
