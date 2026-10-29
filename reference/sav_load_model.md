# Load SAV Model

Load a pre-trained SAV model for predicting cover or presence/absence.

## Usage

``` r
sav_load_model(type = c("cover", "pa"), method = "rf")
```

## Arguments

- type:

  `character`  
  Model type, either `"cover"` or `"pa"` (presence/absence).

- method:

  `character`  
  Modeling method: `"rf"` (random forest), `"lmm"`, or `"gam"`.

## Value

A model object (e.g., randomForest, glmm, or gam object) that can be
used for predictions.

## Details

This function loads pre-trained models from the package's internal data
directory. Models are available for both presence/absence (`"pa"`) and
cover prediction (`"cover"`), using three different statistical methods:
Random Forest (`"rf"`), Linear Mixed Models (`"lmm"`), and Generalized
Additive Models (`"gam"`). All models use both depth and fetch as
predictors.

## Examples

``` r
# \donttest{
# Load a random forest model for presence/absence
model <- sav_load_model("pa", "rf")

# Load a cover model using Linear Mixed Model
model <- sav_load_model("cover", "lmm")
#> Loading required namespace: lmerTest

# Load a GAM model for cover
model <- sav_load_model("cover", "gam")
# }
```
