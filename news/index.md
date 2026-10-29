# Changelog

## SAVM 0.1.1

- Notify users when predictor values are all NAs.
- Drop empty columns from user input data.
- Add SAVM version in the top left of the navbar.

## SAVM 0.1.0

- Fix problematic conversion to one-column matrix.
- Lint code.
- CI update + manifest update.

## SAVM 0.0.2

- New shiny app that exposes the package capacities.
- [`compute_fetch()`](../reference/compute_fetch.md) gains argument
  `land_polygon` to work with land polygons.
- Bearings are oriented properly and the element `transect_lines` of the
  object returned by [`compute_fetch()`](../reference/compute_fetch.md)
  gains a column `cardinal_direction` (see
  [\#15](https://github.com/FishEcologyScience/SAVM/issues/15)).
- Argument `remove_outsiders` has been removed from
  [`compute_fetch()`](../reference/compute_fetch.md), instead a column
  `outsider` which identifies outsiders has been added.  
- `identify_outsiders()` has been renamed
  [`visualize_outsiders()`](../reference/compute_fetch.md).
- [`sav_load_model()`](../reference/sav_load_model.md) is now exposed
  and the vignette “SAV Prediction Models” details the model available.
- Only models using both fetch and depth are supported, therefore
  argument `type` has been dropped from
  [`sav_model()`](../reference/sav_model.md).
- New `glmm` and `gam` models available.

## SAVM 0.0.1

- [`compute_fetch()`](../reference/compute_fetch.md) has a new argument
  `remove_outsider` to remove points falling outside the polygon. Also,
  `identify_outsiders()` helps visualize outsiders (see
  [\#13](https://github.com/FishEcologyScience/SAVM/issues/13)).
- Model and plot functions have been adjusted to handle `sf` objects
  (see [\#11](https://github.com/FishEcologyScience/SAVM/issues/11)).
- The element `mean_fetch` returned by
  [`compute_fetch()`](../reference/compute_fetch.md) is now a `sf`
  object (see
  [\#9](https://github.com/FishEcologyScience/SAVM/issues/9)).
- [`preview_grid()`](../reference/preview_grid.md) allows to preview
  grid (see [\#8](https://github.com/FishEcologyScience/SAVM/issues/8)).
- Add more guidance on reading shapefiles in the vignette (see
  [\#6](https://github.com/FishEcologyScience/SAVM/issues/6)).
- [`invert_polygon()`](../reference/invert_polygon.md) has been added to
  invert polygon (see
  [\#6](https://github.com/FishEcologyScience/SAVM/issues/6)).
- [`compute_fetch()`](../reference/compute_fetch.md) has a new argument
  `n_bearings` that provides the number of bearings, it replaces
  `n_quad_seg` (see
  [\#4](https://github.com/FishEcologyScience/SAVM/issues/4) and
  [\#5](https://github.com/FishEcologyScience/SAVM/issues/5)).
- [`compute_fetch()`](../reference/compute_fetch.md) only compute the
  mean fetch for all bearings, columns with suffix `_all` where
  therefore removed (see
  [\#4](https://github.com/FishEcologyScience/SAVM/issues/4)).
