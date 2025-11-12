# SAVM (devel)

* Shiny App that expose the package capacities.
* Bearings are oriented properly and the element `transect_lines` of the object returned by `compute_fetch()` gains a column `cardinal_direction` (see #15).
* `sav_load_model()` is now exposed and the vignette "SAV Prediction Models" details the model available.
* New `glmm` and `gam` models.

# SAVM 0.0.1

* `compute_fetch()` has a new argument `remove_outsider` to remove points falling outside the polygon. Also, `identify_outsiders()` helps visualize outsiders (see #13).
* Model and plot functions have been adjusted to handle `sf` objects (see #11).
* The element `mean_fetch` returned by `compute_fetch()` is now a `sf` object (see #9).
* `preview_grid()` allows to preview grid (see #8).
* Add more guidance on reading shapefiles in the vignette (see #6). 
* `invert_polygon()` has been added to invert polygon (see #6).
* `compute_fetch()` has a new argument `n_bearings` that provides the number of bearings, it replaces `n_quad_seg` (see #4 and #5). 
* `compute_fetch()` only compute the mean fetch for all bearings, columns with 
suffix `_all` where therefore removed (see #4).