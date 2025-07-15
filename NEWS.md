# SAVM (devel)

* The element `mean_fetch` returned by `compute_fetch()` is now a `sf` object (see #9).
* `preview_grid()` allows to preview grid (see #8).
* Add more guidance on reading shapefiles in the vignette (see #6). 
* `invert_polygon()` has been added to invert polygon (see #6).
* `compute_fetch()` has a new argument `n_bearings` that provides the number of bearings, it replaces `n_quad_seg` (see #4 and #5). 
* `compute_fetch()` only compute the mean fetch for all bearings, columns with 
suffix `_all` where therefore removed (see #4).