# helpers work

    Code
      res <- identify_outsiders(le_pt_out, le_bound)
    Message
      ! All points are outside `polygon`.
      ! Use `visualize_outsiders()` to vizualize outsiders.

# compute_fetch() generates NA for outsiders

    Code
      res1 <- compute_fetch(le_pt_in_out, le_bound_merc)
    Message
      i `points` and `polygon` have different CRS, transforming
      `points` to match `polygon` CRS.
      ! Some points are outside `polygon`.
      ! Use `visualize_outsiders()` to vizualize outsiders.
      i Creating fetch lines
      i Cropping fetch lines

---

    Code
      res2 <- compute_fetch(le_pt_out, le_bound_merc)
    Message
      i `points` and `polygon` have different CRS, transforming
      `points` to match `polygon` CRS.
      ! All points are outside `polygon`.
      ! Use `visualize_outsiders()` to vizualize outsiders.
      i No fetch lines will be created

