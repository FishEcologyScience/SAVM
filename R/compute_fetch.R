#' Compute wind fetch and wind weighted fetch
#'
#' @param points {`sf`}\cr{}
#' Points representing the locations for which fetch will be calculated.
#' @param polygon {`sf`}\cr{}
#' Polygon defining land boundaries used to compute fetch distances.
#' @param max_dist {`numeric`}\cr{}
#' Maximum fetch distance in kilometers. Fetch beyond this distance is capped.
#' @param n_bearings {`integer`}\cr{}
#' Total number of bearings for fetch calculation (minimal number required is
#' 4, default is 16). Ignored if `wind_weights` is provided.
#' @param wind_weights {`data.frame`}\cr{}
#' A data frame specifying directional weights for wind exposure.
#' Must contain two columns: `direction` (numeric, in degrees) and `weight`
#' (numeric). Note that the weighting applies to all points.
#' @param crs {`object`}\cr{}
#' Coordinate reference system (CRS) passed to [sf::st_crs()], used to
#' transform `points` and `polygon`.
#' @param land_polygon {`logical`}\cr{}
#' Indicates whether the polygon represents land (`TRUE`) or water (`FALSE)`.
#' When `TRUE`, points inside the polygon are considered outsiders (inland
#' points). When `FALSE`, points outside the polygon are considered outsiders.
#' Default is `FALSE`.
#'
#'
#' @details
#' Wind fetch is the unobstructed distance over which wind travels
#' across a body of water before reaching a specific point. It plays a crucial
#' role in wave generation, as longer fetch distances allow wind to transfer
#' more energy to the water surface, leading to larger waves.
#'
#' For all points in `points`, `n_bearings` radial transects are generated
#' by default. If `wind_weights` is specified, the column `direction`, which
#' contains angles in degrees, is used instead to generate the transects. The
#' transects are then clipped with the polygon using [`sf::st_intersection()`],
#' and any lines that are not connected to the points are removed. The length of
#' all clipped transects is computed using [`sf::st_length()`] and ranked using
#' `rank()`. The resulting spatial object is stored as the `transect_lines`
#' element in the returned list and is used to generate the second element:
#' `mean_fetch` that includes wind fetch averages.
#'
#' Ensure that `max_dist` is specified in kilometers. An error will be thrown
#' if the spatial projection of points and polygon is not in a meter-based
#' coordinate system.
#'
#' Points identified as outsiders (either outside the polygon when
#' `land_polygon = FALSE`, or inside the polygon when `land_polygon = TRUE`)
#' will have their `fetch_km` and `weighted_fetch_km` values set to `NA` in the
#' `mean_fetch` output. No transect lines are created for outsider points. Use
#' `visualize_outsiders()` to visualize which points are considered outsiders
#' before running the fetch computation.
#'
#'
#' @return
#' A list of two elements:
#'  * `mean_fetch`: a `sf` object with 4 columns:
#'      * `id_point`: point identifier
#'      * `outsider`: logical indicating if the point is an outsider
#'      * `fetch_km`: mean wind fetch based on all bearings (NA for outsiders)
#'      * `weighted_fetch_km`: mean weighted wind fetch based on all bearings (NA for outsiders)
#'  * `transect_lines`: a `sf` object containing all radial transects with the
#'    same columns as `points` and the following additional columns (NULL if
#'    all points are outsiders):
#'      * `id_point`: point identifier
#'      * `direction`: direction (in degrees)
#'      * `cardinal_direction`: cardinal direction (e.g., N, NE, E, etc.)
#'      * `weight`: wind weight
#'      * `transect_length`: transect length in meters computed using [`sf::st_length()`]
#'      * `rank`: transect ranks (the lower the rank, the longer the transect)
#'
#' @references
#' * For an implementation leveraging  [`sf::st_buffer()`], see
#'   <https://github.com/blasee/windfetch>.
#' * Croft-White, M.V., Tang, R., Gardner Costa, J., Doka, S.E., and Midwood, J.
#' D. 2022. Modelling submerged aquatic vegetation presence and percent cover to
#' support the development of a freshwater fish habitat management tool. Can.
#' Tech. Rep. Fish. Aquat. Sci. 3497: vi + 30 p.
#'
#' @export
#'
#' @examples
#' \donttest{
#' le_bound <- system.file("example", "lake_erie.gpkg", package = "SAVM") |>
#'   sf::st_read()
#' le_pt <- system.file("example", "le_points.geojson", package = "SAVM") |>
#'   sf::st_read(quiet = TRUE)
#' res <- compute_fetch(le_pt, le_bound, crs = 32617)
#' # use wind-weight
#' res2 <- compute_fetch(
#'   le_pt, le_bound,
#'   max_dist = 20,
#'   wind_weights = data.frame(
#'     direction = seq(0, 360, by = 360 / 16)[-1],
#'     weight = rep(c(0, 1), each = 8)
#'   ),
#'   crs = 32617
#' )
#'
#' # results
#' res$mean_fetch
#' res2$mean_fetch
#'
#' # visualizing fetch lines
#' plot(le_bound |> sf::st_transform(crs = 32617) |> sf::st_geometry())
#' plot(res$transect_lines |> sf::st_geometry(), add = TRUE, col = 2, lwd = 0.5)
#' }
compute_fetch <- function(
  points, polygon, max_dist = 15, n_bearings = 16, wind_weights = NULL,
  crs = NULL, land_polygon = FALSE
) {
  # basic validation
  valid_points(points)
  points$id_point <- seq_len(nrow(points))
  valid_polygon(polygon)
  sav_stop_if_not(max_dist > 0, "`max_dist` must be strictly positive.")
  max_dist <- 1e3 * max_dist
  sav_stop_if_not(n_bearings >= 4, "`n_bearings` should be equal or greater than 4.")
  if (n_bearings > 64) {
    sav_msg_warning(
      "Large number of bearings detected, computation may take a long time."
    )
  }

  if (!is.null(crs)) {
    if (!is_proj_unit_meter(crs)) {
      rlang::abort("Projection units must be meters.")
    }
    points <- sf::st_transform(points, crs = sf::st_crs(crs))
    polygon <- sf::st_transform(polygon, crs = sf::st_crs(crs))
  } else {
    if (is_proj_unit_meter(polygon)) {
      if (sf::st_crs(points) != sf::st_crs(polygon)) {
        sav_msg_info(
          "`points` and `polygon` have different CRS, transforming
                     `points` to match `polygon` CRS."
        )
        points <- sf::st_transform(points, crs = sf::st_crs(polygon))
      }
      # else both are in meter and the same projection so nothing to do
    } else {
      if (is_proj_unit_meter(points)) {
        sav_msg_info(
          "`points` and `polygon` have different CRS, transforming
                     `polygon` to match `points` CRS."
        )
        polygon <- sf::st_transform(polygon, crs = sf::st_crs(points))
      } else {
        rlang::abort("Projection units must be meters.")
      }
    }
  }

  points <- points |>
    dplyr::select(id_point)
  points$outsider <- identify_outsiders(points, polygon, land_polygon)

  if (is.null(wind_weights)) {
    d_direction <- data.frame(
      direction = utils::head(seq(0, 360, by = 360 / n_bearings), -1),
      weight = 1
    )
  } else {
    sav_msg_info("Using `wind_weights`, ignoring `n_bearings`")
    if (all(c("direction", "weight") %in% names(wind_weights))) {
      d_direction <- wind_weights[c("direction", "weight")]
      valid_direction(d_direction$direction)
    } else {
      rlang::abort("`wind_weights` must include two columns names `direction` and `weight`")
    }
  }
  d_direction <- d_direction |>
    dplyr::mutate(
      cardinal_direction = angle_to_cardinal_direction(direction)
    )

  mean_fetch_na <- NULL
  if (any(points$outsider)) {
    mean_fetch_na <- points |>
      dplyr::filter(outsider) |>
      dplyr::mutate(
        fetch_km = NA_real_,
        weighted_fetch_km = NA_real_
      )
    if (all(points$outsider)) {
      sav_msg_info("No fetch lines will be created")
      return(
        list(
          mean_fetch = mean_fetch_na,
          transect_lines = NULL
        )
      )
    }
  }

  sav_msg_info("Creating fetch lines")
  fetch_lines <- create_fetch_lines(
    points |> dplyr::filter(!outsider), d_direction, max_dist
  )

  sav_msg_info("Cropping fetch lines")
  if (land_polygon) {
    fetch_crop <- suppressWarnings(fetch_lines |> sf::st_difference(polygon))
  } else {
    fetch_crop <- suppressWarnings(fetch_lines |> sf::st_intersection(polygon))
  }
  geom_type <- sf::st_geometry_type(fetch_crop)
  # sf::st_intersection() generates MULTILINESTRING with extra lines if there
  # are intersections within the fetch lines
  transect_lines <- rbind(
    fetch_crop |>
      dplyr::filter(geom_type == "LINESTRING"),
    fetch_crop |>
      dplyr::filter(geom_type == "MULTILINESTRING") |>
      remove_detached_ends(points)
  ) |>
    dplyr::arrange(id_point, direction)
  transect_lines <- transect_lines |>
    dplyr::mutate(transect_length = sf::st_length(transect_lines)) |>
    dplyr::group_by(id_point) |>
    # using -transect so that the longest are ranked 1
    dplyr::mutate(rank = rank(-transect_length, ties.method = "min"))

  list(
    mean_fetch = points |>
      dplyr::left_join(
        transect_lines |>
          sf::st_drop_geometry() |>
          dplyr::group_by(id_point) |>
          # dplyr::mutate(rank = rank(transect_length)) |>
          dplyr::summarise(
            fetch_km = mean(transect_length),
            weighted_fetch_km = mean(transect_length * weight)
          ) |>
          dplyr::mutate(
            dplyr::across(
              !c(id_point),
              ~ as.numeric(units::set_units(.x, "km"))
            )
          ),
        by = "id_point"
      ) |>
      dplyr::select(
        c(id_point, outsider, fetch_km, weighted_fetch_km)
      ) |>
      dplyr::bind_rows() |>
      dplyr::arrange(id_point),
    transect_lines = transect_lines
  )
}


#' @describeIn compute_fetch Visualize outsider points in a plot where
#' outsiders are highlighted in red.
#' @export
visualize_outsiders <- function(points, polygon, land_polygon) {
  v_outsiders <- suppressMessages(
    identify_outsiders(points, polygon, land_polygon)
  )
  plot(polygon |> sf::st_geometry(), border = 1)
  plot(
    points |> sf::st_geometry(),
    col = 1 + v_outsiders,
    pch = 19,
    cex = 2,
    add = TRUE
  )
}


# =============================================
# HELPERS

is_proj_unit_meter <- function(x) {
  proj <- sf::st_crs(x)
  !(is.null(proj$units) || proj$units != "m")
}

valid_points <- function(x) {
  if (all(x |> sf::st_geometry_type() == "POINT")) {
    return(TRUE)
  } else {
    paste0(
      "Geometries in `",
      rlang::caller_arg(x),
      "` must be of type `POINT`."
    ) |>
      rlang::abort()
  }
}

valid_polygon <- function(x) {
  if (all(x |> sf::st_geometry_type() %in% c("POLYGON", "MULTIPOLYGON"))) {
    return(TRUE)
  } else {
    paste0(
      "Geometries in `", rlang::caller_arg(x),
      "` must be of type `POLYGON` or `MULTIPOLYGON`."
    ) |>
      rlang::abort()
  }
}

identify_outsiders <- function(points, polygon, land_polygon = FALSE) {
  v_valid_points <- suppressMessages(
    sf::st_contains(polygon, points, sparse = FALSE) |>
      apply(2, any)
  )
  if (land_polygon) v_valid_points <- !v_valid_points

  if (!all(v_valid_points)) {
    all_out <- ifelse(all(!v_valid_points), "All", "Some")
    pol_lan <- ifelse(land_polygon, "inside", "outside")
    sav_msg_warning("{all_out} points are {pol_lan} `polygon`.")
    sav_msg_warning("Use `visualize_outsiders()` to vizualize outsiders.")
  }
  !v_valid_points
}



valid_direction <- function(direction) {
  if (!all(direction >= 0 & direction <= 360)) {
    rlang::abort("All directions must be within the range [0, 360].")
  } else {
    interv <- findInterval(
      direction,
      seq(0, 360, by = 90),
      all.inside = TRUE
    )
    if (!all(1:4 %in% interv)) {
      sav_warn("Not all 4 quadrants are covered.")
    }
  }
  TRUE
}

# NB: code in windfetch use st_buffer()
create_fetch_lines <- function(points, d_direction, max_dist) {
  coords <- sf::st_coordinates(points)
  directions <- d_direction$direction
  dir_angles <- (-directions + 90) / 360 * 2 * pi
  x_dir <- max_dist * cos(dir_angles)
  y_dir <- max_dist * sin(dir_angles)
  tmp <- list()
  for (i in seq_len(nrow(points))) {
    # computes fetch line coordinates
    tmp[[i]] <- data.frame(
      lon = c(rep(coords[i, 1], length(directions)), coords[i, 1] + x_dir),
      lat = c(rep(coords[i, 2], length(directions)), coords[i, 2] + y_dir),
      id_point = points$id_point[i],
      direction = rep(directions, 2),
      cardinal_direction = rep(d_direction$cardinal_direction, 2),
      weight = rep(d_direction$weight, 2)
    )
  }
  # create lines by casting grouped points
  sf::st_as_sf(
    tmp |> do.call(what = rbind),
    coords = c("lon", "lat"),
    crs = sf::st_crs(points)
  ) |>
    dplyr::group_by(id_point, direction, cardinal_direction, weight) |>
    dplyr::summarize() |>
    sf::st_cast("LINESTRING")
}

remove_detached_ends <- function(x, points) {
  suppressWarnings(tmp <- x |> sf::st_cast("LINESTRING"))
  out <- list()
  for (i in unique(tmp$id_point)) {
    out[[i]] <- tmp |> dplyr::filter(id_point == i)
    out[[i]] <- out[[i]][sf::st_intersects(
      out[[i]],
      points |> dplyr::filter(id_point == i),
      sparse = FALSE
    )[, 1L], ]
  }
  do.call(rbind, out)
}


angle_to_cardinal_direction <- function(x) {
  interv <- findInterval(x, seq(0, 360, by = 11.25), all.inside = TRUE)
  card <- rep(c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW"
  ), each = 2)
  card <- c(card[-1], card[1])
  card[interv]
}