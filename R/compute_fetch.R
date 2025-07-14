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
#' 4, default is 167). Ignored if `wind_weights` is provided.
#' @param wind_weights {`data.frame`}\cr{}
#' A data frame specifying directional weights for wind exposure.
#' Must contain two columns: `direction` (numeric, in degrees) and `weight`
#' (numeric). Note that the weighting applies to all points.
#' @param crs {`object`}\cr{}
#' Coordinate reference system (CRS) passed to [sf::st_crs()], used to
#' transform `points` and `polygon`.
#'
#' @details Wind fetch is the unobstructed distance over which wind travels
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
#' element in the returned list and it used to generate the second element:
#' `mean_fetch` that included wind fetch averages.
#'
#' Ensure that max_dist is specified in meters. An error will be thrown if the
#' spatial projection of points and polygon is not in a meter-based coordinate
#' system.
#'
#' @return A list of two elements:
#'  * `mean_fetch`: data frame with 5 columns:
#'      * `id_point`: point identifier
#'      * `fetch`, `fetch_all`: mean wind fetch based on all bearings.
#'      * `weighted_fetch`, `weighted_fetch_all`: mean weighted wind fetch based on all bearings.
#'  * `transect_lines`: a `sf` object containing all radial transect with the
#'    same columns as `points` and the following additional columns:
#'      * `id_point`: point identifier
#'      * `direction`: direction (in degree)
#'      * `weight`: wind weight
#'      * `transect_length`: transect length in meter computed using [`sf::st_length()`].
#'      * `rank`: transect ranks (the lower the rank the higher the length).
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
#'
#' le_bound <- system.file("example", "lake_erie.gpkg", package = "SAVM") |>
#'     sf::st_read()
#' le_pt <- system.file("example", "le_points.geojson", package = "SAVM") |>
#'     sf::st_read(quiet = TRUE)
#' res <- compute_fetch(le_pt, le_bound, crs = 32617)
#' # use wind-weight
#' res2 <- compute_fetch(
#'     le_pt, le_bound,
#'     max_dist = 20,
#'     wind_weights = data.frame(
#'         direction = seq(0, 360, by = 360 / 16)[-1],
#'         weight = rep(c(0, 1), each = 8)
#'     ),
#'     crs = 32617
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
    points, polygon, max_dist = 15, n_bearings = 16, wind_weights = NULL, crs = NULL) {
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

    valid_polygon_contains_points(points, polygon)

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

    sav_msg_info("Creating fetch lines")
    fetch_lines <- create_fetch_lines(points, d_direction, max_dist)

    sav_msg_info("Cropping fetch lines")
    fetch_crop <- suppressWarnings(fetch_lines |> sf::st_intersection(polygon))
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
        mean_fetch = transect_lines |>
            sf::st_drop_geometry() |>
            dplyr::group_by(id_point) |>
            # dplyr::mutate(rank = rank(transect_length)) |>
            dplyr::summarise(
                fetch_km = mean(transect_length),
                weighted_fetch_km = mean(transect_length * weight),
                fetch_km_all = mean(transect_length),
                weighted_fetch_km_all = mean(transect_length * weight)
            ) |>
            dplyr::mutate(
                dplyr::across(
                    !id_point,
                    ~ as.numeric(units::set_units(.x, "km"))
                )
            ),
        transect_lines = transect_lines
    )
}

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

valid_polygon_contains_points <- function(points, polygon) {
    chk <- suppressMessages({
        sf::st_contains(polygon, points, sparse = FALSE) |>
            apply(2, any) |>
            all()
    })
    if (chk) {
        TRUE
    } else {
        rlang::abort("`polygon` must include `points`.")
    }
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
            sav_warn("Not all quadrants are covered.")
        }
    }
    TRUE
}

# NB: code in windfetch use st_buffer()
create_fetch_lines <- function(points, d_direction, max_dist) {
    coords <- sf::st_coordinates(points)
    directions <- d_direction$direction
    tmp <- list()
    for (i in seq_len(nrow(points))) {
        # computes fetch line coordinates
        tmp[[i]] <- data.frame(
            lon = c(
                rep(coords[i, 1], length(directions)),
                coords[i, 1] + max_dist * cos(directions / 360 * 2 * pi)
            ),
            lat = c(
                rep(coords[i, 2], length(directions)),
                coords[i, 2] + max_dist * sin(directions / 360 * 2 * pi)
            ),
            id_point = points$id_point[i],
            direction = rep(directions, 2),
            weight = rep(d_direction$weight, 2)
        )
    }
    # create lines by casting grouped points
    sf::st_as_sf(
        tmp |> do.call(what = rbind),
        coords = c("lon", "lat"),
        crs = sf::st_crs(points)
    ) |>
        dplyr::group_by(id_point, direction, weight) |>
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