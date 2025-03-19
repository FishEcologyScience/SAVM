#' Compute fetch and wind-weighted fetch
#'
#' @param points {`sf`}\cr{} An `sf` point object representing the locations for which fetch will be calculated.
#' @param polygon {`sf`}\cr{} An `sf` polygon object defining land boundaries used to compute fetch distances.
#' @param n_quad_seg {`integer`}\cr{} Number of segments per quadrant for fetch calculation. Ignored if `wind_weights`
#' is provided.
#' @param max_dist {`numeric`}\cr{} Maximum fetch distance in meters. Fetch beyond this distance is capped.
#' @param wind_weights {`data.frame`}\cr{} A data frame specifying directional 
#' weights for wind exposure. Must contain two columns: `direction` (numeric, 
#' in degrees) and `weight` (numeric). Currently this applies to all points.
#' @param crs {`object`}\cr{} Coordinate reference system (CRS) passed to 
#' [sf::st_crs()], used to transform `points` and `polygon`.
#'
#' @return
#' A list of two elements:
#'  * `mean_fetch`: data frame with 3 columns:
#'      * `id_point`: point identifier
#'      * `mean_fetch``: mean fetch
#'      * `mean_weighted_fetch``: mean fetch
#'  * `sf_fetch_lines`: a `sf` object containing all fetch lines.
#'
#' @references
#' * For an implementation leveraging st_buffer(), see <https://github.com/blasee/windfetch>.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' le_bound <- system.file("example", "lake_erie.gpkg", package = "SAVM") |>
#'      sf::st_read() 
#'  le_pt <- system.file("example", "le_points.geojson", package = "SAVM") |>
#'      sf::st_read(quiet = TRUE)
#' compute_fetch(le_pt, le_bound, 10, max_dist = 15000, crs = 3857)
#' }
#'
compute_fetch <- function(points, polygon, n_quad_seg, max_dist = 15000, wind_weights = NULL, crs = NULL) {
    valid_points(points)
    points$id_point <- seq_len(nrow(points))
    valid_polygon(polygon)

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
            direction = utils::head(seq(0, 360, by = 360 / (n_quad_seg * 4)), -1),
            weight = 1
        )
    } else {
        sav_msg_info("Using `wind_weights`, ignoring `n_quad_seg`")
        if (all(c("direction", "weight") %in% names(wind_weights))) {
            d_direction <- wind_weights[c("direction", "weight")]
        } else {
            rlang::abort("`wind_weights` must include two columns names `direction` and `weight`")
        }
    }

    fetch_lines <- create_fetch_lines(points, d_direction, max_dist)
    fetch_crop <- suppressWarnings(fetch_lines |> sf::st_intersection(polygon))
    geom_type <- sf::st_geometry_type(fetch_crop)
    # sf::st_intersection() generates multilinestring with extra lines if there
    # are intersections within the fetch lines
    sf_fetch_lines <- rbind(
        fetch_crop |>
            dplyr::filter(geom_type == "LINESTRING"),
        fetch_crop |>
            dplyr::filter(geom_type == "MULTILINESTRING") |>
            remove_detached_ends(points)
    ) |>
        dplyr::arrange(id_point, direction)

    sf_fetch_lines$fetch <- sf::st_length(sf_fetch_lines)

    list(
        mean_fetch = sf_fetch_lines |>
            sf::st_drop_geometry() |>
            dplyr::group_by(id_point) |>
            dplyr::summarise(
                mean_fetch = mean(fetch),
                mean_weighted_fetch = mean(fetch * weight)
            ),
        sf_fetch_lines = sf_fetch_lines
    )
}


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