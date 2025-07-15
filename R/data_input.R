#' Read SAV data from different formats
#'
#' @param file_path {`character`}\cr{} Path to the input file.
#' @param spacing {`numeric`}\cr{}
#' Spacing for grid generation. Used if an area of interest (AOI) is provided,
#' ignored otherwise.
#' @param layer {`character`}\cr{} Layer name for multi-layer spatial files (default: NULL).
#' @param crs {`numeric`}\cr{}
#' Coordinate Reference System (CRS) of the output data. Default is 32617.
#' @param crs_input {`numeric`}\cr{}
#' Coordinate Reference System (CRS) of the input data, used for CSV import.
#' Default is 4326.
#' @param export {`character`}\cr{}
#' Optional. Folder path to export outputs as .gpkg.
#'
#' @return A list with `points` (sf object) and `polygon` (sf object).
#'
#' @rdname data_input
#'
#' @export
#'
#' @examples
#' # Example usage with CSV file
#' library(sf)
#' temp_csv <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'     longitude = c(-82.5, -83.0, -84.8),
#'     latitude = c(42.5, 42.8, 42.6),
#'     depth_m = c(5, 10, 7),
#'     fetch_km = c(2.5, 3.0, 2.8),
#'     secchi = c(1.2, 2.3, 1.8),
#'     substrate = c(TRUE, FALSE, TRUE)
#' ), temp_csv, row.names = FALSE)
#'
#' tmp <- read_sav(temp_csv, crs = 32617, crs_input = 4326)
#' head(tmp$points)
#' plot(st_geometry(tmp$polygon))
#' plot(st_geometry(tmp$points), add = TRUE)
#'
#' # Example usage with spatial polygon file (AOI)
#' temp_poly <- st_sf(
#'     geometry = st_sfc(st_polygon(list(
#'         rbind(
#'             c(-82.5, 42.5), c(-82.5, 42.8), c(-82.0, 42.8), c(-82.0, 42.5), c(-82.5, 42.5)
#'         )
#'     )), crs = 4326)
#' )
#' temp_file <- tempfile(fileext = ".gpkg")
#' st_write(temp_poly, temp_file, quiet = TRUE)
#'
#' tmp <- read_sav(temp_file, spacing = 500, crs = 32617)
#' head(tmp$points)
#' plot(st_geometry(tmp$polygon))
#' plot(st_geometry(tmp$points), add = TRUE)
#'
read_sav <- function(file_path, spacing = 500, layer = NULL, crs = 32617, crs_input = 4326, export = NULL) {
    sav_msg_info("Determining file type and processing: {file_path}")
    file_ext <- tools::file_ext(file_path)

    if (file_ext == "csv") {
        sav_msg_info("csv detected")
        points_sf <- read_sav_csv(file_path, crs = crs)
        polygon_sf <- sf::st_sf(geometry = sf::st_union(points_sf) |> sf::st_convex_hull())
    } else if (file_ext %in% c("shp", "geojson", "gpkg", "gbd")) {
        if (file_ext == "gbd") {
            sav_msg_info("gdb detected")
            sf_obj <- sf::st_read(file_path, layer = layer, quiet = TRUE)
        } else {
            sav_msg_info("spatial file detected")
            sf_obj <- sf::st_read(file_path, quiet = TRUE)
        }

        geom_type <- sf::st_geometry_type(sf_obj)

        if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
            points_sf <- read_sav_pts(file_path, crs = crs)
            polygon_sf <- sf::st_sf(geometry = sf::st_union(points_sf) |> sf::st_convex_hull())
        } else if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
            aoi_result <- read_sav_aoi(file_path, spacing, crs)
            points_sf <- aoi_result$points
            polygon_sf <- aoi_result$polygon
        } else {
            rlang::abort("Unsupported spatial file type. Expected point or polygon geometries.")
        }
    } else {
        rlang::abort("Unsupported file extension. Expected csv, shp, geojson, gpkg, or gbd.")
    }

    if (!is.null(export)) {
        sf::st_write(points_sf, file.path(export, "sav_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)
        sf::st_write(polygon_sf, file.path(export, "sav_polygon.gpkg"), delete_dsn = TRUE, quiet = TRUE)
        sav_msg_success("Exported outputs to {export}.")
    }

    return(
        structure(
            list(points = points_sf, polygon = polygon_sf),
            class = "sav_data"
        )
    )
}


#' Read a CSV file and convert to sf object
#'
#' @param file_path {`character`}\cr{} Path to the CSV file.
#' @param crs {`object`}\cr{} Coordinate reference system (CRS)
#' of the output data (see [sf::st_crs()]). Default is 32617.
#' @param crs_input {`numeric`}\cr{} Coordinate Reference System (CRS) of the input data, used for CSV import. Default is 4326.
#' @param ... Further arguments passed on to [utils::read.csv()].
#'
#' @return An sf object with required and optional columns.
#'
#' @rdname data_input
#'
#' @export
#'
#' @examples
#' # Example CSV file creation
#' temp_csv <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'     longitude = c(-82.5, -83.0, -83.2),
#'     latitude = c(42.5, 42.8, 42.6),
#'     depth_m = c(5, 10, 7),
#'     fetch_km = c(2.5, 3.0, 2.8),
#'     secchi = c(1.2, 2.3, 1.8),
#'     substrate = c(TRUE, FALSE, TRUE),
#'     limitation = c(FALSE, FALSE, TRUE)
#' ), temp_csv, row.names = FALSE)
#'
#' # Read the CSV and convert to sf
#' read_sav_csv(temp_csv, crs = 32614, crs_input = 4326)
read_sav_csv <- function(file_path, crs = 32617, crs_input = 4326, ...) {
    # Read CSV
    df <- utils::read.csv(file_path, ...)

    # Validate required columns
    required_cols <- c("longitude", "latitude")
    optional_cols <- c("depth_m", "fetch_km", "secchi", "substrate", "limitation")
    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
        rlang::abort(
            "Missing required columns: {paste(missing_cols, collapse = ', ')}.
            Only the following formatting is allowed:
            Required - {paste(required_cols, collapse=', ')},
            Optional - {paste(optional_cols, collapse=', ')}"
        )
    }

    # Select relevant columns
    retained_cols <- intersect(names(df), c(required_cols, optional_cols))
    removed_cols <- setdiff(names(df), retained_cols)
    df <- df |> dplyr::select(dplyr::any_of(c(required_cols, optional_cols)))

    sav_msg_info(
        "Retained columns: {paste(retained_cols, collapse=', ')}.
        Removed columns: {ifelse(length(removed_cols) == 0,
        'None',paste(removed_cols, collapse=', '))}"
    )

    # Convert to sf object with user-specified CRS
    sf_obj <- sf::st_as_sf(
        df,
        coords = c("longitude", "latitude"),
        crs = crs_input,
        remove = FALSE
    )

    # Check CRS and transform if necessary
    if (sf::st_crs(sf_obj)$epsg != sf::st_crs(crs)) {
        sav_msg_info("Transforming spatial data.")
        sf_obj <- sf::st_transform(sf_obj, crs = sf::st_crs(crs))
        coords <- sf::st_coordinates(sf_obj)
        sf_obj <- sf_obj |> dplyr::mutate(longitude = coords[, 1], latitude = coords[, 2])
    }

    return(sf_obj)
}



#' Read a spatial points file and convert to sf object
#'
#' @param file_path {`character`}\cr{} Path to the spatial file.
#' @param crs {`object`}\cr{} Coordinate reference system (CRS)
#' of the output data (see [sf::st_crs()]). Default is 32617.
#'
#' @return {`sf`}\cr{} An sf object with required and optional columns.
#'
#' @rdname data_input
#'
#' @export
#'
#' @examples
#' # Example spatial points file creation (requires sf package)
#' library(sf)
#' temp_sf <- st_sf(
#'     longitude = c(-82.5, -83.0),
#'     latitude = c(42.5, 42.8),
#'     depth_m = c(5, 10),
#'     geometry = st_sfc(
#'         st_point(c(-82.5, 42.5)),
#'         st_point(c(-83.0, 42.8))
#'     ),
#'     crs = 4326
#' )
#'
#' temp_file <- tempfile(fileext = ".gpkg")
#' st_write(temp_sf, temp_file, quiet = TRUE)
#'
#' # Read the spatial file and convert to sf
#' read_sav_pts(temp_file)
#'
read_sav_pts <- function(file_path, crs = 32617) {
    # Read spatial file
    sf_obj <- sf::st_read(file_path, quiet = TRUE)

    # Ensure it's a point geometry
    if (!all(sf::st_geometry_type(sf_obj) %in% c("POINT", "MULTIPOINT"))) {
        rlang::abort("The provided spatial file does not contain point geometries.")
    }

    # Check CRS and transform if necessary
    if (sf::st_crs(sf_obj)$epsg != sf::st_crs(crs)) {
        sav_msg_info("Transforming spatial data.")
        sf_obj <- sf::st_transform(sf_obj, crs = sf::st_crs(crs))
    }

    # Extract coordinates regardless of original CRS
    coords <- sf::st_coordinates(sf_obj)
    sf_obj <- sf_obj |> dplyr::mutate(longitude = coords[, 1], latitude = coords[, 2])

    # Select relevant columns
    required_cols <- c("longitude", "latitude")
    optional_cols <- c("depth_m", "fetch_km", "secchi", "substrate", "limitation")
    retained_cols <- intersect(names(sf_obj), c(required_cols, optional_cols))
    removed_cols <- setdiff(names(sf_obj), c(retained_cols, "geom"))
    sf_obj <- sf_obj |>
        dplyr::select(dplyr::any_of(c(required_cols, optional_cols)))

    sav_msg_info(
        "Retained columns: {paste(retained_cols, collapse=', ')}.
        Removed columns: {ifelse(length(removed_cols) == 0,
        'None',paste(removed_cols, collapse=', '))}"
    )

    sav_msg_success("Spatial points file successfully read and processed.")
    return(sf_obj)
}



#' Read a spatial polygon file and generate a grid of points
#'
#' @param file_path {`character`}\cr{} Path to the spatial polygon file.
#' @param spacing {`numeric`}\cr{} Distance between points in meters.
#' @param crs {`object`}\cr{} Coordinate reference system (CRS)
#' of the output data (see [sf::st_crs()]). Default is 32617.
#'
#' @return A list with the original polygon and a grid of points.
#'
#' @rdname data_input
#'
#' @export
#'
#' @examples
#' # Example spatial polygon file creation (requires sf package)
#' library(sf)
#' temp_poly <- st_sf(
#'     geometry = st_sfc(st_polygon(list(
#'         rbind(
#'             c(-82.5, 42.5), c(-82.5, 42.8), c(-82.0, 42.8), c(-82.0, 42.5), c(-82.5, 42.5)
#'         )
#'     )), crs = 4326)
#' )
#' temp_file <- tempfile(fileext = ".gpkg")
#' st_write(temp_poly, temp_file, quiet = TRUE)
#'
#' # Read the spatial polygon file and generate a grid
#' read_sav_aoi(temp_file, spacing = 500)
read_sav_aoi <- function(file_path, spacing = 500, crs = 32617) {
    # Read spatial file
    polygon_sf <- sf::st_read(file_path, quiet = TRUE)

    # Ensure it's a polygon
    if (!all(sf::st_geometry_type(polygon_sf) %in% c("POLYGON", "MULTIPOLYGON"))) {
        rlang::abort("The provided spatial file does not contain polygon geometries.")
    }

    # Check CRS and transform if necessary
    if (sf::st_crs(polygon_sf)$epsg != sf::st_crs(crs)) {
        sav_msg_info("Transforming spatial data.")
        polygon_sf <- sf::st_transform(polygon_sf, crs = sf::st_crs(crs))
    }

    # Generate grid points
    grid <- sf::st_make_grid(polygon_sf, cellsize = spacing, what = "centers")
    grid <- grid[polygon_sf]
    grid <- sf::st_sf(geometry = grid, crs = sf::st_crs(polygon_sf))

    # Add coordinates
    coords <- sf::st_coordinates(grid)
    grid <- grid |> dplyr::mutate(longitude = coords[, 1], latitude = coords[, 2])

    sav_msg_success("Grid of {nrow(grid)} points successfully generated from AOI.")
    return(list(polygon = polygon_sf, points = grid))
}
