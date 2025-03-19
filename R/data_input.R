#' Handle inputs
#'
#' @export

sav_data_input <- function() {
    cli::cli_inform("Handle input")
}

#' Master function to read SAV data from different formats
#'
#' @param file_path Character. Path to the input file.
#' @param spacing Numeric. Spacing for grid generation (if AOI is used).
#' @param layer Character. Layer name for multi-layer spatial files (default: NULL).
#' @param crs Numeric. Coordinate Reference System (CRS) of the input data. Default is 3857
#' @param export Optional. Folder path to export outputs as .gpkg.
#' @return A list with `points` (sf object) and `polygon` (sf object).
#' @importFrom sf st_read st_geometry_type st_union st_convex_hull st_sf st_transform st_crs
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
#'     mean_fetch_km = c(2.5, 3.0, 2.8),
#'     turbidity = c(1.2, 2.3, 1.8),
#'     substrate_limits = c(TRUE, FALSE, TRUE)
#' ), temp_csv, row.names = FALSE)
#'
#' tmp <- read_sav(temp_csv, crs = 3857)
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
#' tmp <- read_sav(temp_file, spacing = 500, crs = 3857)
#' head(tmp$points)
#' plot(st_geometry(tmp$polygon))
#' plot(st_geometry(tmp$points), add = TRUE)
read_sav <- function(file_path, spacing = 500, layer = NULL, crs = 3857, export = NULL) {
    if (!file.exists(file_path)) {
        rlang::abort(glue::glue("File not found: {file_path}"))
    }
    sav_msg_info("Determining file type and processing: {file_path}")

    file_ext <- tools::file_ext(file_path)

    if (file_ext == "csv") {
        points_sf <- read_sav_csv(file_path, crs = crs)
        polygon_sf <- sf::st_sf(geometry = sf::st_union(points_sf) |> sf::st_convex_hull())
    } else if (file_ext %in% c("shp", "geojson", "gpkg", "gbd")) {
        if (file_ext == "gbd") {
            sf_obj <- sf::st_read(file_path, layer = layer, quiet = TRUE)
        } else {
            sf_obj <- sf::st_read(file_path, quiet = TRUE)
        }

        geom_type <- sf::st_geometry_type(sf_obj)

        if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
            points_sf <- read_sav_pts(file_path)
            polygon_sf <- sf::st_sf(geometry = sf::st_union(points_sf) |> sf::st_convex_hull())
        } else if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
            aoi_result <- read_sav_aoi(file_path, spacing)
            points_sf <- aoi_result$points
            polygon_sf <- aoi_result$polygon
        } else {
            rlang::abort("Unsupported spatial file type. Expected point or polygon geometries.")
        }
    } else {
        rlang::abort("Unsupported file extension. Expected csv, shp, geojson, gpkg, or gbd.")
    }

    if (!is.null(export)) {
        sf::st_write(points_sf, file.path(export, "sav_points.gpkg"), delete_dsn = TRUE)
        sf::st_write(polygon_sf, file.path(export, "sav_polygon.gpkg"), delete_dsn = TRUE)
        sav_msg_success("Exported outputs to {export}.")
    }

    return(list(points = points_sf, polygon = polygon_sf))
}


#' Read a CSV file and convert to sf object
#'
#' @param file_path Character. Path to the CSV file.
#' @param crs Numeric. Coordinate Reference System (CRS) of the input data. Default is 3857
#' @return An sf object with required and optional columns.
#' @importFrom vroom vroom
#' @importFrom sf st_as_sf st_transform st_crs
#' @importFrom dplyr select any_of mutate
#' @export
#'
#' @examples
#' # Example CSV file creation
#' temp_csv <- tempfile(fileext = ".csv")
#' write.csv(data.frame(
#'     longitude = c(-82.5, -83.0, -83.2),
#'     latitude = c(42.5, 42.8, 42.6),
#'     depth_m = c(5, 10, 7),
#'     mean_fetch_km = c(2.5, 3.0, 2.8),
#'     turbidity = c(1.2, 2.3, 1.8),
#'     substrate_limits = c(TRUE, FALSE, TRUE)
#' ), temp_csv, row.names = FALSE)
#'
#' # Read the CSV and convert to sf
#' read_sav_csv(temp_csv, crs = 4326)
read_sav_csv <- function(file_path, crs = 3857) {
    if (!file.exists(file_path)) {
        rlang::abort(glue::glue("File not found: {file_path}"))
    }
    sav_msg_info("Reading CSV file: {file_path}")

    # Read CSV
    df <- vroom::vroom(file_path, col_types = vroom::cols())

    # Validate required columns
    required_cols <- c("longitude", "latitude")
    optional_cols <- c("depth_m", "mean_fetch_km", "turbidity", "substrate_limits")
    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
        rlang::abort("Missing required columns: {paste(missing_cols, collapse = ', ')}. Only the following formatting is allowed: Required - {paste(required_cols, collapse=', ')}, Optional - {paste(optional_cols, collapse=', ')}")
    }

    # Select relevant columns
    retained_cols <- intersect(names(df), c(required_cols, optional_cols))
    removed_cols <- setdiff(names(df), retained_cols)
    df <- df |> dplyr::select(any_of(c(required_cols, optional_cols)))

    sav_msg_info("Retained columns: {paste(retained_cols, collapse=', ')}. Removed columns: {ifelse(length(removed_cols) == 0, 'None',paste(removed_cols, collapse=', '))}")

    # Convert to sf object with user-specified CRS
    df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = crs, remove = FALSE)

    # Check CRS and transform if necessary
    if (sf::st_crs(df_sf)$epsg != 3857) {
        sav_msg_info("Transforming spatial data to EPSG:3857.")
        df_sf <- sf::st_transform(df_sf, crs = 3857)
        coords <- sf::st_coordinates(df_sf)
        df_sf <- df_sf |> dplyr::mutate(longitude = coords[, 1], latitude = coords[, 2])
    }

    sav_msg_success("CSV successfully read and converted to sf object in EPSG:3857.")
    return(df_sf)
}



#' Read a spatial points file and convert to sf object
#'
#' @param file_path Character. Path to the spatial file.
#' @return An sf object with required and optional columns.
#' @importFrom sf st_read st_coordinates st_geometry_type st_transform st_crs
#' @importFrom dplyr select mutate any_of
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
read_sav_pts <- function(file_path) {
    if (!file.exists(file_path)) {
        rlang::abort(glue::glue("File not found: {file_path}"))
    }
    sav_msg_info("Reading spatial points file: {file_path}")

    # Read spatial file
    sf_obj <- sf::st_read(file_path, quiet = TRUE)

    # Ensure it's a point geometry
    if (!all(sf::st_geometry_type(sf_obj) %in% c("POINT", "MULTIPOINT"))) {
        rlang::abort("The provided spatial file does not contain point geometries.")
    }

    # Check CRS and transform if necessary
    if (sf::st_crs(sf_obj)$epsg != 3857) {
        sav_msg_info("Transforming spatial data to EPSG:3857.")
        sf_obj <- sf::st_transform(sf_obj, crs = 3857)
    }

    # Extract coordinates regardless of original CRS
    coords <- sf::st_coordinates(sf_obj)
    sf_obj <- sf_obj |> dplyr::mutate(longitude = coords[, 1], latitude = coords[, 2])

    # Select relevant columns
    required_cols <- c("longitude", "latitude")
    optional_cols <- c("depth_m", "mean_fetch_km", "turbidity", "substrate_limits")
    retained_cols <- intersect(names(sf_obj), c(required_cols, optional_cols))
    removed_cols <- setdiff(names(sf_obj), c(retained_cols, "geom"))
    sf_obj <- sf_obj |> dplyr::select(any_of(c(required_cols, optional_cols)))

    sav_msg_info("Retained columns: {paste(retained_cols, collapse=', ')}. Removed columns: {ifelse(length(removed_cols) == 0, 'None',paste(removed_cols, collapse=', '))}")

    sav_msg_success("Spatial points file successfully read and processed.")
    return(sf_obj)
}



#' Read a spatial polygon file and generate a grid of points
#'
#' @param file_path Character. Path to the spatial polygon file.
#' @param spacing Numeric. Distance between points in meters.
#' @return A list with the original polygon and a grid of points.
#' @importFrom sf st_read st_geometry_type st_make_grid st_intersection st_sf st_transform st_crs
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
read_sav_aoi <- function(file_path, spacing = 500) {
    if (!file.exists(file_path)) {
        rlang::abort(glue::glue("File not found: {file_path}"))
    }
    sav_msg_info("Reading spatial polygon file: {file_path}")

    # Read spatial file
    polygon_sf <- sf::st_read(file_path, quiet = TRUE)

    # Ensure it's a polygon
    if (!all(sf::st_geometry_type(polygon_sf) %in% c("POLYGON", "MULTIPOLYGON"))) {
        rlang::abort("The provided spatial file does not contain polygon geometries.")
    }

    # Check CRS and transform if necessary
    if (sf::st_crs(polygon_sf)$epsg != 3857) {
        sav_msg_info("Transforming spatial data to EPSG:3857.")
        polygon_sf <- sf::st_transform(polygon_sf, crs = 3857)
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
