#' Plot SAV Data Distribution
#'
#' This function generates up to four plots representing the distribution of 
#' submerged aquatic vegetation (SAV) data by depth (m) and fetch (km). It 
#' visualizes SAV presence/absence (PA) and percent cover (Cover) when the 
#' corresponding columns are available in the input data.
#'
#' @param dat {`data.frame`}\cr{}
#' A data frame containing some or all of the following columns:
#'   - `depth_m`: Numeric, depth in meters.
#'   - `fetch_km`: Numeric, fetch in kilometers.
#'   - `pa`: Binary (0 = absent, 1 = present), indicating SAV presence/absence.
#'   - `cover`: Numeric, percent cover of SAV.
#' @param type {`character vector`}\cr{}
#' Character vector specifying the type of plots to generate. Options:
#'   - `"pa"` (default) for presence/absence plots
#'   - `"cover"` (default) for cover percentage plots
#' @param predictors {`character vector`}\cr{}
#' Character vector specifying which predictors to use. Options:
#'   - `"depth"` (default) for depth-based plots
#'   - `"fetch"` (default) for fetch-based plots
#' @param post_hoc {`logical`}\cr{}
#' Logical value indicating whether to use post-hoc analyzed columns 
#' (`pa_post_hoc`, `cover_post_hoc`) instead of raw columns (`pa`, `cover`). 
#' Default is `TRUE`.
#' @param max_depth {`numeric`}\cr{}
#' Numeric value specifying the maximum depth bin (default: 30 meters).
#' @param max_fetch {`numeric`}\cr{}
#' Numeric value specifying the maximum fetch bin (default: 15 km).
#'
#' @return A set of ggplot2 plots displayed in a grid layout.
#'
#' @rdname plot_sav
#'
#' @examples
#' # Example dataset
#' dat <- data.frame(
#'     depth_m = runif(100, 0, 15),
#'     fetch_km = runif(100, 0, 15),
#'     pa_pred = sample(0:1, 100, replace = TRUE),
#'     cover_pred = runif(100, 0, 100),
#'     pa_post_hoc = sample(0:1, 100, replace = TRUE),
#'     cover_post_hoc = runif(100, 0, 100)
#' )
#'
#' # Generate all available plots
#' plot_sav_distribution(dat)
#'
#' # Generate only presence/absence plots
#' plot_sav_distribution(dat, type = "pa")
#'
#' # Generate cover plots using only fetch as predictor
#' plot_sav_distribution(dat, type = "cover", predictors = "fetch")
#'
#' # Generate plots using post-hoc analyzed data
#' plot_sav_distribution(dat, post_hoc = TRUE)
#'
#' @export
plot_sav_distribution <- function(dat, type = c("pa", "cover"), predictors = c("depth", "fetch"), post_hoc = TRUE, max_depth = 30, max_fetch = 15) {
    plots <- list()

    # Check post-hoc
    if ("pa" %in% type && post_hoc && !"pa_post_hoc" %in% names(dat)) {
        rlang::abort("Requested post-hoc presence/absence predictions, but they are missing from the provided data.")
    }
    if ("cover" %in% type && post_hoc && !"cover_post_hoc" %in% names(dat)) {
        rlang::abort("Requested post-hoc cover predictions, but they are missing from the provided data.")
    }

    # Determine which columns to use based on post_hoc flag
    pa_col <- if (post_hoc) "pa_post_hoc" else "pa_pred"
    cover_col <- if (post_hoc) "cover_post_hoc" else "cover_pred"

    # Check for requested columns, abort if unavailable
    has_depth <- "depth_m" %in% names(dat)
    if (!has_depth && "depth" %in% predictors) 
        rlang::abort("Requested layer `depth` is unavailable in provided data)")
    has_fetch <- "fetch_km" %in% names(dat)
    if (!has_fetch && "fetch" %in% predictors) 
        rlang::abort("Requested layer `fetch` is unavailable in provided data)")
    has_pa <- pa_col %in% names(dat)
    if (!has_pa && "pa" %in% type) 
        rlang::abort("Requested layer `pa` is unavailable in provided data)")
    has_cover <- cover_col %in% names(dat)
    if (!has_cover && "cover" %in% type) 
        rlang::abort("Requested layer `cover` is unavailable in provided data)")

    # Process data
    dat <- dat |>
        dplyr::mutate(
            Depth_Bin = if (has_depth) {
                cut(
                    dat$depth_m,
                    breaks = c(seq(0, max_depth, by = 1), Inf),
                    include.lowest = TRUE, right = FALSE,
                    labels = c(paste0(seq(0, max_depth - 1, by = 1), "-", seq(1, max_depth, by = 1)), paste0(max_depth, "+"))
                )
            } else {
                NULL
            },
            Fetch_Bin = if (has_fetch) {
                cut(
                    dat$fetch_km,
                    breaks = c(seq(0, max_fetch, by = 1), Inf),
                    include.lowest = TRUE, right = FALSE,
                    labels = c(paste0(seq(0, max_fetch - 1, by = 1), "-", seq(1, max_fetch, by = 1)), paste0(max_fetch, "+"))
                )
            } else {
                NULL
            },
            PA_Factor = if (has_pa) {
                factor(c("Absent", "Present")[dat[[pa_col]] + 1])
                } else NULL,
            Cover_Bin = if (has_cover) cut(dat[, cover_col], breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE) else NULL
        )

    # Define colors
    cover_palette <- c(
        "#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8",
        "#D7EED1", "#9CCE97", "#54A35B", "#1A7234", "#00401A"
    )
    cols <- c("#56B4E9", "#52854C")

    # Presence/Absence by Fetch
    if ("pa" %in% type && "fetch" %in% predictors && has_fetch && has_pa) {
        plots[["PA_Fetch"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = Fetch_Bin, fill = PA_Factor)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cols) +
            ggplot2::labs(x = "Fetch Bin", y = "Number of Records", fill = "SAV P/A") +
            ggplot2::theme_minimal()
    }

    # Presence/Absence by Depth
    if ("pa" %in% type && "depth" %in% predictors && has_depth && has_pa) {
        plots[["PA_Depth"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = Depth_Bin, fill = PA_Factor)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cols) +
            ggplot2::labs(x = "Depth Bin", y = "Number of Records", fill = "SAV P/A") +
            ggplot2::theme_minimal()
    }

    # Cover by Fetch
    if ("cover" %in% type && "fetch" %in% predictors && has_fetch && has_cover) {
        plots[["Cover_Fetch"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = Fetch_Bin, fill = Cover_Bin)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cover_palette, drop = FALSE) +
            ggplot2::labs(x = "Fetch Bin", y = "Number of Records", fill = "SAV Cover") +
            ggplot2::theme_minimal()
    }

    # Cover by Depth
    if ("cover" %in% type && "depth" %in% predictors && has_depth && has_cover) {
        plots[["Cover_Depth"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = Depth_Bin, fill = Cover_Bin)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cover_palette, drop = FALSE) +
            ggplot2::labs(x = "Depth Bin", y = "Number of Records", fill = "SAV Cover") +
            ggplot2::theme_minimal()
    }

    # Arrange plots dynamically using patchwork
    if (length(plots) > 0) {
        combined_plot <- patchwork::wrap_plots(plots) + patchwork::plot_layout(ncol = min(2, length(plots)))
        return(combined_plot)
    } else {
        rlang::abort("No suitable columns found for plotting.")
    }
}



#' Plot SAV Density for Presence/Absence Data
#'
#' This function generates one or two density plots for submerged aquatic vegetation (SAV) presence (green)
#' and absence (blue) as a function of depth (m) and/or fetch (km). It includes vertical dotted lines
#' indicating the mean values for each.
#'
#' @param dat A `data.frame` containing:
#'   - `depth_m` (optional): Numeric, depth in meters.
#'   - `fetch_km` (optional): Numeric, fetch in kilometers.
#'   - `pa`: Binary (0 = absent, 1 = present), indicating SAV presence/absence.
#' @param predictors Character vector specifying which predictors to use. Options:
#'   - `"depth"` (default) for depth-based plots
#'   - `"fetch"` (default) for fetch-based plots
#' @param max_depth Numeric value specifying the maximum depth bin (default: 30 meters).
#' @param post_hoc Logical value indicating whether to use post-hoc analyzed column (`pa_post_hoc`) instead of raw column (`pa`). Default is `TRUE`.
#'
#' @return One or two ggplot2 density plots visualizing SAV presence/absence by depth and/or fetch.
#'
#' @rdname plot_sav
#'
#' @examples
#' # Example dataset
#' dat <- data.frame(
#'     depth_m = runif(100, 0, 15),
#'     fetch_km = runif(100, 0, 15),
#'     pa_pred = sample(0:1, 100, replace = TRUE),
#'     pa_post_hoc = sample(0:1, 100, replace = TRUE)
#' )
#'
#' # Generate all available plots
#' plot_sav_density(dat)
#'
#' # Generate depth-based density plot only
#' plot_sav_density(dat, predictors = "depth")
#'
#' # Generate fetch-based density plot only
#' plot_sav_density(dat, predictors = "fetch")
#'
#' # Generate plots using post-hoc analyzed data
#' plot_sav_density(dat, post_hoc = TRUE)
#' @export
plot_sav_density <- function(dat, predictors = c("depth", "fetch"), max_depth = 30, post_hoc = TRUE) {
    plots <- list()

    # Determine which column to use based on post_hoc flag
    pa_col <- if (post_hoc) "pa_post_hoc" else "pa_pred"

    # Check if PA column exists
    if (!pa_col %in% names(dat)) {
        rlang::abort("Data must contain the specified PA column.")
    }

    # Check for requested predictors, abort if unavailable
    has_depth <- "depth_m" %in% names(dat)
    if (!has_depth && "depth" %in% predictors) 
        rlang::abort("Requested layer `depth` is unavailable in provided data)")
    has_fetch <- "fetch_km" %in% names(dat)
    if (!has_fetch && "fetch" %in% predictors) 
        rlang::abort("Requested layer `fetch` is unavailable in provided data)")

    # Convert PA to factor
    dat$PA_Factor <- factor(c("Absent", "Present")[dat[[pa_col]] + 1])

    # Colors
    cols <- c("#56B4E9", "#52854C")

    # Density plot for Depth
    if ("depth" %in% predictors && has_depth) {
        mean_depths <- dplyr::summarise(
            dplyr::group_by(dat, PA_Factor),
            Mean_Value = mean(depth_m, na.rm = TRUE)
        )

        plots[["Depth"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = depth_m, fill = PA_Factor)) +
            ggplot2::geom_density(alpha = 0.5) +
            ggplot2::geom_vline(
                dat = mean_depths, ggplot2::aes(xintercept = Mean_Value, color = PA_Factor),
                linetype = "dotted", linewidth = 1
            ) +
            ggplot2::scale_fill_manual(values = cols) +
            ggplot2::scale_color_manual(values = cols) +
            ggplot2::labs(
                x = "Depth (m)",
                y = "Density",
                fill = "SAV P/A",
                color = "SAV P/A"
            ) +
            ggplot2::theme_minimal()
    }

    # Density plot for Fetch
    if ("fetch" %in% predictors && has_fetch) {
        mean_fetch <- dplyr::summarise(
            dplyr::group_by(dat, PA_Factor),
            Mean_Value = mean(fetch_km, na.rm = TRUE)
        )

        plots[["Fetch"]] <- ggplot2::ggplot(dat, ggplot2::aes(x = fetch_km, fill = PA_Factor)) +
            ggplot2::geom_density(alpha = 0.5) +
            ggplot2::geom_vline(
                data = mean_fetch, ggplot2::aes(xintercept = Mean_Value, color = PA_Factor),
                linetype = "dotted", linewidth = 1
            ) +
            ggplot2::scale_fill_manual(values = cols) +
            ggplot2::scale_color_manual(values = cols) +
            ggplot2::labs(
                x = "Fetch (km)",
                y = "Density",
                fill = "SAV P/A",
                color = "SAV P/A"
            ) +
            ggplot2::theme_minimal()
    }

    # Arrange dynamically using patchwork
    plots <- Filter(Negate(is.null), plots)
    if (length(plots) > 0) {
        combined_plot <- patchwork::wrap_plots(plots) + patchwork::plot_layout(ncol = min(2, length(plots)))
        return(combined_plot)
    } else {
        rlang::abort("No suitable columns found for plotting.")
    }
}

#' Create SAV Map using tmap
#'
#' This function generates an interactive or static map of submerged aquatic vegetation (SAV) data,
#' visualizing cover, presence/absence, depth, and fetch values.
#'
#' @param study_zone A list containing spatial objects:
#'   - `polygon`: An `sf` polygon object representing the area of interest.
#'   - `points`: An `sf` points object containing SAV-related attributes.
#' @param layers Character vector specifying the layers to generate. Options:
#'   - `"pa"` (default) for presence/absence model predictions
#'   - `"cover"` (default) for cover percentage model predictions
#'   - `"depth"` (default) for depth predictor values
#'   - `"fetch"` (default) for fetch predictor values
#' @param post_hoc Logical value indicating whether to use post-hoc analyzed columns (`pa_post_hoc`, `cover_post_hoc`) instead of raw columns (`pa`, `cover`). Default is `FALSE`.
#' @param interactive Logical. If `TRUE` (default), generates an interactive map. If `FALSE`, creates a static map.
#' @param export_path Character. If provided, saves the map to the specified file path. Default is `NULL`.
#'
#' @return A `tmap` map object, optionally saved to a file.
#'
#' @rdname plot_sav
#'
#' @examples
#' polygon <- system.file("example", "lake_erie.gpkg", package = "SAVM") |>
#'     sf::st_read() |>
#'     sf::st_simplify(dTolerance = 1000)
#'
#' # Create sample points with attributes
#' set.seed(42)
#' points <- sf::st_sample(polygon, 100) |>
#'     sf::st_sf() |>
#'     dplyr::mutate(
#'         cover_pred = runif(100, 0, 100),
#'         pa_pred = sample(0:1, 100, replace = TRUE) |>
#'             factor(levels = c(0, 1), labels = c("Absent", "Present")),
#'         depth_m = runif(100, 0, 15),
#'         fetch_km = runif(100, 0, 10),
#'         cover_post_hoc = runif(100, 0, 100),
#'         pa_post_hoc = sample(0:1, 100, replace = TRUE) |>
#'             factor(levels = c(0, 1), labels = c("Absent", "Present")),
#'     )
#'
#' # Combine into study_zone list
#' study_zone <- list(polygon = polygon, points = points)
#'
#' # Generate an interactive map
#' plot_sav_tmap(study_zone, interactive = TRUE)
#'
#' # Generate a static map
#' plot_sav_tmap(study_zone, layers = "cover", interactive = FALSE)
#'
#' # Save map to html file
#' \dontrun{
#' plot_sav_tmap(study_zone, export_path = "sav_map.html")
#' }
#'
#' # Visualize interactive map & save map to static png file
#' \dontrun{
#' plot_sav_tmap(study_zone, layers = "pa", export_path = "sav_map.png")
#' }
#'
#' @export
plot_sav_tmap <- function(study_zone, layers = c("pa", "cover", "depth", "fetch"), interactive = TRUE, export_path = NULL, post_hoc = TRUE) {
    # Set tmap mode
    suppressMessages(tmap::tmap_mode(if (interactive) "view" else "plot"))

    # Colors
    cols <- c("#56B4E9", "#52854C")

    # Determine which columns to use based on post_hoc flag
    pa_col <- if (post_hoc) "pa_post_hoc" else "pa_pred"
    cover_col <- if (post_hoc) "cover_post_hoc" else "cover_pred"

    # Check for requested columns, abort if unavailable
    has_depth <- "depth_m" %in% names(study_zone$points)
    if (!has_depth && "depth" %in% layers) rlang::abort("Requested layer `depth` is unavailable in provided data)")
    has_fetch <- "fetch_km" %in% names(study_zone$points)
    if (!has_fetch && "fetch" %in% layers) rlang::abort("Requested layer `fetch` is unavailable in provided data)")
    has_pa <- pa_col %in% names(study_zone$points)
    if (!has_pa && "pa" %in% layers) rlang::abort("Requested layer `pa` is unavailable in provided data)")
    has_cover <- cover_col %in% names(study_zone$points)
    if (!has_cover && "cover" %in% layers) rlang::abort("Requested layer `cover` is unavailable in provided data)")



    # Base map with polygon
    map <- tmap::tm_shape(study_zone$polygon) +
        tmap::tm_polygons(
            fill = "#000000",
            fill_alpha = 0.2,
            col_alpha = 0.5,
            fill.legend = tmap::tm_legend_hide(),
            group = "Area of interest"
        )

    suppressMessages({
        # Overlay points with different attributes if they exist
        if ("cover" %in% layers) {
            map <- map + tmap::tm_shape(study_zone$points) +
                tmap::tm_dots(
                    col = cover_col,
                    group = "Cover",
                    size = .5,
                    fill.scale = tmap::tm_scale("viridis"),
                    # palette = "viridis",
                    breaks = seq(0, 100, by = 10)
                )
        }

        if ("pa" %in% layers) {
            if (!is.factor(study_zone$points[[pa_col]])) {
                study_zone$points[[pa_col]] <- factor(
                    c("Absent", "Present")[study_zone$points[[pa_col]] + 1]
                )
            }

            map <- map + tmap::tm_shape(study_zone$points) +
                tmap::tm_dots(
                    col = pa_col,
                    group = "Presence",
                    size = 0.5,
                    palette = cols
                )
        }

        if ("depth" %in% layers) {
            map <- map + tmap::tm_shape(study_zone$points) +
                tmap::tm_dots(
                    col = "depth_m",
                    group = "Depth",
                    size = 0.5,
                    palette = viridis::magma(100),
                    breaks = seq(
                        0,
                        max(study_zone$points$depth_m) |>
                            round(0),
                        length.out = 10
                    )
                )
        }

        if ("fetch" %in% layers) {
            map <- map + tmap::tm_shape(study_zone$points) +
                tmap::tm_dots(
                    col = "fetch_km",
                    group = "Fetch",
                    size = 0.5,
                    palette = viridis::magma(100),
                    breaks = seq(
                        0,
                        max(study_zone$points$depth_m) |>
                            round(0),
                        length.out = 10
                    )
                )
        }
    })

    # Export if a path is provided
    if (!is.null(export_path)) {
        suppressMessages({
            tmap::tmap_save(map, filename = export_path)
        })
    }

    return(map)
}
