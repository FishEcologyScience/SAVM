#' Plot SAV Data Distribution
#'
#' This function generates up to four plots representing the distribution of submerged aquatic vegetation (SAV)
#' data by depth (m) and fetch (km). It visualizes SAV presence/absence (PA) and percent cover (Cover) when
#' the corresponding columns are available in the input data.
#'
#' @param data A `data.frame` containing some or all of the following columns:
#'   - `Depth`: Numeric, depth in meters.
#'   - `Fetch`: Numeric, fetch in kilometers.
#'   - `PA`: Binary (0 = absent, 1 = present), indicating SAV presence/absence.
#'   - `Cover`: Numeric, percent cover of SAV.
#'
#' @return A set of ggplot2 plots displayed in a grid layout (1-4 plots depending on available columns).
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_fill_viridis_d labs theme_minimal
#' @importFrom dplyr mutate
#' @importFrom gridExtra grid.arrange
#' @examples
#' # Example dataset
#' data <- data.frame(
#'     Depth = runif(100, 0, 15),
#'     Fetch = runif(100, 0, 15),
#'     PA = sample(0:1, 100, replace = TRUE),
#'     Cover = runif(100, 0, 100)
#' )
#'
#' # Generate plots
#' plot_sav_distribution(data)
#' @export
plot_sav_distribution <- function(data) {
    plots <- list()

    # Check for required columns
    has_depth <- "Depth" %in% names(data)
    has_fetch <- "Fetch" %in% names(data)
    has_pa <- "PA" %in% names(data)
    has_cover <- "Cover" %in% names(data)

    # Process data
    data <- dplyr::mutate(
        data,
        Depth_Bin = if (has_depth) cut(data$Depth, breaks = seq(0, 15, by = 1), include.lowest = TRUE, right = FALSE) else NULL,
        Fetch_Bin = if (has_fetch) cut(data$Fetch, breaks = seq(0, 15, by = 1), include.lowest = TRUE, right = FALSE) else NULL,
        PA_Factor = if (has_pa) factor(data$PA, labels = c("Absent", "Present")) else NULL,
        Cover_Bin = if (has_cover) cut(data$Cover, breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE) else NULL
    )

    # Cols
    cover_palette <- c(
        "#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8",
        "#D7EED1", "#9CCE97", "#54A35B", "#1A7234", "#00401A"
    )
    cols <- c("#56B4E9", "#52854C")

    # Presence/Absence by Fetch
    if (has_fetch && has_pa) {
        plots[["PA_Fetch"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Fetch_Bin, fill = PA_Factor)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = c(cols[1], cols[2])) +
            ggplot2::labs(x = "Fetch Bin", y = "Number of Records", fill = "SAV P/A") +
            ggplot2::theme_minimal()
    }

    # Presence/Absence by Depth
    if (has_depth && has_pa) {
        plots[["PA_Depth"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Depth_Bin, fill = PA_Factor)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = c(cols[1], cols[2])) +
            ggplot2::labs(x = "Depth Bin", y = "Number of Records", fill = "SAV P/A") +
            ggplot2::theme_minimal()
    }

    # Cover by Fetch
    if (has_fetch && has_cover) {
        plots[["Cover_Fetch"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Fetch_Bin, fill = Cover_Bin)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cover_palette, drop = FALSE) +
            ggplot2::labs(x = "Fetch Bin", y = "Number of Records", fill = "SAV Cover") +
            ggplot2::theme_minimal()
    }

    # Cover by Depth
    if (has_depth && has_cover) {
        plots[["Cover_Depth"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Depth_Bin, fill = Cover_Bin)) +
            ggplot2::geom_bar() +
            ggplot2::scale_fill_manual(values = cover_palette, drop = FALSE) +
            ggplot2::labs(x = "Depth Bin", y = "Number of Records", fill = "SAV Cover") +
            ggplot2::theme_minimal()
    }

    # Arrange plots dynamically
    if (length(plots) > 0) {
        do.call(gridExtra::grid.arrange, c(plots, ncol = min(2, length(plots))))
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
#' @param data A `data.frame` containing:
#'   - `Depth` (optional): Numeric, depth in meters.
#'   - `Fetch` (optional): Numeric, fetch in kilometers.
#'   - `PA`: Binary (0 = absent, 1 = present), indicating SAV presence/absence.
#'
#' @return One or two ggplot2 density plots visualizing SAV presence/absence by depth and/or fetch.
#' @importFrom ggplot2 ggplot aes geom_density geom_vline scale_fill_manual scale_color_manual labs theme_minimal
#' @importFrom dplyr filter summarise group_by
#' @importFrom gridExtra grid.arrange
#' @examples
#' # Example dataset
#' data <- data.frame(
#'     Depth = runif(200, 0, 20),
#'     Fetch = runif(200, 0, 15),
#'     PA = sample(0:1, 200, replace = TRUE)
#' )
#'
#' # Generate density plot(s)
#' plot_sav_density(data)
#' @export
plot_sav_density <- function(data) {
    #library(units)
    plots <- list()

    # Check if PA column exists
    if (!"PA" %in% names(data)) {
        stop("Data must contain 'PA' column.")
    }

    # Convert PA to factor
    data$PA_Factor <- factor(data$PA, labels = c("Absent", "Present"))

    # Colors
    cols <- c("#56B4E9", "#52854C")

    # Density plot for Depth
    if ("Depth" %in% names(data)) {
        mean_depths <- dplyr::summarise(
            dplyr::group_by(data, PA_Factor),
            Mean_Value = mean(Depth, na.rm = TRUE)
        )

        plots[["Depth"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Depth, fill = PA_Factor)) +
            ggplot2::geom_density(alpha = 0.5) +
            ggplot2::geom_vline(
                data = mean_depths, ggplot2::aes(xintercept = Mean_Value, color = PA_Factor),
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
    if ("Fetch" %in% names(data)) {
        mean_fetch <- dplyr::summarise(
            dplyr::group_by(data, PA_Factor),
            Mean_Value = mean(Fetch, na.rm = TRUE)
        )

        plots[["Fetch"]] <- ggplot2::ggplot(data, ggplot2::aes(x = Fetch, fill = PA_Factor)) +
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

    # Arrange dynamically
    if (length(plots) > 0) {
        do.call(gridExtra::grid.arrange, c(plots, ncol = length(plots)))
    } else {
        rlang::abort("No suitable columns found for plotting.")
    }
}