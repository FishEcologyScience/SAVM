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
#'   Depth = runif(100, 0, 15),
#'   Fetch = runif(100, 0, 15),
#'   PA = sample(0:1, 100, replace = TRUE),
#'   Cover = runif(100, 0, 100)
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
