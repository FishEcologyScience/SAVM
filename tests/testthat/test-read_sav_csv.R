withr::with_options(
  list(savm.verbose = "quiet"),
  {
    test_that("read_sav_csv() correctly reads a valid CSV file", {
      # Create a temporary CSV file
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2),
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7),
        mean_fetch_km = c(2.5, 3.0, 2.8),
        secchi = c(1.2, 2.3, 1.8),
        substrate = c(TRUE, FALSE, TRUE),
        limitation = c(FALSE, FALSE, TRUE)
      ), temp_csv, row.names = FALSE)

      # Run the function
      result <- read_sav_csv(temp_csv, crs = 32617, crs_input = 4326)

      # Check outputs
      expect_s3_class(result, "sf") # Should return an sf object
      expect_true("geometry" %in% colnames(result)) # Ensure it has a geometry column
      expect_gt(nrow(result), 0) # Ensure at least one row exists
    })

    test_that("read_sav_csv() correctly transforms CRS when needed", {
      # Create a CSV file in EPSG:4326
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2),
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7),
        mean_fetch_km = c(2.5, 3.0, 2.8),
        secchi = c(1.2, 2.3, 1.8),
        substrate = c(TRUE, FALSE, TRUE),
        limitation = c(FALSE, FALSE, TRUE)
      ), temp_csv, row.names = FALSE)

      # Run the function with default CRS (should transform to EPSG:32617)
      result <- read_sav_csv(temp_csv, crs_input = 4326)
      expect_true(sf::st_crs(result)$epsg == 32617)
    })

    test_that("read_sav_csv() correctly retains required and optional columns", {
      # Create a CSV file with extra columns
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0),
        latitude = c(42.5, 42.8),
        depth_m = c(5, 10),
        extra_col = c("A", "B") # This column should be removed
      ), temp_csv, row.names = FALSE)

      result <- read_sav_csv(temp_csv, crs = 32617)

      # Expected columns
      expected_cols <- c("longitude", "latitude", "depth_m", "mean_fetch_km", "secchi", "substrate", "limitation", "geometry")

      expect_setequal(colnames(result), intersect(colnames(result), expected_cols)) # Ensure only expected columns exist
      expect_false("extra_col" %in% colnames(result)) # Extra column should be removed
    })

    test_that("read_sav_csv() throws an error when required columns are missing", {
      # Create a CSV file missing longitude
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2),
        depth_m = c(5, 10, 7),
        mean_fetch_km = c(2.5, 3.0, 2.8),
        secchi = c(1.2, 2.3, 1.8),
        substrate = c(TRUE, FALSE, TRUE)
      ), temp_csv, row.names = FALSE)
      # Expect an error about missing required columns
      expect_error(read_sav_csv(temp_csv), "Missing required columns")
    })

    test_that("read_sav_csv() handles missing files gracefully", {
      suppressWarnings(
        expect_error(
          read_sav_csv("nonexistent_file.csv"), "cannot open the connection"
        )
      )
    })

    test_that("read_sav_csv() correctly updates longitude and latitude after CRS transformation", {
      # Create a CSV file in EPSG:4326
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2),
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7),
        mean_fetch_km = c(2.5, 3.0, 2.8),
        secchi = c(1.2, 2.3, 1.8),
        substrate = c(TRUE, FALSE, TRUE)
      ), temp_csv, row.names = FALSE)
      # Run function with a non-32617 CRS (to trigger transformation)
      result <- read_sav_csv(temp_csv, crs = 32617, crs_input = 4326)

      # Extract transformed coordinates
      transformed_coords <- sf::st_coordinates(result)

      # Ensure longitude and latitude are updated
      expect_equal(result$longitude, transformed_coords[, 1])
      expect_equal(result$latitude, transformed_coords[, 2])
    })

    test_that("read_sav_csv() correctly handles different user-specified CRS", {
      # Create a CSV file with coordinates in EPSG:4326
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0),
        latitude = c(42.5, 42.8),
        depth_m = c(5, 10)
      ), temp_csv, row.names = FALSE)

      # Run function with EPSG:26917 (UTM zone 17N)
      result <- read_sav_csv(temp_csv, crs = 26917, crs_input = 4326)
      expect_true(sf::st_crs(result)$epsg == 26917)
    })
  }
)
