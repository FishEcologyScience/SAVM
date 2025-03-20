withr::with_options(
  list(savm.verbose = "quiet"),
  {
    test_that("read_sav() correctly reads a CSV file", {
      # Create a temporary CSV file
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2), # Lake Erie coordinates
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7),
        mean_fetch_km = c(2.5, 3.0, 2.8),
        secchi = c(1.2, 2.3, 1.8),
        substrate = c(TRUE, FALSE, TRUE),
        limitation = c(FALSE, FALSE, TRUE)
      ), temp_csv, row.names = FALSE)

      # Run the function
      result <- read_sav(temp_csv)

      # Check outputs
      expect_type(result, "list")
      expect_s3_class(result$points, "sf") # Should return an sf object
      expect_s3_class(result$polygon, "sf") # Should return a polygon
      expect_true("geometry" %in% colnames(result$points)) # Points should have geometry
      expect_gt(nrow(result$points), 0) # Ensure at least one point exists
      expect_true(sf::st_crs(result$points)$epsg == 32617) # CRS should be transformed to 32617
    })

    test_that("read_sav() correctly reads a spatial polygon file (AOI)", {
      # Create a temporary AOI polygon in EPSG:4326
      temp_poly <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_polygon(list(
          rbind(
            c(-82.5, 42.5), c(-82.5, 42.8), c(-82.0, 42.8), c(-82.0, 42.5), c(-82.5, 42.5)
          )
        ))),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_poly, temp_file, quiet = TRUE)

      # Run the function
      result <- read_sav(temp_file, spacing = 500, crs = 32617)

      # Check outputs
      expect_type(result, "list")
      expect_s3_class(result$points, "sf")
      expect_s3_class(result$polygon, "sf")
      expect_gt(nrow(result$points), 0) # Ensure points were created
      expect_true(sf::st_crs(result$polygon)$epsg == 32617) # CRS should be transformed to 32617
    })

    test_that("read_sav() correctly reads a spatial points file", {
      # Create a temporary spatial points file in EPSG:4326
      temp_pts <- sf::st_sf(
        longitude = c(-82.5, -83.0, -83.2),
        latitude = c(42.5, 42.8, 42.6),
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8)),
          sf::st_point(c(-83.2, 42.6))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_pts, temp_file, quiet = TRUE)

      # Run the function
      result <- read_sav(temp_file)

      # Check outputs
      expect_type(result, "list")
      expect_s3_class(result$points, "sf")
      expect_s3_class(result$polygon, "sf")
      expect_gt(nrow(result$points), 0)
      expect_true(sf::st_crs(result$points)$epsg == 32617) # CRS should be transformed to 32617
    })

    test_that("read_sav() throws an error when provided an unsupported file type", {
      temp_txt <- tempfile(fileext = ".txt")
      writeLines("This is a test", temp_txt)

      expect_error(read_sav(temp_txt), "Unsupported file extension")
    })

    test_that("read_sav() throws an error when file does not exist", {
      expect_error(
        read_sav("nonexistent_file.gpkg"),
        "The file doesn't seem to exist."
      )
    })

    test_that("read_sav() correctly exports data when export path is provided", {
      # Create a temporary CSV file
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0, -83.2),
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7)
      ), temp_csv, row.names = FALSE)

      # Temporary export directory
      temp_dir <- tempdir()

      # Run function with export
      result <- read_sav(temp_csv, export = temp_dir)

      # Check if files were created
      expect_true(file.exists(file.path(temp_dir, "sav_points.gpkg")))
      expect_true(file.exists(file.path(temp_dir, "sav_polygon.gpkg")))
    })

    test_that("read_sav() correctly updates longitude and latitude after CRS transformation", {
      # Create a CSV file with EPSG:4326
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data.frame(
        longitude = c(-82.5, -83.0),
        latitude = c(42.5, 42.8),
        depth_m = c(5, 10)
      ), temp_csv, row.names = FALSE)

      # Run function (should transform to EPSG:32617)
      result <- read_sav(temp_csv)

      # Extract transformed coordinates
      transformed_coords <- sf::st_coordinates(result$points)

      # Ensure longitude and latitude are updated
      expect_equal(result$points$longitude, transformed_coords[, 1])
      expect_equal(result$points$latitude, transformed_coords[, 2])
    })

    test_that("read_sav() correctly handles different user-specified CRS", {
      # Create a spatial polygon file with coordinates in EPSG:4326
      temp_poly <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_polygon(list(
          rbind(
            c(-82.5, 42.5), c(-82.5, 42.8), c(-82.0, 42.8), c(-82.0, 42.5), c(-82.5, 42.5)
          )
        ))),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_poly, temp_file, quiet = TRUE)

      # Run function with EPSG:26917 (UTM zone 17N)
      result <- read_sav(temp_file, spacing = 500, crs = 26917)

      # Check that CRS is transformed to EPSG:26917
      expect_true(sf::st_crs(result$polygon)$epsg == 26917)
    })
  }
)
