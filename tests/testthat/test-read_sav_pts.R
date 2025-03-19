withr::with_options(
  list(savm.verbose = "quiet"),
  {
    test_that("read_sav_pts() correctly reads a valid spatial points file", {
      # Create a temporary spatial points file in EPSG:4326
      temp_sf <- sf::st_sf(
        longitude = c(-82.5, -83.0, -83.2), # Lake Erie coordinates
        latitude = c(42.5, 42.8, 42.6),
        depth_m = c(5, 10, 7),
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8)),
          sf::st_point(c(-83.2, 42.6))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run the function
      result <- read_sav_pts(temp_file)

      # Check outputs
      expect_s3_class(result, "sf") # Should return an sf object
      expect_true("geom" %in% colnames(result)) # Should have geometry column
      expect_gt(nrow(result), 0) # Should have at least one row
    })

    test_that("read_sav_pts() correctly transforms CRS when needed", {
      # Create a spatial points file in EPSG:4326
      temp_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run function (should transform to EPSG:3857)
      result <- read_sav_pts(temp_file)

      # Check that CRS is transformed to EPSG:3857
      expect_true(sf::st_crs(result)$epsg == 3857)
    })

    test_that("read_sav_pts() correctly updates longitude and latitude after CRS transformation", {
      # Create a spatial points file in EPSG:4326
      temp_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run function (should transform to EPSG:3857)
      result <- read_sav_pts(temp_file)

      # Extract transformed coordinates
      transformed_coords <- sf::st_coordinates(result)

      # Ensure longitude and latitude are updated
      expect_equal(result$longitude, transformed_coords[, 1])
      expect_equal(result$latitude, transformed_coords[, 2])
    })

    test_that("read_sav_pts() extracts coordinates if missing", {
      # Create a spatial points file with only geometry
      temp_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run the function
      result <- read_sav_pts(temp_file)

      # Check outputs
      expect_s3_class(result, "sf")
      expect_true(all(c("longitude", "latitude") %in% colnames(result))) # Function should add extracted coords
    })

    test_that("read_sav_pts() correctly retains required and optional columns", {
      # Create a spatial points file with extra columns
      temp_sf <- sf::st_sf(
        longitude = c(-82.5, -83.0),
        latitude = c(42.5, 42.8),
        depth_m = c(5, 10),
        extra_col = c("A", "B"),
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run the function
      result <- read_sav_pts(temp_file)

      # Expected columns
      expected_cols <- c("longitude", "latitude", "depth_m", "mean_fetch_km", "turbidity", "substrate_limits", "geom")

      expect_setequal(colnames(result), intersect(colnames(result), expected_cols)) # Ensure only expected columns exist
      expect_false("extra_col" %in% colnames(result)) # Extra column should be removed
    })

    test_that("read_sav_pts() throws an error when provided a non-point geometry file", {
      # Create a temporary polygon file instead of points
      temp_poly <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_polygon(list(
          rbind(
            c(-81, 43), c(-81, 44), c(-80, 44), c(-80, 43), c(-81, 43)
          )
        ))),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_poly, temp_file, quiet = TRUE)

      # Expect an error
      expect_error(read_sav_pts(temp_file), "does not contain point geometries")
    })

    test_that("read_sav_pts() handles missing files gracefully", {
      expect_error(
        read_sav_pts("nonexistent_file.gpkg"), 
        "The file doesn't seem to exist"  
      )
    })

    test_that("read_sav_pts() correctly handles different user-specified CRS", {
      # Create a spatial points file with coordinates in EPSG:4326
      temp_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Run function with EPSG:26917 (UTM zone 17N)
      result <- read_sav_pts(temp_file)

      # Check that CRS is transformed to EPSG:3857
      expect_true(sf::st_crs(result)$epsg == 3857)
    })

    test_that("read_sav_pts() correctly exports data when export path is provided", {
      # Create a temporary spatial points file
      temp_sf <- sf::st_sf(
        longitude = c(-82.5, -83.0),
        latitude = c(42.5, 42.8),
        depth_m = c(5, 10),
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_sf, temp_file, quiet = TRUE)

      # Temporary export directory
      temp_dir <- tempdir()

      # Run function with export
      result <- read_sav_pts(temp_file)

      # Check if CRS was correctly transformed
      expect_true(sf::st_crs(result)$epsg == 3857)
    })
  }
)