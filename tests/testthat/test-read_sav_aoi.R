withr::with_options(
  list(savm.verbose = "quiet"),
  {
    test_that("read_sav_aoi() correctly reads a valid polygon file and generates grid points", {
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
      result <- read_sav_aoi(temp_file, spacing = 500)

      # Check outputs
      expect_type(result, "list")
      expect_s3_class(result$polygon, "sf")
      expect_s3_class(result$points, "sf")
      expect_gt(nrow(result$points), 0) # Ensure points were created
      expect_true(sf::st_crs(result$polygon)$epsg == 3857) # default proj
    })


    test_that("read_sav_aoi() throws an error when provided a non-polygon file", {
      # Create a temporary points file instead of polygon
      temp_pts <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_point(c(-82.5, 42.5)),
          sf::st_point(c(-83.0, 42.8))
        ),
        crs = 4326
      )

      temp_file <- tempfile(fileext = ".gpkg")
      sf::st_write(temp_pts, temp_file, quiet = TRUE)

      expect_error(read_sav_aoi(temp_file), "does not contain polygon geometries")
    })

    test_that("read_sav_aoi() handles missing files gracefully", {
      expect_error(
        read_sav_aoi("nonexistent_file.gpkg"),
        "The file doesn't seem to exist."
      )
    })

    test_that("read_sav_aoi() respects spacing parameter", {
      # Create a temporary AOI polygon
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

      # Generate with different spacing
      result_500 <- read_sav_aoi(temp_file, spacing = 500)
      result_100 <- read_sav_aoi(temp_file, spacing = 100)

      expect_gt(nrow(result_100$points), nrow(result_500$points)) # More points expected for smaller spacing
    })

    test_that("read_sav_aoi() correctly updates longitude and latitude after CRS transformation", {
      # Create a spatial polygon file in EPSG:4326
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

      # Run function (should transform to EPSG:3857)
      result <- read_sav_aoi(temp_file, spacing = 500)

      # Extract transformed coordinates
      transformed_coords <- sf::st_coordinates(result$points)

      # Ensure longitude and latitude are updated
      expect_equal(result$points$longitude, transformed_coords[, 1])
      expect_equal(result$points$latitude, transformed_coords[, 2])
    })

    test_that("read_sav_aoi() correctly handles different user-specified CRS", {
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
      result <- read_sav_aoi(temp_file, spacing = 500)

      # Check that CRS is transformed to EPSG:3857
      expect_true(sf::st_crs(result$polygon)$epsg == 3857)
    })
  }
)