le_bound <- system.file("example", "lake_erie.gpkg", package = "SAVM") |>
    sf::st_read(quiet = TRUE)

le_bound_merc <- le_bound |> sf::st_transform(crs = 3857)

le_pt <- system.file("example", "le_points.geojson", package = "SAVM") |>
    sf::st_read(quiet = TRUE)

le_pt_mid <- system.file("example", "le_middle.geojson", package = "SAVM") |>
    sf::st_read(quiet = TRUE)

le_pt_out <- system.file("example", "le_land.geojson", package = "SAVM") |>
    sf::st_read(quiet = TRUE)


test_that("helpers work", {
    expect_false(is_proj_unit_meter(4326))
    expect_false(is_proj_unit_meter(sf::st_crs(4326)))
    expect_true(is_proj_unit_meter(3857))
    #
    expect_true(valid_points(le_pt))
    expect_error(valid_points(le_bound), "Geometries in `le_bound` must be of type `POINT`")
    expect_true(valid_polygon(le_bound))
    expect_error(
        valid_polygon(le_pt),
        "Geometries in `le_pt` must be of type `POLYGON` or `MULTIPOLYGON`."
    )
    #
    expect_true(valid_polygon_contains_points(le_pt, le_bound))
    expect_error(valid_polygon_contains_points(le_pt_out, le_bound))
})



test_that("helpers work", {
    withr::with_options(
        list(savm.verbose = "quiet"),
        {
            expect_error(
                compute_fetch(le_pt, le_bound, 10, max_dist = 15000),
                "Projection units must be meters."
            )
            expect_error(
                compute_fetch(le_pt_out, le_bound_merc, 10, max_dist = 15000),
                "`polygon` must include `points`"
            )
        }
    )
})

test_that("compute_fetch() work", {
    withr::with_options(
        list(savm.verbose = "quiet"),
        {
            res <- compute_fetch(le_pt, le_bound_merc, 10, max_dist = 15000)
            expect_identical(names(res), c("mean_fetch", "sf_fetch_lines"))
            expect_identical(
                names(res$mean_fetch), 
                c("id_point", "mean_fetch", "mean_weighted_fetch")
            )
            expect_true(inherits(res$sf_fetch_lines, "sf"))
            # test with a point in the middle of the lake
            res2 <- compute_fetch(le_pt_mid, le_bound_merc, 10, max_dist = 15000)
            expect_equal(res2$mean_fetch$mean_fetch, units::as_units(15000, "m"))
            expect_equal(res2$mean_fetch$mean_weighted_fetch, units::as_units(15000, "m"))
        }
    )
})
