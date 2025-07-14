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
    #
    expect_true(valid_direction(seq(0, 350, 10)))
    expect_warning(
        valid_direction(seq(0, 260, 10)),
        "Not all quadrants are covered"
    )
    expect_error(
        valid_direction(-10),
        "All directions must be within the range [0, 360].",
        fixed = TRUE
    )
})


test_that("helpers work", {
    withr::with_options(
        list(savm.verbose = "quiet"),
        {   
            expect_error(
                compute_fetch(le_pt, le_bound, n_bearings = 3),
                "`n_bearings` should be equal or greater than 4."
            )
            expect_error(
                compute_fetch(le_pt, le_bound, max_dist = 0),
                "`max_dist` must be strictly positive."
            )
            expect_error(
                compute_fetch(le_pt, le_bound, max_dist = 15000, n_bearings = 4),
                "Projection units must be meters."
            )
            expect_error(
                compute_fetch(le_pt_out, le_bound_merc),
                "`polygon` must include `points`"
            )
        }
    )
})

test_that("compute_fetch() work", {
    withr::with_options(
        list(savm.verbose = "quiet"),
        {
            res <- compute_fetch(le_pt, le_bound_merc)
            expect_identical(names(res), c("mean_fetch", "transect_lines"))
            expect_identical(
                names(res$mean_fetch),
                c(
                    "id_point", "fetch_km", "weighted_fetch_km", "fetch_km_all",
                    "weighted_fetch_km_all"
                )
            )
            expect_true(inherits(res$transect_lines, "sf"))
            expect_true(
                all(
                    c("direction", "weight", "transect_length", "rank") %in% 
                    names(res$transect_lines)
                )
            )
            # test with a point in the middle of the lake
            res2 <- compute_fetch(le_pt_mid, le_bound_merc, max_dist = 15)
            expect_equal(res2$mean_fetch$fetch_km, 15)
            expect_equal(res2$mean_fetch$weighted_fetch_km, 15)
            expect_equal(res2$mean_fetch$fetch_km_all, 15)
            expect_equal(res2$mean_fetch$weighted_fetch_km_all, 15)
        }
    )
})

test_that("compute_fetch() with wind_weight", {
    withr::with_options(
        list(savm.verbose = "quiet"),
        {
            res <- compute_fetch(le_pt_mid, le_bound_merc,
                wind_weights = data.frame(
                    direction = seq(0, 360, by = 360 / 16)[-1],
                    weight = 1
                )
            )
            expect_equal(res$mean_fetch$fetch_km, 15)
            expect_equal(res$mean_fetch$weighted_fetch_km, 15)
            expect_equal(res$mean_fetch$fetch_km_all, 15)
            expect_equal(res$mean_fetch$weighted_fetch_km_all, 15)
            #
            res2 <- compute_fetch(le_pt[1, ], le_bound_merc,
                wind_weights = data.frame(
                    direction = seq(0, 360, by = 360 / 16)[-1],
                    weight = rep(c(0, 1), each = 8)
                )
            )
            expect_equal(round(res2$mean_fetch$fetch_km, 4), 2.3682)
            expect_equal(round(res2$mean_fetch$weighted_fetch_km, 4), 1.6387)
            expect_equal(round(res2$mean_fetch$fetch_km_all, 4), 2.3682)
            expect_equal(round(res2$mean_fetch$weighted_fetch_km_all, 4), 1.6387)
        }
    )
})
