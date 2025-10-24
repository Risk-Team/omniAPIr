# Test miscellaneous API functions
test_that("get_giga_schools_data works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_giga_schools_data(
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("get_osm_features works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_osm_features(
        bbox = c(-74.0, 40.7, -73.9, 40.8), # NYC area
        key = "amenity",
        value = "school",
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("get_ndc_data works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_ndc_data(
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("get_osm_features handles different geometry types", {
    skip_if_not_installed("httr")

    # Test with point features
    result_points <- get_osm_features(
        bbox = c(-74.0, 40.7, -73.9, 40.8),
        key = "amenity",
        value = "restaurant",
        geometry = "point",
        verbose = FALSE
    )

    expect_s3_class(result_points, "data.frame")

    # Test with polygon features
    result_polygons <- get_osm_features(
        bbox = c(-74.0, 40.7, -73.9, 40.8),
        key = "leisure",
        value = "park",
        geometry = "polygon",
        verbose = FALSE
    )

    expect_s3_class(result_polygons, "data.frame")
})
