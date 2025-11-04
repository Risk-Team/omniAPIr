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

test_that("get_fishwatch_data works with basic parameters", {
    skip_if_not_installed("gfwr")

    # Test with minimal parameters (will fail without API key, but should not error on validation)
    expect_error(
        get_fishwatch_data(
            spatial_resolution = "LOW",
            temporal_resolution = "MONTHLY",
            start_date = "2023-01-01",
            end_date = "2023-12-31",
            region_source = "EEZ",
            region = "ESP",
            api_key = NULL,
            verbose = FALSE
        ),
        "api_key parameter is required"
    )
})

test_that("get_fishwatch_data validates required parameters", {
    skip_if_not_installed("gfwr")

    # Test missing api_key
    expect_error(
        get_fishwatch_data(
            spatial_resolution = "LOW",
            temporal_resolution = "MONTHLY",
            start_date = "2023-01-01",
            end_date = "2023-12-31",
            region_source = "EEZ",
            region = "ESP",
            api_key = NULL
        ),
        "api_key parameter is required for Global Fishing Watch API"
    )

    # Test missing region_source
    expect_error(
        get_fishwatch_data(
            spatial_resolution = "LOW",
            temporal_resolution = "MONTHLY",
            start_date = "2023-01-01",
            end_date = "2023-12-31",
            region_source = NULL,
            region = "ESP",
            api_key = "test_key"
        ),
        "region_source parameter is required"
    )

    # Test missing region
    expect_error(
        get_fishwatch_data(
            spatial_resolution = "LOW",
            temporal_resolution = "MONTHLY",
            start_date = "2023-01-01",
            end_date = "2023-12-31",
            region_source = "EEZ",
            region = NULL,
            api_key = "test_key"
        ),
        "region parameter is required"
    )
})
