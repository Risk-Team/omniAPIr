# Test FAOSTAT functions
test_that("get_faostat_data works with livestock data", {
    skip_if_not_installed("httr")

    # Test with cattle data (should work without authentication)
    result <- get_faostat_data(
        element = "2111",
        item = "cattle",
        database = "QCL",
        iso3 = "USA",
        mrv = 5,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(
        c("isocode", "Item", "Year", "Value", "Unit") %in% names(result)
    ))
    expect_equal(unique(result$isocode), "USA")
})

test_that("get_faostat_data works with crop production", {
    skip_if_not_installed("httr")

    result <- get_faostat_data(
        element = "2413",
        item = "wheat",
        database = "QCL",
        iso3 = "USA",
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
})

test_that("list_faostat_metadata works for databases", {
    skip_if_not_installed("httr")

    result <- list_faostat_metadata("databases", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "label") %in% names(result)))
})

test_that("list_faostat_metadata works for elements", {
    skip_if_not_installed("httr")

    result <- list_faostat_metadata(
        "elements",
        database = "QCL",
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "label") %in% names(result)))
})

test_that("list_faostat_metadata works for items", {
    skip_if_not_installed("httr")

    result <- list_faostat_metadata("items", database = "QCL", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "label") %in% names(result)))
})

test_that("get_faostat_data handles invalid items gracefully", {
    skip_if_not_installed("httr")

    expect_error(
        get_faostat_data(
            element = "2111",
            item = "invalid_animal",
            database = "QCL",
            use_lookup = TRUE
        ),
        "Invalid item"
    )
})
