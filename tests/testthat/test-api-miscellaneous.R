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

test_that("get_ndc_data works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_ndc_data(
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})
