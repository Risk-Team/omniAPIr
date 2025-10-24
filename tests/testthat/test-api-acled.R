# Test ACLED API functions
test_that("get_acled_data requires authentication", {
    skip_if_not_installed("httr")

    # This should fail without proper authentication
    expect_error(
        get_acled_data(
            iso3 = "USA",
            mrv = 3,
            verbose = FALSE
        ),
        "Authentication"
    )
})

test_that("get_acled_data validates parameters", {
    expect_error(
        get_acled_data(
            iso3 = "INVALID",
            mrv = 3,
            verbose = FALSE
        ),
        "Invalid"
    )
})

# Note: Full ACLED tests would require valid API credentials
# which should not be included in public test files
