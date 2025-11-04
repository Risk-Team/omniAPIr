# Test biodiversity API functions
test_that("get_invasive_alien_species works with basic parameters", {
    skip_if_not_installed("httr")
    skip_if_not_installed("rgbif")

    result <- get_invasive_alien_species(
        country = "US",
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("get_and_process_ibat_data works with basic parameters", {
    skip_if_not_installed("httr")
    skip_if_not_installed("curl")

    # This test might be slow due to file download
    result <- get_and_process_ibat_data(
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("get_invasive_alien_species handles different countries", {
    skip_if_not_installed("httr")
    skip_if_not_installed("rgbif")

    # Test with different country codes
    result_us <- get_invasive_alien_species(
        country = "US",
        mrv = 2,
        verbose = FALSE
    )

    expect_s3_class(result_us, "data.frame")

    result_ca <- get_invasive_alien_species(
        country = "CA",
        mrv = 2,
        verbose = FALSE
    )

    expect_s3_class(result_ca, "data.frame")
})
