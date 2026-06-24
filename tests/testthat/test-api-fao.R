# Test FAOSTAT functions
test_that("EMPRES species helper builds public API tag tuples", {
    expect_equal(
        omniAPIr:::.empres_species_filter(
            specie = "cattle",
            specie_type = "domestic",
            specie_class = "mammal"
        ),
        "<type:Domestic><class:Mammal><specie:Cattle>"
    )

    expect_equal(
        omniAPIr:::.empres_species_filter(
            specie = c("Cattle", "Chicken"),
            specie_type = "Domestic"
        ),
        "<type:Domestic><specie:Cattle>,<type:Domestic><specie:Chicken>"
    )

    expect_equal(
        omniAPIr:::.empres_species_filter("<type:domestic><class:mammal><specie:cattle>"),
        "<type:Domestic><class:Mammal><specie:Cattle>"
    )

    expect_equal(
        omniAPIr:::.empres_species_filter("All"),
        "<all>"
    )

    expect_equal(
        omniAPIr:::.empres_species_filter("<all>"),
        "<all>"
    )
})

test_that("EMPRES public API key requirement is explicit before network calls", {
    expect_error(
        get_empres_data(api_key = NA_character_),
        "requires an X-API-Key"
    )
})

test_that("EMPRES validates disease values before network calls", {
    expect_error(
        get_empres_data(disease = "Not a real disease"),
        "Supported disease values"
    )

    expect_equal(
        omniAPIr:::.empres_validate_disease("foot and mouth disease -- O"),
        "Foot and mouth disease -- O"
    )
})

test_that("EMPRES validates species type and class values", {
    expect_error(
        get_empres_data(specie = "Not a real species"),
        "Supported values are"
    )

    expect_error(
        omniAPIr:::.empres_species_filter(specie = "Cattle", specie_type = "Farmed"),
        "Supported values are: Captive, Domestic, Environmental sample, Wild"
    )

    expect_error(
        omniAPIr:::.empres_species_filter(specie = "Cattle", specie_class = "Livestock"),
        "Supported values are: Arthropod, Birds, Environmental sample, Fish, Mammal, Reptiles"
    )
})

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
