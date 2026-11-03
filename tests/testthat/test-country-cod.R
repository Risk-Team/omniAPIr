test_that("EMPRES resolves COD to its API country label", {
    expect_equal(
        .empres_country_names(c("COD", "GAB", "CMR")),
        c("Democratic Republic of the Congo", "Gabon", "Cameroon")
    )
    expect_equal(.empres_country_names("cod"), "Democratic Republic of the Congo")
})

test_that("GRIIS selects the COD national checklist rather than Congo or a protected area", {
    for (country_name in c("Democratic Republic of Congo", "Democratic Republic of the Congo")) {
        local_mocked_bindings(
            dataset_export = function(...) data.frame(
                title = paste("Global Register of Introduced and Invasive Species -", c(
                    "Congo", paste(country_name, "protected areas"), country_name
                )),
                datasetKey = c("cog-key", "protected-key", "cod-key")
            ),
            dataset_endpoint = function(dataset_key) {
                expect_equal(dataset_key, "cod-key")
                stop("Reached the correct COD checklist", call. = FALSE)
            },
            .package = "rgbif"
        )
        expect_error(
            get_invasive_alien_species("COD", output_filename = tempfile(fileext = ".zip")),
            "Reached the correct COD checklist"
        )
    }
})
