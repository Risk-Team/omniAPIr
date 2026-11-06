test_that("EMPRES resolves MMR to the country label accepted by the API", {
    expect_equal(
        .empres_country_names(c("MMR", "COD", "KHM")),
        c("Myanmar", "Democratic Republic of the Congo", "Cambodia")
    )
    expect_equal(.empres_country_names("mmr"), "Myanmar")
})

test_that("GRIIS selects the Myanmar national checklist", {
    local_mocked_bindings(
        dataset_export = function(...) data.frame(
            title = c(
                "GRIIS Checklist of Introduced and Invasive Species - Myanmar protected areas",
                "GRIIS Checklist of Introduced and Invasive Species - Myanmar",
                "Global Register of Introduced and Invasive Species - Malaysia"
            ),
            datasetKey = c("protected-key", "mmr-key", "mys-key")
        ),
        dataset_endpoint = function(dataset_key) {
            expect_equal(dataset_key, "mmr-key")
            stop("Reached the correct Myanmar checklist", call. = FALSE)
        },
        .package = "rgbif"
    )
    expect_error(
        get_invasive_alien_species("MMR", output_filename = tempfile(fileext = ".zip")),
        "Reached the correct Myanmar checklist"
    )
})
