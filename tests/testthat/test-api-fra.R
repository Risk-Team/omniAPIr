test_that("FRA 2025 forest change uses table 1a net-change values", {
    skip_on_cran()

    periods <- c(
        "1990-2000",
        "2000-2010",
        "2010-2015",
        "2015-2020",
        "2020-2025"
    )
    result <- get_fao_fra_data(
        data_type = "forest_change",
        ref_year = 2025,
        iso3 = "KEN",
        years = NULL,
        years_groups = periods
    )
    changes <- result$fra$`2025`$KEN$forestAreaChange

    expect_equal(
        unname(vapply(
            periods,
            function(period) {
                as.numeric(
                    changes[[period]][["forestAreaNetChangeFrom1a"]]$raw
                )
            },
            numeric(1)
        )),
        c(7.561, 6.226, -98.918, 41.516, 64.47)
    )
    expect_true(all(vapply(
        periods,
        function(period) {
            is.null(changes[[period]][["forestAreaNetChange"]])
        },
        logical(1)
    )))
})
