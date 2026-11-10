test_that("EMPRES country names use UN labels with straight apostrophes", {
    expect_equal(
        .empres_country_names(c("LAO", "VNM", "TUR", "CIV", "TZA")),
        c(
            "Lao People's Democratic Republic",
            "Viet Nam",
            "Türkiye",
            "Côte d'Ivoire",
            "United Republic of Tanzania"
        )
    )
})
