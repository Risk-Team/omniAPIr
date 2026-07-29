test_that("latest FAOSTAT release is selected from live metadata shape", {
    local_mocked_bindings(
        .faostat_api_get = function(...) {
            list(data = data.frame(
                code = c("current", "7S2024", "8S2025", "7S2026"),
                label = c(
                    "Preview",
                    "July 2024 (SOFI report)",
                    "August 2025",
                    "July 2026 (SOFI report)"
                )
            ))
        },
        .package = "omniAPIr"
    )

    expect_equal(
        omniAPIr:::.latest_faostat_release("CAHD"),
        "7S2026"
    )
})

test_that("CAHD requests discover and use the latest release by default", {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    csv <- paste(
        "Area Code (ISO3),Item Code,Item,Element Code,Element,Year,Value,Unit",
        paste(
            "KEN,70040,Cost of a healthy diet (CoHD),6226,Value,",
            current_year,
            ",4.5,Int$ (PPP) per person per day",
            sep = ""
        ),
        sep = "\n"
    )
    seen <- new.env(parent = emptyenv())

    local_mocked_bindings(
        .latest_faostat_release = function(...) "7S2099",
        .get_faostat_token = function(verbose = FALSE) "token",
        .package = "omniAPIr"
    )
    local_mocked_bindings(
        req_perform = function(req) {
            seen$url <- req$url
            httr2::response(body = charToRaw(csv))
        },
        .package = "httr2"
    )

    result <- suppressMessages(get_faostat_data(
        element = "6120",
        item = "healthy_diet",
        database = "CAHD",
        iso3 = "KEN",
        mrv = 1,
        verbose = FALSE
    ))

    expect_true(grepl("release=7S2099", seen$url, fixed = TRUE))
    expect_equal(result$isocode, "KEN")
    expect_equal(result$Year, current_year)
})
