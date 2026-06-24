# Test UN API functions
test_that("get_unsdg_data works with basic parameters", {
    skip_if_not_installed("httr2")

    result <- get_unsdg_data(
        indicators = "SI_POV_DAY1",
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
})

test_that("get_unsdg_data normalizes UNSDG geography to isocode", {
    skip_if_not_installed("httr2")

    result <- get_unsdg_data(
        indicators = "1.4.2",
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true("isocode" %in% names(result))
    expect_gt(length(unique(stats::na.omit(result$isocode))), 1)

    kenya_rows <- result[result$isocode == "KEN", , drop = FALSE]
    if (nrow(kenya_rows) > 0) {
        expect_true(all(kenya_rows$isocode == "KEN"))
        expect_true(any(result$isocode != "KEN", na.rm = TRUE))
    }
})

test_that("get_ilo_data works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_ilo_data(
        iso3 = "USA",
        indicators = "UNE_DEAP_SEX_AGE_RT_A",
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
    if (nrow(result) > 0) {
        expect_type(result$isocode, "character")
        expect_type(result$Year, "integer")
        expect_type(result$Value, "double")
    }
})

test_that("get_ilo_data returns on invalid indicators", {
    skip_if_not_installed("httr2")

    start_time <- Sys.time()
    result <- get_ilo_data(
        iso3 = "KEN",
        indicators = "NOT_A_REAL_ILO_INDICATOR",
        mrv = 3,
        verbose = FALSE,
        max_retries = 1,
        timeout_s = 5
    )
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    expect_s3_class(result, "data.frame")
    expect_lt(elapsed, 15)
})

test_that("get_ilo_data normalizes Year and Value types", {
    skip_if_not_installed("httr2")

    result <- get_ilo_data(
        iso3 = "KEN",
        indicators = "EAR_GGAP_OCU_RT_A",
        mrv = 10,
        verbose = FALSE,
        max_retries = 1,
        timeout_s = 10
    )

    expect_s3_class(result, "data.frame")
    if (nrow(result) > 0) {
        expect_type(result$isocode, "character")
        expect_type(result$Year, "integer")
        expect_type(result$Value, "double")
        expect_true(all(is.na(result$Year) | result$Year >= 1900))
    }
})

test_that("get_undp_data preserves country rows for GII", {
    skip_if_not_installed("httr2")

    apikey <- Sys.getenv("UNDP_KEY")
    skip_if(identical(apikey, ""), "UNDP_KEY not configured")

    result <- get_undp_data(
        indicator = "gii",
        apikey = apikey,
        iso3 = NULL,
        mrv = 23,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true("countryIsoCode" %in% names(result))
    expect_true(any(result$countryIsoCode == "KEN"))
})

test_that("get_who_data works with basic parameters", {
    skip_if_not_installed("httr")

    result <- get_who_data(
        indicator = "WHOSIS_000001",
        mrv = 3,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    # May be empty if no data, but should not error
})

test_that("list_un_indicators works for WHO", {
    skip_if_not_installed("httr")

    result <- list_un_indicators("WHO", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "name") %in% names(result)))
})

test_that("list_un_indicators works for ILO", {
    skip_if_not_installed("httr")

    result <- list_un_indicators("ILO", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "name") %in% names(result)))
})

test_that("list_un_indicators works for UNSDG", {
    skip_if_not_installed("httr")

    result <- list_un_indicators("UNSDG", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(c("code", "name") %in% names(result)))
})

test_that("list_un_indicators errors on invalid source", {
    expect_error(
        list_un_indicators("InvalidSource"),
        "Unknown source"
    )
})

test_that("list_un_indicators requires conda_env for WorldBank", {
    expect_error(
        list_un_indicators("WorldBank"),
        "conda_env parameter is required"
    )
})

# Note: World Bank tests are skipped because they require Python setup
test_that("get_wb_data requires conda_env", {
    expect_error(
        get_wb_data(indicators = "SP.POP.TOTL", mrv = 3),
        "conda_env parameter is required"
    )
})

test_that("get_unesco_data returns WHC data.frame with expected columns", {
    skip_if_not_installed("httr2")

    result <- get_unesco_data(iso = "IT", dataset = "whc", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true(all(c("name_en", "iso_codes", "latitude", "longitude", "category",
                       "date_inscribed", "transboundary") %in% names(result)))
    expect_type(result$name_en, "character")
    expect_type(result$latitude, "double")
    expect_type(result$longitude, "double")
})

test_that("get_unesco_data returns ICH data.frame with expected columns", {
    skip_if_not_installed("httr2")

    result <- get_unesco_data(iso = "IT", dataset = "ich", verbose = FALSE)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true(all(c("title_en", "countries", "type_of_element_en",
                       "inscription_year") %in% names(result)))
    expect_type(result$title_en, "character")
    expect_type(result$countries, "character")
})

test_that("get_unesco_data includes transboundary WHC sites", {
    skip_if_not_installed("httr2")

    result <- get_unesco_data(iso = "IT", dataset = "whc", verbose = FALSE)

    expect_true(any(result$transboundary == "True"))
})

test_that("get_unesco_data includes multinational ICH elements", {
    skip_if_not_installed("httr2")

    result <- get_unesco_data(iso = "IT", dataset = "ich", verbose = FALSE)

    # At least one element shared with other countries (semicolon-separated)
    expect_true(any(grepl(";", result$countries)))
})

test_that("get_unesco_data returns invisible NULL for unknown ISO", {
    skip_if_not_installed("httr2")

    result <- get_unesco_data(iso = "XX", dataset = "whc", verbose = FALSE)

    expect_null(result)
})

test_that("get_unesco_data errors on invalid iso argument", {
    expect_error(get_unesco_data(iso = 123, dataset = "whc"), "`iso` must be")
    expect_error(get_unesco_data(iso = "", dataset = "whc"),  "`iso` must be")
})
