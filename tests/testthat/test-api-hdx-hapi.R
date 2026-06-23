skip_hdx_hapi <- function() {
  skip_if(
    Sys.getenv("HDX_HAPI_APP_IDENTIFIER") == "",
    "HDX_HAPI_APP_IDENTIFIER is not available"
  )
}

expect_hdx_country <- function(x, iso3, country = NULL) {
  expect_true(all(c("location_code", "location_name", "iso3", "country", "source") %in% names(x)))
  expect_true(all(x$location_code == iso3))
  expect_true(all(x$iso3 == iso3))
  expect_true(all(x$source == "HDX HAPI"))
  if (!is.null(country)) {
    expect_true(all(x$location_name == country))
    expect_true(all(x$country == country))
  }
}

expect_hdx_reference_year <- function(x) {
  expect_true(all(c("reference_period_start", "reference_period_end", "Year") %in% names(x)))
  expect_s3_class(x$reference_period_start, "POSIXct")
  expect_s3_class(x$reference_period_end, "POSIXct")
  expect_equal(x$Year, as.integer(format(x$reference_period_start, "%Y")))
}

expect_hdx_reference_overlap <- function(x, start_date, end_date) {
  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date <- as.POSIXct(end_date, tz = "UTC")
  expect_true(all(x$reference_period_start <= end_date, na.rm = TRUE))
  expect_true(all(x$reference_period_end >= start_date, na.rm = TRUE))
}

test_that("HDX HAPI does not expose row limit semantics", {
  expect_error(
    get_hdx_hapi("availability", app_identifier = "dummy", limit = 1),
    "limit and offset are managed internally"
  )
})

test_that("HDX HAPI availability for Kenya returns expected columns", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_availability(
    iso3 = "KEN"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "KEN", "Kenya")
  expect_true(all(c("admin1_code", "admin2_code", "admin_level", "category", "subcategory") %in% names(result)))
})

test_that("HDX HAPI availability for Ethiopia returns expected country rows", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_availability(
    iso3 = "ETH"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "ETH", "Ethiopia")
})

test_that("HDX HAPI WFP markets for Kenya and Ethiopia include coordinates", {
  skip_hdx_hapi()

  kenya <- get_hdx_hapi_wfp_markets(iso3 = "KEN")
  ethiopia <- get_hdx_hapi_wfp_markets(iso3 = "ETH")

  expect_gt(nrow(kenya), 0)
  expect_gt(nrow(ethiopia), 0)
  expect_hdx_country(kenya, "KEN", "Kenya")
  expect_hdx_country(ethiopia, "ETH", "Ethiopia")

  result <- dplyr::bind_rows(kenya, ethiopia)
  expect_true(all(c("lat", "lon") %in% names(result)))
  expect_true(any(!is.na(result$lat)))
  expect_true(any(!is.na(result$lon)))
  expect_true(all(result$lat >= -90 & result$lat <= 90, na.rm = TRUE))
  expect_true(all(result$lon >= -180 & result$lon <= 180, na.rm = TRUE))
})

test_that("HDX HAPI WFP commodities include core metadata", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_wfp_commodities()

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(c("code", "category", "name", "source") %in% names(result)))
  expect_true(all(result$source == "HDX HAPI"))
  expect_true(any(grepl("maize", result$name, ignore.case = TRUE)))
})

test_that("HDX HAPI WFP prices for Kenya include price and reference period fields", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_wfp_prices(
    iso3 = "KEN",
    start_date = "2024-01-01",
    end_date = "2024-02-01"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "KEN", "Kenya")
  expect_true(all(
    c(
      "price",
      "market_code",
      "market_name",
      "commodity_code",
      "commodity_name",
      "reference_period_start",
      "reference_period_end",
      "Year"
    ) %in% names(result)
  ))
  expect_hdx_reference_year(result)
  expect_hdx_reference_overlap(result, "2024-01-01", "2024-02-01")
})

test_that("HDX HAPI WFP prices for Ethiopia respect country and date filters", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_wfp_prices(
    iso3 = "ETH",
    start_date = "2024-01-01",
    end_date = "2024-02-01"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "ETH", "Ethiopia")
  expect_true(all(c("market_code", "market_name", "commodity_code", "commodity_name", "price") %in% names(result)))
  expect_true(all(result$price >= 0, na.rm = TRUE))
  expect_hdx_reference_year(result)
  expect_hdx_reference_overlap(result, "2024-01-01", "2024-02-01")
})

test_that("HDX HAPI food security for Somalia returns IPC data", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_food_security(
    iso3 = "SOM",
    start_date = "2024-01-01",
    end_date = "2024-12-31"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "SOM", "Somalia")
  expect_true(all(c("ipc_phase", "ipc_type", "population_in_phase", "population_fraction_in_phase") %in% names(result)))
  expect_true(all(result$population_in_phase >= 0, na.rm = TRUE))
  expect_hdx_reference_year(result)
  expect_hdx_reference_overlap(result, "2024-01-01", "2024-12-31")
})

test_that("HDX HAPI poverty for Kenya returns poverty indicators", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_poverty(iso3 = "KEN")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "KEN", "Kenya")
  expect_true(all(c("mpi", "headcount_ratio", "intensity_of_deprivation") %in% names(result)))
  expect_true(all(result$mpi >= 0, na.rm = TRUE))
  expect_hdx_reference_year(result)
})

test_that("HDX HAPI population for Ethiopia returns population by gender", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_population(
    iso3 = "ETH",
    mrv = 1
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_hdx_country(result, "ETH", "Ethiopia")
  expect_true(all(c("gender", "age_range", "population") %in% names(result)))
  expect_true(all(result$population >= 0, na.rm = TRUE))
  expect_true(all(c("all", "f", "m") %in% unique(result$gender)))
  expect_lte(length(unique(stats::na.omit(result$Year))), 1)
  expect_hdx_reference_year(result)
})

test_that("HDX HAPI wrappers return stable empty schemas for countries with no data", {
  skip_hdx_hapi()

  availability <- get_hdx_hapi_availability(iso3 = "XXX")
  markets <- get_hdx_hapi_wfp_markets(iso3 = "XXX")
  prices <- get_hdx_hapi_wfp_prices(
    iso3 = "XXX",
    start_date = "2024-01-01",
    end_date = "2024-02-01"
  )
  food_security <- get_hdx_hapi_food_security(
    iso3 = "XXX",
    start_date = "2024-01-01",
    end_date = "2024-02-01"
  )
  poverty <- get_hdx_hapi_poverty(iso3 = "XXX")
  population <- get_hdx_hapi_population(iso3 = "XXX")
  markets_sf <- get_hdx_hapi_wfp_markets(iso3 = "XXX", as_sf = TRUE)

  expect_equal(nrow(availability), 0)
  expect_equal(nrow(markets), 0)
  expect_equal(nrow(prices), 0)
  expect_equal(nrow(food_security), 0)
  expect_equal(nrow(poverty), 0)
  expect_equal(nrow(population), 0)
  expect_s3_class(markets_sf, "sf")
  expect_equal(nrow(markets_sf), 0)

  expect_true(all(c("location_code", "category", "subcategory", "iso3", "country", "source") %in% names(availability)))
  expect_true(all(c("code", "name", "lat", "lon", "iso3", "country", "source") %in% names(markets)))
  expect_true(all(c("market_code", "commodity_code", "price", "reference_period_start", "Year", "iso3", "country", "source") %in% names(prices)))
  expect_true(all(c("ipc_phase", "ipc_type", "population_in_phase", "Year", "iso3", "country", "source") %in% names(food_security)))
  expect_true(all(c("mpi", "headcount_ratio", "Year", "iso3", "country", "source") %in% names(poverty)))
  expect_true(all(c("gender", "age_range", "population", "Year", "iso3", "country", "source") %in% names(population)))
})

test_that("HDX HAPI pagination fetches beyond one page", {
  skip_hdx_hapi()

  result <- get_hdx_hapi(
    "availability",
    location_code = "KEN",
    category = "affected-people",
    subcategory = "idps",
    page_size = 25
  )

  expect_gt(nrow(result), 25)
})

test_that("HDX HAPI mrv keeps the most recent years, not rows", {
  skip_hdx_hapi()

  result <- get_hdx_hapi_wfp_prices(
    iso3 = "KEN",
    start_date = "2023-01-01",
    end_date = "2025-12-31",
    mrv = 1
  )

  expect_gt(nrow(result), 0)
  expect_lte(length(unique(stats::na.omit(result$Year))), 1)
})
