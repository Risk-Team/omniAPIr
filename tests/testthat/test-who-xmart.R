who_test_response <- function(records, next_link = NULL) {
    payload <- list(value = unname(records))
    if (!is.null(next_link)) payload[["@odata.nextLink"]] <- next_link
    httr2::response(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
    )
}

who_test_record <- function(year = "2023", sex = "BTSX", country = "JAM", value = 72) {
    list(INDICATOR_CODE = "WHOSIS_000001", PERIOD_CODE = year,
         PERIOD_TYPE = "YEAR", LOCATION_CODE = country,
         LOCATION_TYPE = "COUNTRY", DIMENSION_1 = "SEX",
         DIMENSION_MEMBER_1 = sex, DIMENSION_2 = NULL,
         DIMENSION_MEMBER_2 = NULL, VALUE_NUMERIC = value,
         VALUE_STRING = "72 [70-74]", VALUE_LOW = 70, VALUE_HIGH = 74,
         LAST_UPDATED = "2026-10-01T18:04:56+02:00", Sys_PK = 1)
}

test_that("WHO xMart queries and native observations retain their meaning", {
    seen <- NULL
    local_mocked_bindings(req_perform = function(req) {
        seen <<- req
        who_test_response(list(who_test_record()))
    }, .package = "httr2")
    result <- get_who_data("WHOSIS_000001", iso3 = "JAM", mrv = NULL, timeout_s = 9)
    expect_match(seen$url, "xmart-api-public.who.int/GHO/FACT", fixed = TRUE)
    expect_match(utils::URLdecode(seen$url), "INDICATOR_CODE eq 'WHOSIS_000001'", fixed = TRUE)
    expect_match(utils::URLdecode(seen$url), "LOCATION_CODE eq 'JAM'", fixed = TRUE)
    expect_match(utils::URLdecode(seen$url), "PERIOD_TYPE eq 'YEAR'", fixed = TRUE)
    expect_equal(seen$options$timeout_ms, 9000)
    expect_identical(result$Year, 2023L)
    expect_equal(result$Value, 72)
    expect_equal(result$isocode, "JAM")
    expect_equal(result$indicator, "WHOSIS_000001")
    expect_equal(result$DIMENSION_MEMBER_1, "BTSX")
    expect_equal(result$VALUE_STRING, "72 [70-74]")
    expect_equal(result$VALUE_LOW, 70)
    expect_true(is.na(result$DIMENSION_MEMBER_2))
    expect_false(any(c("Dim1", "SpatialDim", "TimeDim") %in% names(result)))
})

test_that("WHO mrv keeps distinct years without truncating disaggregations", {
    records <- list(who_test_record("2023", "BTSX"), who_test_record("2023", "FMLE"),
                    who_test_record("2022", "BTSX"), who_test_record("2022", "MLE"),
                    who_test_record("2021", "BTSX"), who_test_record("2023", country = "AFR"))
    local_mocked_bindings(req_perform = function(...) who_test_response(records), .package = "httr2")
    result <- get_who_data("WHOSIS_000001", mrv = 2)
    expect_equal(nrow(result), 4)
    expect_setequal(result$Year, c(2023L, 2022L))
    expect_true(all(result$isocode == "JAM"))
    expect_equal(nrow(get_who_data("WHOSIS_000001", mrv = NULL, exclude_aggregates = FALSE)), 6)
})

test_that("WHO follows continuation links and never returns a failed partial result", {
    requests <- character()
    local_mocked_bindings(req_perform = function(req) {
        requests <<- c(requests, req$url)
        if (length(requests) == 1) {
            who_test_response(list(who_test_record()), "FACT?$skip=1")
        } else {
            who_test_response(list(who_test_record("2022")))
        }
    }, .package = "httr2")
    expect_equal(nrow(get_who_data("WHOSIS_000001", mrv = NULL)), 2)
    expect_match(utils::URLdecode(requests[[2]]), "/GHO/FACT?$skip=1", fixed = TRUE)
})

test_that("WHO fetches beyond a full page even without a continuation link", {
    calls <- 0L
    urls <- character()
    local_mocked_bindings(req_perform = function(req) {
        calls <<- calls + 1L
        urls <<- c(urls, req$url)
        if (calls == 1L) who_test_response(rep(list(who_test_record()), 10000))
        else who_test_response(list(who_test_record("2022")))
    }, .package = "httr2")
    result <- get_who_data("WHOSIS_000001", mrv = NULL)
    expect_equal(nrow(result), 10001)
    expect_match(utils::URLdecode(urls[[2]]), "$skip=10000", fixed = TRUE)
    expect_match(utils::URLdecode(urls[[2]]), "$orderby=Sys_PK", fixed = TRUE)
})

test_that("WHO errors on failed later pages and malformed responses", {
    calls <- 0L
    local_mocked_bindings(req_perform = function(req) {
        calls <<- calls + 1L
        if (calls == 1L) who_test_response(list(who_test_record()), "FACT?$skip=1")
        else stop("simulated page failure")
    }, .package = "httr2")
    expect_error(get_who_data("WHOSIS_000001", max_retries = 1),
                 "Failed to fetch WHO data for indicator WHOSIS_000001.*page failure")
})

test_that("WHO rejects a response without an observations array", {
    local_mocked_bindings(req_perform = function(...) {
        httr2::response(headers = list(`content-type` = "application/json"),
                        body = charToRaw('{"message":"not an observations response"}'))
    }, .package = "httr2")
    expect_error(get_who_data("WHOSIS_000001", max_retries = 1), "missing its value array")
})

test_that("WHO accepts an empty observations array", {
    local_mocked_bindings(req_perform = function(...) who_test_response(list()), .package = "httr2")
    expect_equal(nrow(get_who_data("WHOSIS_000001")), 0)
})

test_that("WHO indicator discovery uses native xMart metadata and search", {
    seen <- NULL
    local_mocked_bindings(req_perform = function(req) {
        seen <<- req$url
        who_test_response(list(
            list(INDICATOR_CODE = "WHOSIS_000001", INDICATOR_NAME = "Life expectancy at birth (years)"),
            list(INDICATOR_CODE = "ANAEMIA", INDICATOR_NAME = "Anaemia prevalence (%)")
        ))
    }, .package = "httr2")
    result <- list_un_indicators("WHO", search = "life expectancy")
    expect_match(seen, "/GHO/REF_INDICATOR", fixed = TRUE)
    expect_equal(result$code, "WHOSIS_000001")
    expect_equal(result$name, "Life expectancy at birth (years)")
})
