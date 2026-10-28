unesco_test_response <- function(records) {
    httr2::response(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
            list(total_count = length(records), results = unname(records)),
            auto_unbox = TRUE, null = "null", digits = NA
        ))
    )
}

test_that("WHC uses UNESCO's representative point for nested multipoint geometry", {
    record <- list(
        name_en = "Port Royal", iso_codes = "JM", category = "Cultural",
        geo_point_2d = list(lon = -76.8433777778, lat = 17.9386138889),
        coordinates = list(type = "Feature", geometry = list(
            type = "MultiPoint", coordinates = list(c(-76.9, 17.9), c(-76.8, 18.0))
        ))
    )
    local_mocked_bindings(req_perform = function(...) unesco_test_response(list(record)), .package = "httr2")
    result <- get_unesco_data("JM", "whc")
    expect_equal(nrow(result), 1)
    expect_identical(result$name_en, "Port Royal")
    expect_type(result$latitude, "double")
    expect_type(result$longitude, "double")
    expect_equal(result$latitude, 17.9386138889)
    expect_equal(result$longitude, -76.8433777778)
    expect_false("coordinates" %in% names(result))
})

test_that("WHC accepts legacy flat coordinates when no representative field is available", {
    record <- list(name_en = "Legacy site", coordinates = list(lat = 12.5, lon = -16.3))
    local_mocked_bindings(req_perform = function(...) unesco_test_response(list(record)), .package = "httr2")
    result <- get_unesco_data("SN", "whc")
    expect_equal(result$latitude, 12.5)
    expect_equal(result$longitude, -16.3)
})

test_that("WHC retains sites with unavailable locations without inventing coordinates", {
    records <- list(
        list(name_en = "Located", geo_point_2d = list(lat = 1, lon = 2)),
        list(name_en = "Missing", geo_point_2d = NULL),
        list(name_en = "Multipoint only", coordinates = list(type = "Feature",
            geometry = list(type = "MultiPoint", coordinates = list(c(10, 20), c(30, 40)))))
    )
    local_mocked_bindings(req_perform = function(...) unesco_test_response(records), .package = "httr2")
    result <- get_unesco_data("KE", "whc")
    expect_equal(nrow(result), 3)
    expect_equal(result$latitude[[1]], 1)
    expect_equal(result$longitude[[1]], 2)
    expect_true(all(is.na(result$latitude[2:3])))
    expect_true(all(is.na(result$longitude[2:3])))
})

test_that("WHC missing coordinate fields still produce numeric location columns", {
    local_mocked_bindings(req_perform = function(...) {
        unesco_test_response(list(list(name_en = "Unlocated", coordinates = NULL)))
    }, .package = "httr2")
    result <- get_unesco_data("KE", "whc")
    expect_identical(result$latitude, NA_real_)
    expect_identical(result$longitude, NA_real_)
})

test_that("WHC coordinate parsing preserves rows across pagination", {
    requests <- character()
    local_mocked_bindings(req_perform = function(req) {
        requests <<- c(requests, req$url)
        record <- list(name_en = "Site", geo_point_2d = list(lat = 1, lon = 2))
        if (length(requests) == 1) unesco_test_response(rep(list(record), 100))
        else unesco_test_response(list(record))
    }, .package = "httr2")
    result <- get_unesco_data("KE", "whc")
    expect_equal(nrow(result), 101)
    expect_match(requests[[2]], "offset=100", fixed = TRUE)
    expect_true(all(result$latitude == 1 & result$longitude == 2))
})

test_that("ICH retrieval and list-valued country fields remain unchanged", {
    record <- list(title_en = "Shared element", countries = c("SN", "GM"), inscription_year = 2020)
    local_mocked_bindings(req_perform = function(...) unesco_test_response(list(record)), .package = "httr2")
    result <- get_unesco_data("SN", "ich")
    expect_equal(result$title_en, "Shared element")
    expect_equal(result$countries, "SN; GM")
    expect_false(any(c("latitude", "longitude") %in% names(result)))
})
