# Live GEMStat checks. Counts come from the service itself so a new
# station-year does not fail the test, while a dropped page still does.

gemstat_service_count <- function(iso3, parameter) {
    services <- c(
        "O2-Dis" = "GEMS_INDICATOR_STATIONS_DO",
        "TN" = "GEMS_INDICATOR_STATIONS_NITROGEN",
        "TP" = "GEMS_INDICATOR_STATIONS_PHOSPHORUS"
    )
    url <- sprintf(
        "https://geoportal.bafg.de/arcgis3/rest/services/GEMSTAT/%s/MapServer/0/query",
        services[[parameter]]
    )
    resp <- httr2::request(url) |>
        httr2::req_user_agent("omniAPIr") |>
        httr2::req_timeout(60) |>
        httr2::req_url_query(
            where = sprintf(
                "COUNTRY_CODE='%s' AND PARAMETER_CODE='%s'",
                iso3,
                parameter
            ),
            returnCountOnly = "true",
            f = "json"
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    resp$count
}

expect_gemstat_points <- function(result, iso3) {
    expect_s3_class(result, "sf")
    expect_equal(sf::st_crs(result)$epsg, 4326)
    if (nrow(result) == 0) {
        return(invisible(result))
    }
    expect_true(all(sf::st_geometry_type(result) == "POINT"))
    expect_true(all(result$COUNTRY_CODE == iso3))
    expect_type(result$SAMPLE_YEAR, "integer")
    expect_type(result$MEAN_VALUE, "double")
    coords <- sf::st_coordinates(result)
    expect_true(all(abs(coords[, "X"]) <= 180))
    expect_true(all(abs(coords[, "Y"]) <= 90))
    invisible(result)
}

test_that("get_gemstat_data rejects invalid arguments", {
    expect_error(get_gemstat_data("Kenya"), "ISO3")
    expect_error(get_gemstat_data(c("KEN", "TZA")), "ISO3")
    expect_error(get_gemstat_data("KEN", parameters = "BOD"), "Unknown: BOD")
    expect_error(get_gemstat_data("KEN", page_size = 5001), "page_size")
})

test_that("get_gemstat_data returns an empty table for a country with no stations", {
    skip_if_not_installed("httr2")
    skip_if_not_installed("sf")

    result <- expect_no_warning(get_gemstat_data("ETH"))
    expect_gemstat_points(result, "ETH")
    expect_equal(nrow(result), 0)
})

test_that("get_gemstat_data pages match a single request and keep the latest year", {
    skip_if_not_installed("httr2")
    skip_if_not_installed("sf")

    full <- get_gemstat_data("KEN", latest_only = FALSE)
    paged <- get_gemstat_data("KEN", latest_only = FALSE, page_size = 5)
    expect_gemstat_points(full, "KEN")
    expect_equal(nrow(full), nrow(paged))
    expect_equal(
        gemstat_service_count("KEN", "O2-Dis") + gemstat_service_count("KEN", "TP"),
        nrow(full)
    )

    key <- function(x) {
        paste(x$GEMS_STATION_CODE, x$PARAMETER_CODE, x$SAMPLE_YEAR, x$MEAN_VALUE)
    }
    expect_setequal(key(full), key(paged))

    latest <- get_gemstat_data("ken", latest_only = TRUE)
    expect_gemstat_points(latest, "KEN")
    expect_false(any(duplicated(paste(latest$GEMS_STATION_CODE, latest$PARAMETER_CODE))))

    station_id <- paste(full$GEMS_STATION_CODE, full$PARAMETER_CODE)
    max_year <- tapply(full$SAMPLE_YEAR, station_id, max)
    kept_year <- tapply(
        latest$SAMPLE_YEAR,
        paste(latest$GEMS_STATION_CODE, latest$PARAMETER_CODE),
        max
    )
    expect_equal(as.integer(sort(max_year)), as.integer(sort(kept_year)))

    thika <- full[full$GEMS_STATION_CODE == "KEN00005" & full$SAMPLE_YEAR == 1987, ]
    expect_equal(nrow(thika), 1)
    expect_equal(thika$MEAN_VALUE, 7.98)
    expect_equal(thika$UNIT, "mg/l")
    expect_equal(thika$WATER_TYPE, "River station")
    xy <- sf::st_coordinates(thika)
    expect_equal(unname(xy[1, "X"]), 37.26, tolerance = 0.02)
    expect_equal(unname(xy[1, "Y"]), -0.80, tolerance = 0.02)
})

test_that("get_gemstat_data keeps groundwater oxygen separate from nutrients", {
    skip_if_not_installed("httr2")
    skip_if_not_installed("sf")

    result <- get_gemstat_data("TZA", latest_only = FALSE)
    expect_gemstat_points(result, "TZA")
    expect_equal(
        nrow(result),
        gemstat_service_count("TZA", "O2-Dis") +
            gemstat_service_count("TZA", "TN") +
            gemstat_service_count("TZA", "TP")
    )
    oxygen <- result[result$PARAMETER_CODE == "O2-Dis", ]
    nutrients <- result[result$PARAMETER_CODE %in% c("TN", "TP"), ]
    expect_true("Groundwater station" %in% oxygen$WATER_TYPE)
    expect_false(any(nutrients$WATER_TYPE == "Groundwater station"))
    expect_equal(unique(oxygen$UNIT), "mg/l")
    expect_true(all(nutrients$UNIT == "µg/l"))
})

test_that("get_gemstat_data reads past the 5000-row service page", {
    skip_if_not_installed("httr2")
    skip_if_not_installed("sf")

    result <- get_gemstat_data(
        "IND",
        parameters = "O2-Dis",
        latest_only = FALSE
    )
    expect_gemstat_points(result, "IND")
    expect_gt(nrow(result), 5000)
    expect_equal(nrow(result), gemstat_service_count("IND", "O2-Dis"))

    latest <- get_gemstat_data("IND", parameters = "O2-Dis", latest_only = TRUE)
    expect_false(any(duplicated(latest$GEMS_STATION_CODE)))
    expect_lt(nrow(latest), nrow(result))
})
