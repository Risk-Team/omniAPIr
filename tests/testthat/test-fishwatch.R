test_that("get_fishwatch_data validates required parameters", {
    expect_error(
        get_fishwatch_data(region_source = "EEZ", region = 8349),
        "api_key parameter is required"
    )
    expect_error(
        get_fishwatch_data(region = 8349, api_key = "token"),
        "region_source parameter is required"
    )
    expect_error(
        get_fishwatch_data(region_source = "EEZ", api_key = "token"),
        "region parameter is required"
    )
})

test_that("get_fishwatch_eez_ids resolves one exact EEZ", {
    local_mocked_bindings(
        .gfwr_region_id = function(region, key) {
            tibble::tibble(
                id = 8349,
                label = "Kenya",
                iso3 = "KEN",
                GEONAME = "Kenyan Exclusive Economic Zone",
                POL_TYPE = "200NM"
            )
        }
    )

    result <- get_fishwatch_eez_ids("ken", "token")

    expect_equal(result$id, 8349)
    expect_equal(result$iso3, "KEN")
})

test_that("get_fishwatch_eez_ids returns every exact ISO3 match", {
    local_mocked_bindings(
        .gfwr_region_id = function(region, key) {
            tibble::tibble(
                id = c(8456, 8453, 9999),
                label = c("United States", "Hawaii", "Unrelated"),
                iso3 = c("USA", "USA", "USX"),
                GEONAME = c("A", "B", "C"),
                POL_TYPE = rep("200NM", 3)
            )
        }
    )

    result <- get_fishwatch_eez_ids("USA", "token")

    expect_equal(result$id, c(8453, 8456))
    expect_true(all(result$iso3 == "USA"))
})

test_that("get_fishwatch_eez_ids identifies a landlocked country", {
    local_mocked_bindings(
        .gfwr_region_id = function(region, key) {
            tibble::tibble(
                id = double(),
                label = character(),
                iso3 = character(),
                GEONAME = character(),
                POL_TYPE = character()
            )
        }
    )

    result <- get_fishwatch_eez_ids("BOL", "token")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
    expect_named(result, c("id", "label", "iso3", "GEONAME", "POL_TYPE"))
})

test_that("get_fishwatch_eez_ids propagates invalid token errors", {
    local_mocked_bindings(
        .gfwr_region_id = function(region, key) {
            stop("HTTP 401 Unauthorized")
        }
    )

    expect_error(
        get_fishwatch_eez_ids("KEN", "expired-token"),
        "HTTP 401 Unauthorized"
    )
})

test_that("get_fishwatch_data returns a valid single EEZ response", {
    local_mocked_bindings(
        .gfwr_fishing_hours = function(..., region) {
            expect_equal(region, 8349)
            tibble::tibble(
                Lat = -2.5,
                Lon = 41.5,
                `Time Range` = 2023,
                flag = "KEN",
                `Vessel IDs` = 1,
                `Apparent Fishing Hours` = 2.5
            )
        }
    )

    result <- get_fishwatch_data(
        temporal_resolution = "YEARLY",
        start_date = "2023-01-01",
        end_date = "2024-01-01",
        region_source = "EEZ",
        region = 8349,
        group_by = "FLAG",
        api_key = "token",
        verbose = FALSE
    )

    expect_equal(nrow(result), 1)
    expect_equal(result$`Apparent Fishing Hours`, 2.5)
})

test_that("get_fishwatch_data combines multiple EEZ responses", {
    local_mocked_bindings(
        .gfwr_fishing_hours = function(..., region) {
            tibble::tibble(
                Lat = as.double(region),
                Lon = 1,
                `Time Range` = 2023,
                flag = "KEN",
                `Vessel IDs` = 1,
                `Apparent Fishing Hours` = 2
            )
        }
    )

    result <- get_fishwatch_data(
        temporal_resolution = "YEARLY",
        start_date = "2023-01-01",
        end_date = "2024-01-01",
        region_source = "EEZ",
        region = c(10, 20),
        group_by = "FLAG",
        api_key = "token",
        verbose = FALSE
    )

    expect_equal(result$Lat, c(10, 20))
})

test_that("get_fishwatch_data normalizes mixed response column types", {
    local_mocked_bindings(
        .gfwr_fishing_hours = function(..., region) {
            if (region == 10) {
                tibble::tibble(
                    Lat = 1.5,
                    Lon = 2.5,
                    `Time Range` = 2023,
                    flag = "COL",
                    `Vessel IDs` = 1,
                    `Apparent Fishing Hours` = 2
                )
            } else {
                tibble::tibble(
                    Lat = "3.5",
                    Lon = "4.5",
                    `Time Range` = "2023",
                    flag = "COL",
                    `Vessel IDs` = "2",
                    `Apparent Fishing Hours` = "5.25"
                )
            }
        }
    )

    result <- get_fishwatch_data(
        temporal_resolution = "YEARLY",
        start_date = "2023-01-01",
        end_date = "2024-01-01",
        region_source = "EEZ",
        region = c(10, 20),
        group_by = "FLAG",
        api_key = "token",
        verbose = FALSE
    )

    expect_type(result$Lat, "double")
    expect_type(result$Lon, "double")
    expect_type(result$`Time Range`, "double")
    expect_type(result$`Vessel IDs`, "double")
    expect_type(result$`Apparent Fishing Hours`, "double")
    expect_equal(result$Lat, c(1.5, 3.5))
    expect_equal(result$`Apparent Fishing Hours`, c(2, 5.25))
})

test_that("get_fishwatch_data rejects partial yearly results", {
    local_mocked_bindings(
        .gfwr_fishing_hours = function(..., region) {
            if (region == 20) stop("yearly request failed")
            tibble::tibble(
                Lat = 1,
                Lon = 2,
                `Time Range` = 2023,
                `Vessel IDs` = 1,
                `Apparent Fishing Hours` = 2
            )
        }
    )

    expect_error(
        get_fishwatch_data(
            temporal_resolution = "YEARLY",
            start_date = "2023-01-01",
            end_date = "2024-01-01",
            region_source = "EEZ",
            region = c(10, 20),
            api_key = "token",
            verbose = FALSE,
            max_retries = 1
        ),
        "region 20.*yearly request failed"
    )
})

test_that("get_fishwatch_data returns an empty tibble for zero activity", {
    local_mocked_bindings(
        .gfwr_fishing_hours = function(...) NULL
    )

    result <- get_fishwatch_data(
        temporal_resolution = "YEARLY",
        start_date = "2023-01-01",
        end_date = "2024-01-01",
        region_source = "EEZ",
        region = 8349,
        group_by = "FLAG",
        api_key = "token",
        verbose = FALSE
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_named(
        result,
        c(
            "Lat", "Lon", "Time Range", "flag", "Vessel IDs",
            "Apparent Fishing Hours"
        )
    )
})
