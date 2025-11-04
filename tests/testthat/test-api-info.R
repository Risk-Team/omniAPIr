test_that("get_api_info returns data.frame with all APIs", {
    result <- get_api_info()

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) >= 15)
    expect_true(all(
        c(
            "function_name",
            "api_name",
            "base_url",
            "api_docs_url",
            "requires_python",
            "python_packages",
            "requires_auth",
            "description"
        ) %in%
            names(result)
    ))
})

test_that("get_api_info returns specific API when requested", {
    result <- get_api_info("ACLED")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$api_name, "ACLED")
    expect_equal(result$function_name, "get_acled_data")
    expect_true(result$requires_auth)
})

test_that("get_api_info errors on invalid API name", {
    expect_error(
        get_api_info("InvalidAPI"),
        "API 'InvalidAPI' not found"
    )
})

test_that("World Bank API is marked as requiring Python", {
    result <- get_api_info("World Bank")

    expect_true(result$requires_python)
    expect_equal(result$python_packages, "wbgapi")
})

test_that("APIs requiring authentication are correctly flagged", {
    result <- get_api_info()

    # Check specific APIs that require auth
    acled <- result[result$api_name == "ACLED", ]
    undp <- result[result$api_name == "UNDP HDR", ]
    ibat <- result[result$api_name == "IBAT", ]

    expect_true(acled$requires_auth)
    expect_true(undp$requires_auth)
    expect_true(ibat$requires_auth)
})

test_that("All API documentation URLs are present", {
    result <- get_api_info()

    expect_true(all(!is.na(result$api_docs_url)))
    expect_true(all(nchar(result$api_docs_url) > 0))
})
