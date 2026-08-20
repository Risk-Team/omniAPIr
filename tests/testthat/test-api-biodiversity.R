# Test biodiversity API functions
test_that("get_invasive_alien_species errors after exhausted occurrence retries", {
    skip_if_not_installed("rgbif")
    skip_if_not_installed("readr")

    tmp_dir <- tempfile("griis-")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines(
        c("id countryCode", "1 US"),
        file.path(tmp_dir, "distribution.txt")
    )
    writeLines(
        c("id isInvasive", "1 Invasive"),
        file.path(tmp_dir, "speciesprofile.txt")
    )
    writeLines(
        c("id\tscientificName", "1\tHomo sapiens"),
        file.path(tmp_dir, "taxon.txt")
    )

    old_wd <- setwd(tmp_dir)
    on.exit(setwd(old_wd), add = TRUE)
    utils::zip("griis.zip", c("distribution.txt", "speciesprofile.txt", "taxon.txt"))
    setwd(old_wd)
    zip_path <- file.path(tmp_dir, "griis.zip")

    local_mocked_bindings(
        dataset_export = function(...) {
            data.frame(
                title = "Global Register of Introduced and Invasive Species - United States",
                datasetKey = "fake-key",
                stringsAsFactors = FALSE
            )
        },
        dataset_endpoint = function(...) {
            data.frame(
                type = "DWC_ARCHIVE",
                url = "http://example.invalid/griis.zip",
                stringsAsFactors = FALSE
            )
        },
        occ_search = function(..., curlopts = list()) {
            stop("simulated GBIF occurrence failure", call. = FALSE)
        },
        .package = "rgbif"
    )
    local_mocked_bindings(
        download.file = function(url, destfile, ...) {
            ok <- file.copy(zip_path, destfile, overwrite = TRUE)
            if (!isTRUE(ok)) {
                stop("failed to copy fixture zip", call. = FALSE)
            }
            invisible(0L)
        },
        .package = "utils"
    )

    out_zip <- file.path(tmp_dir, "out.zip")
    expect_error(
        get_invasive_alien_species(
            iso3 = "USA",
            max_species = 1,
            geolocation = TRUE,
            output_filename = out_zip,
            verbose = FALSE,
            max_retries = 1
        ),
        "Failed to fetch occurrences"
    )
})

test_that("get_invasive_alien_species applies per-attempt curl timeout", {
    skip_if_not_installed("rgbif")
    skip_if_not_installed("readr")

    tmp_dir <- tempfile("griis-timeout-")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    writeLines(
        c("id countryCode", "1 US"),
        file.path(tmp_dir, "distribution.txt")
    )
    writeLines(
        c("id isInvasive", "1 Invasive"),
        file.path(tmp_dir, "speciesprofile.txt")
    )
    writeLines(
        c("id\tscientificName", "1\tHomo sapiens"),
        file.path(tmp_dir, "taxon.txt")
    )

    old_wd <- setwd(tmp_dir)
    on.exit(setwd(old_wd), add = TRUE)
    utils::zip("griis.zip", c("distribution.txt", "speciesprofile.txt", "taxon.txt"))
    setwd(old_wd)
    zip_path <- file.path(tmp_dir, "griis.zip")

    seen_curlopts <- NULL
    local_mocked_bindings(
        dataset_export = function(...) {
            data.frame(
                title = "Global Register of Introduced and Invasive Species - United States",
                datasetKey = "fake-key",
                stringsAsFactors = FALSE
            )
        },
        dataset_endpoint = function(...) {
            data.frame(
                type = "DWC_ARCHIVE",
                url = "http://example.invalid/griis.zip",
                stringsAsFactors = FALSE
            )
        },
        occ_search = function(..., curlopts = list()) {
            seen_curlopts <<- curlopts
            stop("simulated GBIF occurrence failure", call. = FALSE)
        },
        .package = "rgbif"
    )
    local_mocked_bindings(
        download.file = function(url, destfile, ...) {
            ok <- file.copy(zip_path, destfile, overwrite = TRUE)
            if (!isTRUE(ok)) {
                stop("failed to copy fixture zip", call. = FALSE)
            }
            invisible(0L)
        },
        .package = "utils"
    )

    out_zip <- file.path(tmp_dir, "out.zip")
    expect_error(
        get_invasive_alien_species(
            iso3 = "USA",
            max_species = 1,
            geolocation = TRUE,
            output_filename = out_zip,
            verbose = FALSE,
            max_retries = 1,
            timeout_s = 13
        ),
        "Failed to fetch occurrences"
    )
    expect_equal(seen_curlopts$timeout, 13)
})

test_that("get_invasive_alien_species works with basic parameters", {
    skip_if_not_installed("rgbif")
    skip_if_not_installed("readr")

    out_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(c(out_zip, sub("\\.zip$", "", out_zip)), recursive = TRUE), add = TRUE)

    result <- get_invasive_alien_species(
        iso3 = "KEN",
        output_filename = out_zip,
        max_species = 5,
        geolocation = FALSE,
        verbose = FALSE,
        max_retries = 1
    )

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
})

test_that("get_ibat_data validates required arguments", {
    expect_error(
        get_ibat_data(
            region_sf = NULL,
            datasets = character(),
            ibat_api_key = "key",
            ibat_token = "token",
            path = tempdir()
        ),
        "datasets"
    )
})

test_that("get_ibat_data works when credentials are available", {
    skip_if_not_installed("sf")
    skip_if_not_installed("httr2")

    api_key <- Sys.getenv("IBAT_API_KEY")
    token <- Sys.getenv("IBAT_TOKEN")
    skip_if(identical(api_key, "") || identical(token, ""), "IBAT credentials not configured")

    region_sf <- sf::st_as_sf(
        sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )
    region_sf <- sf::st_buffer(region_sf, dist = 0.1)

    path <- tempfile("ibat-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)

    result <- get_ibat_data(
        region_sf = region_sf,
        datasets = "kba",
        ibat_api_key = api_key,
        ibat_token = token,
        path = path,
        verbose = FALSE,
        max_retries = 1
    )

    expect_true(is.character(result) || inherits(result, "data.frame"))
})

test_that("get_invasive_alien_species handles different countries", {
    skip_if_not_installed("rgbif")
    skip_if_not_installed("readr")

    out_ke <- tempfile(fileext = ".zip")
    out_au <- tempfile(fileext = ".zip")
    on.exit(
        unlink(
            c(out_ke, out_au, sub("\\.zip$", "", out_ke), sub("\\.zip$", "", out_au)),
            recursive = TRUE
        ),
        add = TRUE
    )

    result_ke <- get_invasive_alien_species(
        iso3 = "KEN",
        output_filename = out_ke,
        max_species = 5,
        geolocation = FALSE,
        verbose = FALSE,
        max_retries = 1
    )

    expect_s3_class(result_ke, "data.frame")
    expect_gt(nrow(result_ke), 0)

    result_au <- get_invasive_alien_species(
        iso3 = "AUS",
        output_filename = out_au,
        max_species = 5,
        geolocation = FALSE,
        verbose = FALSE,
        max_retries = 1
    )

    expect_s3_class(result_au, "data.frame")
})
