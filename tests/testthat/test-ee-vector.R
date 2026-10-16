test_that("download_and_process_ee_vector rejects new arguments before starting Python", {
    expect_error(
        download_and_process_ee_vector(
            asset_id = "x",
            output_filename = "x.geojson",
            region = NULL,
            region_sf = NULL,
            column_empty_return = "CSI",
            value_empty_return = NA,
            conda_env = "gee_llm",
            geometry_type = "point"
        ),
        "should be one of"
    )
    expect_error(
        download_and_process_ee_vector(
            asset_id = "x",
            output_filename = "x.geojson",
            region = NULL,
            region_sf = NULL,
            column_empty_return = "CSI",
            value_empty_return = NA,
            conda_env = "gee_llm",
            properties = c("DOF", "")
        ),
        "properties"
    )
    expect_error(
        download_and_process_ee_vector(
            asset_id = "x",
            output_filename = "x.geojson",
            region = NULL,
            region_sf = NULL,
            column_empty_return = "CSI",
            value_empty_return = NA,
            conda_env = "gee_llm",
            max_features_per_request = 5001
        ),
        "max_features_per_request"
    )
})

test_that("large Earth Engine collections are split below the response cap", {
    pages <- omniAPIr:::.ee_vector_page_specs(
        count = 10001,
        max_features_per_request = 4000
    )

    expect_equal(pages$offset, c(0L, 4000L, 8000L))
    expect_equal(pages$size, c(4000L, 4000L, 2001L))
    expect_true(all(pages$size <= 4000L))
})

test_that("line results drop points left by an intersection", {
    features <- sf::st_sf(
        id = c(1L, 2L, 3L),
        geometry = sf::st_sfc(
            sf::st_linestring(rbind(c(0, 0), c(1, 1))),
            sf::st_point(c(2, 2)),
            sf::st_geometrycollection(list(
                sf::st_linestring(rbind(c(0, 0), c(1, 0))),
                sf::st_point(c(1, 0))
            )),
            crs = 4326
        )
    )

    kept <- omniAPIr:::.keep_linear_features(features)
    expect_equal(kept$id, c(1L, 3L))
    expect_true(all(sf::st_geometry_type(kept) == "LINESTRING"))
    expect_equal(nrow(omniAPIr:::.keep_linear_features(features[0, ])), 0)
})

test_that("Earth Engine filter and property selection run in that order", {
    mock_collection <- function() {
        collection <- new.env(parent = emptyenv())
        collection$calls <- character()
        collection$filter <- function(query) {
            collection$calls <- c(collection$calls, paste0("filter:", query))
            collection
        }
        collection$select <- function(props) {
            collection$calls <- c(
                collection$calls,
                paste0("select:", paste(unlist(props), collapse = ","))
            )
            collection
        }
        collection
    }

    filtered <- omniAPIr:::.prepare_ee_feature_collection(
        mock_collection(),
        ee_filter = "RIV",
        properties = c("DOF", "CSI")
    )
    expect_equal(filtered$calls, c("filter:RIV", "select:DOF,CSI"))

    called <- omniAPIr:::.prepare_ee_feature_collection(
        mock_collection(),
        ee_filter = function(features) {
            features$calls <- c(features$calls, "fn")
            features
        }
    )
    expect_equal(called$calls, "fn")
})
