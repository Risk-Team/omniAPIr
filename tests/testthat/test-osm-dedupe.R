test_that("OSM de-duplication keeps distinct way-based multipolygons", {
    x <- data.frame(
        osm_id = c("1", "1", NA, NA, NA, NA),
        osm_way_id = c(NA, NA, "10", "11", "11", NA)
    )
    expect_equal(
        dedupe_osm_features(x),
        x[c(1, 3, 4, 6), , drop = FALSE]
    )
})
