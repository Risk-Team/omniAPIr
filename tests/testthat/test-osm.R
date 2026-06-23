test_that("OSM tag catalog contains expected feature classes", {
    catalog <- get_osm_tag_catalog()
    expected_classes <- c(
        "food_retail",
        "markets",
        "health_facilities",
        "schools",
        "water_points",
        "slaughterhouses",
        "veterinary_services",
        "storage",
        "transport_nodes",
        "border_crossings",
        "major_roads"
    )

    expect_named(catalog, expected_classes)
    expect_equal(list_osm_feature_classes(), expected_classes)
    expect_true("school" %in% catalog$schools$amenity)
    expect_true("hospital" %in% catalog$health_facilities$amenity)
})

test_that("OSM feature class validation rejects unknown classes", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )

    expect_error(
        get_osm_feature_class(region, "not_a_feature_class"),
        "Unknown OSM feature class.*not_a_feature_class.*Available feature classes"
    )
})

test_that("OSM tag set combining preserves repeated keys", {
    catalog <- get_osm_tag_catalog()
    combined <- omniAPIr:::combine_osm_tag_sets(
        catalog[c("health_facilities", "schools")]
    )

    expect_true("amenity" %in% names(combined))
    expect_true("hospital" %in% combined$amenity)
    expect_true("school" %in% combined$amenity)
    expect_equal(length(combined$amenity), length(unique(combined$amenity)))
})

test_that("empty OSM feature filtering returns empty sf objects", {
    empty_features <- list(
        pts = omniAPIr:::empty_osm_sf(),
        lines = omniAPIr:::empty_osm_sf(),
        poly = omniAPIr:::empty_osm_sf(),
        multipoly = omniAPIr:::empty_osm_sf()
    )

    result <- omniAPIr:::filter_osm_features_by_tags(
        empty_features,
        list(amenity = "school"),
        feature_class = "schools",
        feature_label = "Schools"
    )

    expect_s3_class(result$pts, "sf")
    expect_equal(nrow(result$pts), 0)
    expect_equal(sf::st_crs(result$pts)$epsg, 4326)
    expect_true("feature_class" %in% names(result$pts))
    expect_false(is.null(result$pts))
})

test_that("OSM feature class API splits one combined query by class", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )
    query_count <- 0
    captured_tag_sets <- NULL

    mocked_points <- sf::st_sf(
        osm_id = c("1", "2", "3"),
        amenity = c("hospital", "school", NA),
        shop = c(NA, NA, "supermarket"),
        geometry = sf::st_sfc(
            sf::st_point(c(36.8, -1.3)),
            sf::st_point(c(36.81, -1.31)),
            sf::st_point(c(36.82, -1.32)),
            crs = 4326
        )
    )

    local_mocked_bindings(
        get_osm_features = function(region_sf, tag_sets, ...) {
            query_count <<- query_count + 1
            captured_tag_sets <<- tag_sets
            list(
                pts = mocked_points,
                lines = omniAPIr:::empty_osm_sf(),
                poly = omniAPIr:::empty_osm_sf(),
                multipoly = omniAPIr:::empty_osm_sf()
            )
        }
    )

    result <- get_osm_feature_class(
        region,
        c("health_facilities", "schools"),
        layers = "points",
        verbose = FALSE
    )

    expect_equal(query_count, 1)
    expect_true("hospital" %in% captured_tag_sets$amenity)
    expect_true("school" %in% captured_tag_sets$amenity)
    expect_named(result, c("health_facilities", "schools"))
    expect_equal(result$health_facilities$pts$osm_id, "1")
    expect_equal(result$schools$pts$osm_id, "2")
    expect_equal(result$health_facilities$pts$feature_class, "health_facilities")
    expect_equal(result$schools$pts$feature_label, "Schools")
})

test_that("OSM API metadata points users to feature classes and osmextract", {
    info <- get_api_info("OpenStreetMap")

    expect_equal(info$function_name, "get_osm_feature_class")
    expect_equal(info$base_url, "https://download.geofabrik.de/")
    expect_equal(info$r_packages, "osmextract")
    expect_match(info$description, "feature classes")
})
