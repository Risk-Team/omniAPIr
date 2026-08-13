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

test_that("OSM SQL only uses layer-supported columns", {
    tag_sets <- list(
        amenity = c("hospital", "school"),
        highway = "primary",
        shop = "supermarket"
    )

    points_clause <- omniAPIr:::build_osm_where_clause(
        tag_sets,
        column_tags = omniAPIr:::osm_default_layer_tags("points")
    )
    lines_clause <- omniAPIr:::build_osm_where_clause(
        tag_sets,
        column_tags = omniAPIr:::osm_default_layer_tags("lines")
    )
    promoted_points_clause <- omniAPIr:::build_osm_where_clause(
        tag_sets,
        column_tags = union(omniAPIr:::osm_default_layer_tags("points"), names(tag_sets))
    )

    expect_no_match(points_clause, '"amenity" (=|IN)')
    expect_no_match(points_clause, '"shop" (=|IN)')
    expect_match(points_clause, 'other_tags LIKE .*"amenity"=>"hospital"')
    expect_match(points_clause, 'other_tags LIKE .*"shop"=>"supermarket"')
    expect_match(lines_clause, '"highway" =')
    expect_match(lines_clause, 'other_tags LIKE .*"amenity"=>"school"')
    expect_match(promoted_points_clause, '"amenity" IN')
    expect_match(promoted_points_clause, '"shop" =')
})

test_that("OSM feature filtering matches tags stored in other_tags", {
    features <- sf::st_sf(
        osm_id = c("1", "2", "3"),
        other_tags = c(
            '"shop"=>"supermarket"',
            '"amenity"=>"school","name:en"=>"Example School"',
            '"amenity"=>"clinic"'
        ),
        geometry = sf::st_sfc(
            sf::st_point(c(36.8, -1.3)),
            sf::st_point(c(36.81, -1.31)),
            sf::st_point(c(36.82, -1.32)),
            crs = 4326
        )
    )

    result <- omniAPIr:::filter_osm_sf_by_tags(
        features,
        list(amenity = "school"),
        feature_class = "schools",
        feature_label = "Schools"
    )

    expect_equal(result$osm_id, "2")
    expect_equal(result$feature_class, "schools")
    expect_equal(result$feature_label, "Schools")
})

test_that("OSM region boundary preparation repairs invalid polygons", {
    bowtie <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(
            sf::st_polygon(list(rbind(
                c(0, 0),
                c(1, 1),
                c(1, 0),
                c(0, 1),
                c(0, 0)
            ))),
            crs = 4326
        )
    )

    fixed <- omniAPIr:::prepare_osm_region_boundary(bowtie)

    expect_s3_class(fixed, "sf")
    expect_true(all(sf::st_is_valid(fixed)))
    expect_equal(sf::st_crs(fixed)$epsg, 4326)
    expect_true(all(sf::st_dimension(fixed) == 2))
})

test_that("OSM match input uses explicit match_place when supplied", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )

    expect_equal(omniAPIr:::osm_match_input(region, "Kenya"), "Kenya")
    expect_error(
        omniAPIr:::osm_match_input(region, ""),
        "match_place must be a non-empty length-one character value"
    )
})

test_that("OSM cache key separates explicit match places", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )
    tag_sets <- list(amenity = "school")

    point_cache <- omniAPIr:::osm_cache_file(
        region_sf = region,
        provider = "geofabrik",
        match_level = 2,
        layers = "points",
        tag_sets = tag_sets,
        cache_dir = tempdir()
    )
    place_cache <- omniAPIr:::osm_cache_file(
        region_sf = region,
        provider = "geofabrik",
        match_level = 2,
        layers = "points",
        tag_sets = tag_sets,
        cache_dir = tempdir(),
        match_place = "Kenya"
    )

    expect_false(identical(point_cache, place_cache))
})

test_that("OSM extract coverage validation rejects partial provider zones", {
    india_bbox <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = 68.1,
        ymin = 6.7,
        xmax = 97.2,
        ymax = 33.2
    ), crs = 4326))
    india_region <- sf::st_sf(id = 1, geometry = india_bbox)

    western_match <- list(
        url = "https://download.geofabrik.de/asia/india/western-zone-latest.osm.pbf",
        file_size = 216006656
    )
    india_match <- list(
        url = "https://download.geofabrik.de/asia/india-latest.osm.pbf",
        file_size = 1500000000
    )

    expect_error(
        omniAPIr:::validate_osm_extract_coverage(
            region_sf = india_region,
            match_info = western_match,
            provider = "geofabrik",
            coverage_check = "error",
            min_coverage = 0.98
        ),
        "covers .* of the requested region"
    )
    expect_true(omniAPIr:::validate_osm_extract_coverage(
        region_sf = india_region,
        match_info = india_match,
        provider = "geofabrik",
        coverage_check = "error",
        min_coverage = 0.50
    ))
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

test_that("OSM feature class API does not cache failed layer queries", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )
    cache_dir <- tempfile("osm-cache-")
    dir.create(cache_dir)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
    query_count <- 0

    local_mocked_bindings(
        get_osm_features = function(region_sf, tag_sets, ...) {
            query_count <<- query_count + 1
            result <- list(
                pts = omniAPIr:::empty_osm_sf(),
                lines = omniAPIr:::empty_osm_sf(),
                poly = omniAPIr:::empty_osm_sf(),
                multipoly = omniAPIr:::empty_osm_sf()
            )
            attr(result, "osm_failed_layers") <- "points"
            attr(result, "osm_query_errors") <- list(points = "test failure")
            result
        }
    )

    get_osm_feature_class(region, "schools", cache_dir = cache_dir, layers = "points")
    get_osm_feature_class(region, "schools", cache_dir = cache_dir, layers = "points")

    expect_equal(query_count, 2)
    expect_length(list.files(cache_dir, pattern = "[.]rds$"), 0)
})

test_that("OSM feature class API caches successful empty queries", {
    region <- sf::st_sf(
        id = 1,
        geometry = sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326)
    )
    cache_dir <- tempfile("osm-cache-")
    dir.create(cache_dir)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
    query_count <- 0

    local_mocked_bindings(
        get_osm_features = function(region_sf, tag_sets, ...) {
            query_count <<- query_count + 1
            result <- list(
                pts = omniAPIr:::empty_osm_sf(),
                lines = omniAPIr:::empty_osm_sf(),
                poly = omniAPIr:::empty_osm_sf(),
                multipoly = omniAPIr:::empty_osm_sf()
            )
            attr(result, "osm_failed_layers") <- character()
            attr(result, "osm_query_errors") <- list()
            result
        }
    )

    get_osm_feature_class(region, "schools", cache_dir = cache_dir, layers = "points")
    get_osm_feature_class(region, "schools", cache_dir = cache_dir, layers = "points")

    expect_equal(query_count, 1)
    expect_length(list.files(cache_dir, pattern = "[.]rds$"), 1)
})

test_that("OSM API metadata points users to feature classes and osmextract", {
    info <- get_api_info("OpenStreetMap")

    expect_equal(info$function_name, "get_osm_feature_class")
    expect_equal(info$base_url, "https://download.geofabrik.de/")
    expect_equal(info$r_packages, "osmextract")
    expect_match(info$description, "feature classes")
})
