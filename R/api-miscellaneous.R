#' Get Giga Schools Connectivity Data
#'
#' @description
#' Retrieves school connectivity data from the Giga Initiative API with automatic
#' pagination support. Requires authentication token.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character. ISO3 country code (required).
#' @param giga_token Character. Giga API authentication token (required).
#' @param page_size Integer. Number of records per page. Default is 100.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return An sf object containing school locations with connectivity information,
#'   or NULL if no data found.
#'
#' @details
#' API Documentation: \url{https://gigamaps.org/}
#'
#' **Authentication Required:** Obtain a token from Giga Initiative.
#'
#' The function automatically handles pagination and converts results to an
#' sf spatial object with WGS84 coordinates.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' giga_data <- get_giga_schools_data(
#'   iso3 = "KEN",
#'   giga_token = "your_token"
#' )
#' }
get_giga_schools_data <- function(
  iso3,
  giga_token,
  page_size = 100,
  verbose = FALSE,
  max_retries = 3
) {
  if (verbose) {
    message(sprintf("Fetching Giga schools data for country: %s", iso3))
  }

  # Initialize variables for pagination
  all_schools_data <- list()
  page <- 1
  total_schools <- 0

  # Iterate through all pages
  repeat {
    # Construct API URL for current page
    api_url <- sprintf(
      "https://uni-ooi-giga-maps-service.azurewebsites.net/api/v1/schools_location/country/%s?page=%d&size=%d",
      iso3,
      page,
      page_size
    )

    if (verbose) {
      message(sprintf("Fetching page %d...", page))
    }

    # Make API request with retry logic
    retry_attempt <- 1
    success <- FALSE
    response <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          response <- httr2::request(api_url) %>%
            httr2::req_headers(
              "accept" = "application/json",
              "Authorization" = sprintf("Bearer %s", giga_token)
            ) %>%
            httr2::req_perform() %>%
            httr2::resp_body_json(check_type = FALSE)

          # Check if request was successful
          if (!response$success) {
            stop("API request failed for country: ", iso3, call. = FALSE)
          }

          success <- TRUE
        },
        error = function(e) {
          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt
            if (verbose) {
              message(sprintf(
                "Request failed (attempt %d/%d): %s. Retrying in %d seconds...",
                retry_attempt,
                max_retries,
                conditionMessage(e),
                wait_time
              ))
            }
            Sys.sleep(wait_time)
            retry_attempt <<- retry_attempt + 1
          } else {
            stop(
              sprintf(
                "Failed to fetch Giga schools data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    # Extract data from current page
    schools_data <- response$data

    # If no data returned, we've reached the end
    if (length(schools_data) == 0) {
      if (verbose) {
        message(sprintf(
          "No more data found at page %d. Stopping pagination.",
          page
        ))
      }
      break
    }

    # Add data from current page to collection
    all_schools_data <- c(all_schools_data, schools_data)
    total_schools <- total_schools + length(schools_data)

    if (verbose) {
      message(sprintf(
        "Retrieved %d schools from page %d (total so far: %d)",
        length(schools_data),
        page,
        total_schools
      ))
    }

    # Move to next page
    page <- page + 1
  }

  # Check if we got any data
  if (length(all_schools_data) == 0) {
    message(sprintf("No schools data found for country: %s", iso3))
    return(NULL)
  }

  # Convert all data to data frame
  schools_df <- do.call(
    rbind,
    lapply(all_schools_data, function(school) {
      data.frame(
        school_name = school$school_name,
        longitude = school$longitude,
        latitude = school$latitude,
        education_level = school$education_level,
        stringsAsFactors = FALSE
      )
    })
  )

  # Convert to sf object
  schools_sf <- sf::st_as_sf(
    schools_df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  message(sprintf(
    "✓ Giga schools data retrieved successfully: %s schools for %s",
    format(nrow(schools_sf), big.mark = ","),
    iso3
  ))

  return(schools_sf)
}


#' Get OpenStreetMap Tag Catalog
#'
#' @description
#' Returns the built-in OpenStreetMap tag definitions used by
#' \code{get_osm_feature_class()}.
#'
#' @return A named list. Each element is a feature class and contains a named
#'   list of OSM tag keys and accepted values.
#'
#' @export
#'
#' @examples
#' catalog <- get_osm_tag_catalog()
#' names(catalog)
get_osm_tag_catalog <- function() {
  list(
    food_retail = list(
      shop = c(
        "supermarket", "convenience", "general", "grocery",
        "greengrocer", "butcher", "bakery", "dairy", "seafood",
        "frozen_food", "food", "kiosk"
      )
    ),
    markets = list(
      amenity = "marketplace",
      shop = c("market", "farm")
    ),
    health_facilities = list(
      amenity = c(
        "hospital", "clinic", "doctors", "health_post", "pharmacy",
        "dentist"
      ),
      healthcare = c(
        "hospital", "clinic", "doctor", "health_post", "pharmacy",
        "dentist", "nurse"
      )
    ),
    schools = list(
      amenity = c("school", "kindergarten", "college", "university"),
      building = c("school", "college", "university")
    ),
    water_points = list(
      amenity = c("drinking_water", "water_point"),
      man_made = c("water_well", "water_tap", "water_tower"),
      emergency = "water_tank"
    ),
    slaughterhouses = list(
      amenity = "slaughterhouse",
      industrial = "slaughterhouse",
      building = "slaughterhouse"
    ),
    veterinary_services = list(
      amenity = "veterinary",
      healthcare = "veterinary"
    ),
    storage = list(
      building = c("warehouse", "storage_tank", "silo"),
      man_made = c("storage_tank", "silo"),
      industrial = c("warehouse", "depot")
    ),
    transport_nodes = list(
      amenity = c("bus_station", "ferry_terminal"),
      railway = c("station", "halt", "tram_stop"),
      highway = c("bus_stop", "services"),
      public_transport = c("station", "stop_position", "platform"),
      aeroway = c("aerodrome", "terminal", "helipad")
    ),
    border_crossings = list(
      barrier = "border_control",
      amenity = "border_control",
      border_control = c("checkpoint", "border_crossing"),
      highway = "border_control"
    ),
    major_roads = list(
      highway = c(
        "motorway", "trunk", "primary", "secondary", "tertiary",
        "motorway_link", "trunk_link", "primary_link", "secondary_link",
        "tertiary_link"
      )
    )
  )
}


#' List OpenStreetMap Feature Classes
#'
#' @description
#' Lists the feature classes available through \code{get_osm_feature_class()}.
#'
#' @return A character vector of feature class names.
#'
#' @export
#'
#' @examples
#' list_osm_feature_classes()
list_osm_feature_classes <- function() {
  names(get_osm_tag_catalog())
}


normalize_osm_tag_set <- function(tag_set) {
  if (!is.list(tag_set) || is.null(names(tag_set))) {
    stop("OSM tag sets must be named lists.", call. = FALSE)
  }

  tag_names <- names(tag_set)
  if (any(is.na(tag_names) | tag_names == "")) {
    stop("Every OSM tag set entry must have a tag key name.", call. = FALSE)
  }

  normalized <- list()
  for (tag_name in tag_names) {
    values <- unique(stats::na.omit(as.character(
      unlist(tag_set[[tag_name]], use.names = FALSE)
    )))

    if (length(values) == 0) {
      next
    }

    normalized[[tag_name]] <- unique(c(normalized[[tag_name]], values))
  }

  normalized[sort(names(normalized))]
}


combine_osm_tag_sets <- function(tag_sets) {
  if (!is.list(tag_sets)) {
    stop("tag_sets must be a list of named OSM tag sets.", call. = FALSE)
  }

  combined <- list()
  for (tag_set in tag_sets) {
    tag_set <- normalize_osm_tag_set(tag_set)
    for (tag_name in names(tag_set)) {
      combined[[tag_name]] <- unique(c(combined[[tag_name]], tag_set[[tag_name]]))
    }
  }

  combined[sort(names(combined))]
}


osm_feature_label <- function(feature_class) {
  label <- gsub("_", " ", feature_class)
  tools::toTitleCase(label)
}


osm_sql_quote_identifier <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}


osm_sql_quote_value <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}


osm_default_layer_tags <- function(layer_name) {
  switch(
    layer_name,
    points = c("barrier", "highway", "ref", "address", "is_in", "place", "man_made"),
    lines = c("highway", "waterway", "aerialway", "barrier", "man_made", "railway"),
    multipolygons = c(
      "type",
      "aeroway",
      "amenity",
      "admin_level",
      "barrier",
      "boundary",
      "building",
      "craft",
      "geological",
      "historic",
      "land_area",
      "landuse",
      "leisure",
      "man_made",
      "military",
      "natural",
      "office",
      "place",
      "shop",
      "sport",
      "tourism"
    ),
    character()
  )
}


build_osm_where_clause <- function(tag_sets, column_tags = names(tag_sets)) {
  tag_sets <- normalize_osm_tag_set(tag_sets)
  column_tags <- intersect(names(tag_sets), column_tags)

  if (length(tag_sets) == 0) {
    stop("At least one OSM tag value is required.", call. = FALSE)
  }

  conditions <- lapply(names(tag_sets), function(tag) {
    values <- tag_sets[[tag]]
    tag_conditions <- character()

    if (tag %in% column_tags) {
      tag_sql <- osm_sql_quote_identifier(tag)
      tag_conditions <- c(tag_conditions, if (length(values) == 1) {
        sprintf("%s = %s", tag_sql, osm_sql_quote_value(values))
      } else {
        quoted_values <- paste(vapply(
          values,
          osm_sql_quote_value,
          character(1),
          USE.NAMES = FALSE
        ), collapse = ", ")
        sprintf("%s IN (%s)", tag_sql, quoted_values)
      })
    }

    other_tags_expr <- paste(vapply(
      values,
      function(value) {
        sprintf(
          "other_tags LIKE %s",
          osm_sql_quote_value(paste0("%\"", tag, "\"=>\"", value, "\"%"))
        )
      },
      character(1),
      USE.NAMES = FALSE
    ), collapse = " OR ")
    tag_conditions <- c(tag_conditions, other_tags_expr)

    sprintf("(%s)", paste(tag_conditions, collapse = " OR "))
  })

  paste0("(", paste(conditions, collapse = " OR "), ")")
}


empty_osm_sf <- function(template = NULL, feature_class = NULL, feature_label = NULL) {
  if (inherits(template, "sf")) {
    result <- template[0, , drop = FALSE]
    if (is.na(sf::st_crs(result))) {
      result <- sf::st_set_crs(result, 4326)
    } else {
      result <- sf::st_transform(result, 4326)
    }
  } else {
    result <- sf::st_sf(osm_id = character(), geometry = sf::st_sfc(crs = 4326))
  }

  if (!is.null(feature_class)) {
    result$feature_class <- character()
  }
  if (!is.null(feature_label)) {
    result$feature_label <- character()
  }

  result
}


add_osm_feature_metadata <- function(x, feature_class, feature_label) {
  if (nrow(x) == 0) {
    x$feature_class <- character()
    x$feature_label <- character()
  } else {
    x$feature_class <- rep(feature_class, nrow(x))
    x$feature_label <- rep(feature_label, nrow(x))
  }

  x
}


filter_osm_sf_by_tags <- function(x, tag_set, feature_class, feature_label) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(empty_osm_sf(x, feature_class, feature_label))
  }

  tag_set <- normalize_osm_tag_set(tag_set)
  matches <- rep(FALSE, nrow(x))
  other_tags <- if ("other_tags" %in% names(x)) {
    as.character(x$other_tags)
  } else {
    rep(NA_character_, nrow(x))
  }

  for (tag_name in names(tag_set)) {
    if (tag_name %in% names(x)) {
      tag_values <- as.character(x[[tag_name]])
      matches <- matches | (!is.na(tag_values) & tag_values %in% tag_set[[tag_name]])
    }

    other_tag_patterns <- paste0("\"", tag_name, "\"=>\"", tag_set[[tag_name]], "\"")
    other_tag_matches <- Reduce(
      `|`,
      lapply(other_tag_patterns, grepl, x = other_tags, fixed = TRUE),
      init = rep(FALSE, length(other_tags))
    )
    matches <- matches | (!is.na(other_tags) & other_tag_matches)
  }

  result <- x[matches, , drop = FALSE]
  if (is.na(sf::st_crs(result))) {
    result <- sf::st_set_crs(result, 4326)
  } else {
    result <- sf::st_transform(result, 4326)
  }

  add_osm_feature_metadata(result, feature_class, feature_label)
}


filter_osm_features_by_tags <- function(osm_features, tag_set,
                                        feature_class = NA_character_,
                                        feature_label = NA_character_) {
  layer_names <- c("pts", "lines", "poly", "multipoly")
  result <- stats::setNames(vector("list", length(layer_names)), layer_names)

  for (layer_name in layer_names) {
    layer <- osm_features[[layer_name]]
    result[[layer_name]] <- filter_osm_sf_by_tags(
      layer,
      tag_set,
      feature_class,
      feature_label
    )
  }

  result
}


osm_cache_file <- function(region_sf, provider, match_level, layers, tag_sets,
                           cache_dir, match_place = NULL) {
  region_4326 <- sf::st_transform(region_sf, 4326)
  cache_key <- rlang::hash(list(
    osm_feature_cache_version = 4L,
    region_geometry = sf::st_as_binary(sf::st_geometry(region_4326)),
    provider = provider,
    match_level = match_level,
    match_place = match_place,
    layers = sort(layers),
    tag_sets = normalize_osm_tag_set(tag_sets)
  ))

  file.path(cache_dir, paste0("osm_features_", cache_key, ".rds"))
}


prepare_osm_region_boundary <- function(region_sf) {
  if (is.na(sf::st_crs(region_sf))) {
    stop("region_sf must have a coordinate reference system.", call. = FALSE)
  }

  region_sf <- sf::st_transform(region_sf, 4326)
  region_sf <- suppressWarnings(sf::st_transform(region_sf, 3857))
  region_sf <- sf::st_make_valid(region_sf)
  if (any(!sf::st_is_valid(region_sf))) {
    region_sf <- suppressWarnings(sf::st_buffer(region_sf, 0))
  }
  region_sf <- suppressWarnings(sf::st_collection_extract(region_sf, "POLYGON"))
  region_sf <- sf::st_transform(region_sf, 4326)

  if (nrow(region_sf) == 0 || all(sf::st_is_empty(region_sf))) {
    stop("region_sf does not contain a valid polygon boundary.", call. = FALSE)
  }
  if (any(!sf::st_is_valid(region_sf))) {
    stop("region_sf could not be repaired to a valid polygon boundary.", call. = FALSE)
  }

  region_sf
}


osm_bbox_boundary <- function(region_sf) {
  sf::st_sf(
    geometry = sf::st_as_sfc(sf::st_bbox(region_sf)),
    crs = sf::st_crs(region_sf)
  )
}


osm_region_match_point <- function(region_sf) {
  region_geometry <- sf::st_transform(sf::st_geometry(region_sf), 3857)
  match_point <- sf::st_point_on_surface(sf::st_union(region_geometry))
  sf::st_transform(match_point, 4326)
}


osm_match_input <- function(region_sf, match_place = NULL) {
  if (!is.null(match_place)) {
    match_place <- as.character(match_place)
    if (length(match_place) != 1L || is.na(match_place) || !nzchar(match_place)) {
      stop("match_place must be a non-empty length-one character value.", call. = FALSE)
    }
    return(match_place)
  }

  osm_region_match_point(region_sf)
}


osm_extract_zone <- function(match_info, provider) {
  if (is.null(match_info$url) || is.na(match_info$url)) {
    return(NULL)
  }

  provider_data <- switch(
    provider,
    geofabrik = osmextract::geofabrik_zones,
    bbbike = osmextract::bbbike_zones,
    openstreetmap_fr = osmextract::openstreetmap_fr_zones,
    test = osmextract::test_zones,
    NULL
  )

  if (is.null(provider_data) || !"pbf" %in% names(provider_data)) {
    return(NULL)
  }

  sf::st_crs(provider_data) <- 4326
  matched <- provider_data[provider_data$pbf == match_info$url, , drop = FALSE]
  if (nrow(matched) != 1L) {
    return(NULL)
  }

  matched
}


validate_osm_extract_coverage <- function(region_sf, match_info, provider,
                                          coverage_check = c("error", "warn", "none"),
                                          min_coverage = 0.98) {
  coverage_check <- match.arg(coverage_check)
  if (identical(coverage_check, "none")) {
    return(invisible(TRUE))
  }

  if (!is.numeric(min_coverage) || length(min_coverage) != 1L ||
      is.na(min_coverage) || min_coverage < 0 || min_coverage > 1) {
    stop("min_coverage must be a numeric value between 0 and 1.", call. = FALSE)
  }

  extract_zone <- osm_extract_zone(match_info, provider)
  if (is.null(extract_zone)) {
    msg <- paste0(
      "Could not validate OSM extract coverage because the matched extract ",
      "was not found in the provider zone catalogue."
    )
    if (identical(coverage_check, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }

  area_crs <- 6933
  region_area <- sf::st_transform(region_sf, area_crs)
  extract_area <- sf::st_transform(extract_zone, area_crs)

  region_geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(region_area)))
  extract_geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(extract_area)))
  intersection <- suppressWarnings(sf::st_intersection(region_geom, extract_geom))

  total_area <- sum(as.numeric(sf::st_area(region_geom)))
  covered_area <- if (length(intersection) == 0 || all(sf::st_is_empty(intersection))) {
    0
  } else {
    sum(as.numeric(sf::st_area(intersection)))
  }
  coverage_ratio <- covered_area / total_area

  if (is.na(coverage_ratio) || coverage_ratio < min_coverage) {
    msg <- sprintf(
      paste0(
        "Matched OSM extract %s covers %.1f%% of the requested region, ",
        "below min_coverage = %.1f%%. Pass a larger match_place, lower ",
        "match_level, or set coverage_check = \"none\" if partial coverage is intentional."
      ),
      match_info$url,
      100 * coverage_ratio,
      100 * min_coverage
    )
    if (identical(coverage_check, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}


#' Get OpenStreetMap Features by Feature Class
#'
#' @description
#' Retrieves one or more built-in OSM feature classes for a region. This is a
#' user-friendly wrapper around \code{get_osm_features()}, which remains
#' available for custom tag-based queries.
#'
#' @param region_sf An sf object defining the region of interest.
#' @param feature_classes Character vector of feature classes. See
#'   \code{list_osm_feature_classes()}.
#' @param cache_dir Optional directory used for osmextract downloads and cached
#'   combined query results.
#' @param provider Character. OSM data provider. Default is "geofabrik".
#' @param match_level Integer provider matching level passed to
#'   \code{osmextract::oe_match()} and \code{osmextract::oe_get()}.
#' @param match_place Optional length-one character place name passed to
#'   \code{osmextract::oe_match()} and \code{osmextract::oe_get()}. When
#'   \code{NULL}, the provider extract is matched using a point inside
#'   \code{region_sf}. Supplying a country or region name is safer for
#'   national or large-area workflows.
#' @param max_download_size_mb Maximum matched extract size in MB. Set to
#'   \code{NA} to disable the check.
#' @param layers Character vector of osmextract layers to query.
#' @param as_sf Logical. If TRUE, return sf objects. If FALSE, geometries are
#'   dropped from returned layers.
#' @param coverage_check Character. Whether to \code{"error"}, \code{"warn"},
#'   or do \code{"none"} when the matched provider extract covers less than
#'   \code{min_coverage} of \code{region_sf}.
#' @param min_coverage Minimum fraction of \code{region_sf} that must be
#'   covered by the matched provider extract when \code{coverage_check} is not
#'   \code{"none"}.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A named list by feature class. Each feature class contains
#'   \code{pts}, \code{lines}, \code{poly}, and \code{multipoly} entries.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' region <- st_read("region.shp")
#'
#' food <- get_osm_feature_class(region, "food_retail")
#' social <- get_osm_feature_class(region, c("health_facilities", "schools"))
#' }
get_osm_feature_class <- function(
  region_sf,
  feature_classes,
  cache_dir = NULL,
  provider = "geofabrik",
  match_level = 2,
  match_place = NULL,
  max_download_size_mb = 1500,
  layers = c("points", "lines", "multipolygons"),
  as_sf = TRUE,
  coverage_check = c("error", "warn", "none"),
  min_coverage = 0.98,
  verbose = FALSE
) {
  stopifnot(inherits(region_sf, "sf"))
  coverage_check <- match.arg(coverage_check)

  if (missing(feature_classes) || length(feature_classes) == 0) {
    stop("feature_classes must contain at least one OSM feature class.", call. = FALSE)
  }

  catalog <- get_osm_tag_catalog()
  feature_classes <- unique(as.character(feature_classes))
  unknown_classes <- setdiff(feature_classes, names(catalog))

  if (length(unknown_classes) > 0) {
    stop(
      "Unknown OSM feature class(es): ",
      paste(unknown_classes, collapse = ", "),
      ". Available feature classes: ",
      paste(names(catalog), collapse = ", "),
      call. = FALSE
    )
  }

  selected_tag_sets <- catalog[feature_classes]
  combined_tag_sets <- combine_osm_tag_sets(selected_tag_sets)

  cache_file <- NULL
  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    cache_file <- osm_cache_file(
      region_sf = region_sf,
      provider = provider,
      match_level = match_level,
      match_place = match_place,
      layers = layers,
      tag_sets = combined_tag_sets,
      cache_dir = cache_dir
    )
  }

  if (!is.null(cache_file) && file.exists(cache_file)) {
    if (verbose) {
      message("Reading cached OSM feature query: ", cache_file)
    }
    combined_features <- readRDS(cache_file)
  } else {
    if (verbose) {
      message(
        "Fetching OSM feature classes: ",
        paste(feature_classes, collapse = ", ")
      )
    }

    fetch_features <- function() {
      get_osm_features(
        region_sf = region_sf,
        tag_sets = combined_tag_sets,
        verbose = verbose,
        provider = provider,
        match_level = match_level,
        match_place = match_place,
        max_download_size_mb = max_download_size_mb,
        layers = layers,
        coverage_check = coverage_check,
        min_coverage = min_coverage,
        cache_dir = cache_dir
      )
    }

    combined_features <- if (isTRUE(verbose)) {
      fetch_features()
    } else {
      suppressMessages(fetch_features())
    }

    failed_layers <- attr(combined_features, "osm_failed_layers", exact = TRUE)
    if (!is.null(cache_file) && length(failed_layers) == 0L) {
      saveRDS(combined_features, cache_file)
    } else if (!is.null(cache_file) && verbose) {
      message(
        "Not caching OSM results because these layers failed: ",
        paste(failed_layers, collapse = ", ")
      )
    }
  }

  result <- stats::setNames(vector("list", length(feature_classes)), feature_classes)
  for (feature_class in feature_classes) {
    result[[feature_class]] <- filter_osm_features_by_tags(
      combined_features,
      catalog[[feature_class]],
      feature_class = feature_class,
      feature_label = osm_feature_label(feature_class)
    )
  }

  if (!isTRUE(as_sf)) {
    result <- lapply(result, function(feature_result) {
      lapply(feature_result, function(layer) {
        if (inherits(layer, "sf")) {
          sf::st_drop_geometry(layer)
        } else {
          as.data.frame(layer)
        }
      })
    })
  }

  result
}


#' Get OpenStreetMap Features
#'
#' @description
#' Retrieves OpenStreetMap (OSM) features within a specified spatial region
#' based on tag sets. Returns points, lines, polygons, and multipolygons
#' separately. Uses osmextract for efficient bulk data downloads.
#'
#' @importFrom magrittr %>%
#'
#' @param region_sf An sf object defining the region of interest.
#' @param tag_sets A named list of OSM tags to query. See
#'   \url{https://taginfo.openstreetmap.org/} for valid tags.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param provider Character. OSM data provider. Options: "geofabrik" (default),
#'   "bbbike", "openstreetmap_fr". See \code{osmextract::oe_providers()}.
#' @param match_level Integer provider matching level passed to
#'   \code{osmextract::oe_match()} and \code{osmextract::oe_get()}.
#' @param match_place Optional length-one character place name passed to
#'   \code{osmextract::oe_match()} and \code{osmextract::oe_get()}. When
#'   \code{NULL}, the provider extract is matched using a point inside
#'   \code{region_sf}. Supplying a country or region name is safer for
#'   national or large-area workflows.
#' @param max_download_size_mb Maximum matched extract size in MB. Set to
#'   \code{NA} to disable the check.
#' @param layers Character vector of osmextract layers to query.
#' @param coverage_check Character. Whether to \code{"error"}, \code{"warn"},
#'   or do \code{"none"} when the matched provider extract covers less than
#'   \code{min_coverage} of \code{region_sf}.
#' @param min_coverage Minimum fraction of \code{region_sf} that must be
#'   covered by the matched provider extract when \code{coverage_check} is not
#'   \code{"none"}.
#' @param cache_dir Optional directory used for osmextract downloads and
#'   converted files.
#'
#' @return A list containing:
#'   \describe{
#'     \item{pts}{sf object with OSM point features}
#'     \item{lines}{sf object with OSM line features (e.g., roads)}
#'     \item{poly}{sf object with OSM polygon features}
#'     \item{multipoly}{sf object with OSM multipolygon features}
#'   }
#'
#' @details
#' API Documentation: \url{https://wiki.openstreetmap.org/wiki/API}
#'
#' This function uses the osmextract package to download bulk OSM data from
#' providers like Geofabrik. This is more efficient and reliable than the
#' Overpass API for large regions, as it downloads pre-processed extracts
#' rather than querying the live database.
#'
#' **How it works:**
#' 1. Finds the smallest OSM extract covering the region (e.g., "Kenya")
#' 2. Downloads it ONCE in .pbf format (~100-200MB, cached locally)
#' 3. Converts to .gpkg format with requested tags as columns (cached)
#' 4. For each layer (points, lines, multipolygons):
#'    - Ensures tag columns exist via \code{extra_tags} parameter
#'    - Filters by tags using SQL at the database level (efficient)
#'    - Spatially clips to the region bounding box using GDAL
#'    - Intersects returned features with a repaired region boundary in R
#'    - Only filtered results are loaded into R memory
#' 5. Returns sf objects in WGS84 (EPSG:4326)
#'
#' **Performance notes:**
#' - First run: Downloads + converts data (few minutes for country-sized regions)
#' - Subsequent runs: Uses cached data (seconds, unless new tags requested)
#' - Tag filtering happens at SQL level, NOT by loading all data into R
#' - The \code{extra_tags} parameter ensures tag keys are available as columns
#' - No timeout issues unlike Overpass API
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a region of interest
#' library(sf)
#' region <- st_as_sf(st_as_sfc(st_bbox(c(
#'   xmin = -0.15, ymin = 51.5,
#'   xmax = -0.10, ymax = 51.52
#' ), crs = 4326)))
#'
#' # Query for specific features
#' osm_data <- get_osm_features(
#'   region_sf = region,
#'   tag_sets = list(
#'     "amenity" = "school",
#'     "highway" = c("primary", "secondary")
#'   )
#' )
#'
#' # Access different geometry types
#' schools <- osm_data$pts
#' roads <- osm_data$lines
#' }
get_osm_features <- function(
  region_sf,
  tag_sets,
  verbose = FALSE,
  provider = "geofabrik",
  match_level = 2, # 2 ~ countries for geofabrik
  match_place = NULL,
  max_download_size_mb = 1500, # set to NA to disable the check
  layers = c("points", "lines", "multipolygons"), # which layers to query
  coverage_check = c("error", "warn", "none"),
  min_coverage = 0.98,
  cache_dir = NULL
) {
  stopifnot(inherits(region_sf, "sf"))
  coverage_check <- match.arg(coverage_check)

  # ---- Work in EPSG:4326 for osmextract ----
  region_sf <- prepare_osm_region_boundary(region_sf)
  bb <- sf::st_bbox(region_sf)
  query_boundary <- osm_bbox_boundary(region_sf)
  region_geometry <- sf::st_union(sf::st_geometry(region_sf))

  if (!is.null(cache_dir) && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) {
    message("Fetching OpenStreetMap features using osmextract...")
    message("For valid OSM tags, see https://taginfo.openstreetmap.org/")
    message(sprintf(
      "Bounding box: (%.4f, %.4f) to (%.4f, %.4f)",
      bb["xmin"],
      bb["ymin"],
      bb["xmax"],
      bb["ymax"]
    ))
  }

  # ---- Match the provider zone; explicit place names are safer for countries ----
  match_input <- osm_match_input(region_sf, match_place = match_place)

  # Pre-check which extract will be used and how big it is
  match_info <- osmextract::oe_match(
    place = match_input,
    provider = provider,
    level = match_level,
    quiet = !verbose
  )

  validate_osm_extract_coverage(
    region_sf = region_sf,
    match_info = match_info,
    provider = provider,
    coverage_check = coverage_check,
    min_coverage = min_coverage
  )

  size_mb <- as.numeric(match_info$file_size) / 1024^2

  if (verbose) {
    message(sprintf(
      "Matched provider file: %s (%.0f MB)",
      match_info$url,
      size_mb
    ))
  }

  if (
    !is.null(max_download_size_mb) &&
      !is.na(max_download_size_mb) &&
      size_mb > max_download_size_mb
  ) {
    stop(
      sprintf(
        "Matched OSM extract is %.0f MB (> max_download_size_mb = %s). 
Refusing to download. Try:
  • a smaller region, or
  • lowering match_level, or
  • set max_download_size_mb = NA if you really want this file.",
        size_mb,
        max_download_size_mb
      ),
      call. = FALSE
    )
  }

  max_file_size <- if (
    !is.null(max_download_size_mb) &&
      !is.na(max_download_size_mb)
  ) {
    max_download_size_mb * 1024^2
  } else {
    Inf
  }

  tag_sets <- normalize_osm_tag_set(tag_sets)
  extra_tags <- names(tag_sets)

  if (verbose) {
    message("Requested OSM tags: ", paste(extra_tags, collapse = ", "))
    message("Ensuring columns: ", paste(extra_tags, collapse = ", "))
    message("Downloading OSM extract and querying layers...")
  }

  # ---- Helper: Query a specific layer with error handling ----
  query_layer <- function(layer_name, extra_tags, verbose) {
    tryCatch(
      {
        if (verbose) {
          message(sprintf("Querying %s layer...", layer_name))
        }
        where_clause <- build_osm_where_clause(
          tag_sets,
          column_tags = union(osm_default_layer_tags(layer_name), extra_tags)
        )
        if (verbose) {
          message("  SQL WHERE clause: ", where_clause)
        }

        sql_query <- sprintf(
          "SELECT * FROM '%s' WHERE %s",
          layer_name,
          where_clause
        )

        result <- osmextract::oe_get(
          place = match_input,
          provider = provider,
          layer = layer_name,
          query = sql_query, # SQL runs at st_read() stage
          extra_tags = extra_tags,
          download_directory = if (is.null(cache_dir)) {
            osmextract::oe_download_directory()
          } else {
            cache_dir
          },
          max_file_size = max_file_size,
          quiet = !verbose,
          boundary = query_boundary,
          boundary_type = "clipsrc"
        )

        if (!is.null(result) && nrow(result) > 0) {
          result <- sf::st_transform(result, 4326)
          result <- suppressWarnings(sf::st_intersection(
            result,
            region_geometry
          ))

          if (verbose) {
            message(sprintf(
              "  Found %d features in %s",
              nrow(result),
              layer_name
            ))
          }
          list(data = result, error = NULL)
        } else {
          if (verbose) {
            message(sprintf("  No features found in %s", layer_name))
          }
          list(data = empty_osm_sf(), error = NULL)
        }
      },
      error = function(e) {
        error_message <- sprintf(
          "Error querying OSM %s layer: %s",
          layer_name,
          conditionMessage(e)
        )
        warning(error_message, call. = FALSE)
        list(data = empty_osm_sf(), error = conditionMessage(e))
      }
    )
  }

  # ---- Query only requested layers ----
  pts_result <- if ("points" %in% layers) {
    query_layer("points", extra_tags, verbose)
  } else {
    list(data = empty_osm_sf(), error = NULL)
  }

  lines_result <- if ("lines" %in% layers) {
    query_layer("lines", extra_tags, verbose)
  } else {
    list(data = empty_osm_sf(), error = NULL)
  }

  poly_result <- if ("multipolygons" %in% layers) {
    query_layer("multipolygons", extra_tags, verbose)
  } else {
    list(data = empty_osm_sf(), error = NULL)
  }

  pts <- pts_result$data
  lines <- lines_result$data
  poly <- poly_result$data
  multipoly <- empty_osm_sf() # placeholder, as in your original function

  query_errors <- Filter(
    Negate(is.null),
    list(
      points = if ("points" %in% layers) pts_result$error else NULL,
      lines = if ("lines" %in% layers) lines_result$error else NULL,
      multipolygons = if ("multipolygons" %in% layers) poly_result$error else NULL
    )
  )
  failed_layers <- names(query_errors)

  total_features <- nrow(pts) + nrow(lines) + nrow(poly) + nrow(multipoly)
  if (verbose) {
    if (length(failed_layers) == 0L) {
      message(sprintf(
        "✓ OSM data retrieved successfully: %d total features",
        total_features
      ))
    } else {
      message(sprintf(
        "OSM query completed with %d failed layer(s) (%s); returning empty data for failed layers",
        length(failed_layers),
        paste(failed_layers, collapse = ", ")
      ))
    }
  }

  result <- list(pts = pts, lines = lines, poly = poly, multipoly = multipoly)
  attr(result, "osm_failed_layers") <- failed_layers
  attr(result, "osm_query_errors") <- query_errors
  result
}


#' Get Climate Watch NDC Data
#'
#' @description
#' Retrieves Nationally Determined Contributions (NDC) data from the Climate Watch
#' API with automatic pagination.
#'
#' @importFrom magrittr %>%
#'
#' @param iso3 Character. ISO3 country code (required).
#' @param per_page Integer. Number of records per page. Default is 500.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is FALSE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A list containing:
#'   \describe{
#'     \item{raw}{Complete raw data from the API}
#'     \item{critical}{Pivoted data with key NDC elements}
#'     \item{summary}{High-level summary statistics}
#'     \item{by_sector}{Aggregated statistics by sector}
#'   }
#'
#' @details
#' API Documentation: \url{https://www.climatewatchdata.org/about/ndc}
#'
#' The function processes NDC content extracting actions, targets, timeframes,
#' and cost information by sector and theme.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ndc_data <- get_ndc_data(iso3 = "KEN")
#'
#' # View summary
#' ndc_data$summary
#'
#' # View by sector
#' ndc_data$by_sector
#' }
get_ndc_data <- function(
  iso3,
  per_page = 500,
  verbose = FALSE,
  max_retries = 3
) {
  base <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"
  page <- 1
  pages <- list()

  if (verbose) {
    message(sprintf("Fetching NDC data for country: %s", iso3))
  }

  repeat {
    if (verbose) {
      message(sprintf("Fetching page %d...", page))
    }

    # Retry logic for each page
    retry_attempt <- 1
    success <- FALSE
    resp <- NULL

    while (retry_attempt <= max_retries && !success) {
      tryCatch(
        {
          resp <- httr2::request(base) %>%
            httr2::req_url_query(
              "countries[]" = iso3,
              page = page,
              per_page = per_page
            ) %>%
            httr2::req_user_agent("ndc-content-r/0.1") %>%
            httr2::req_perform()
          success <- TRUE
        },
        error = function(e) {
          if (retry_attempt < max_retries) {
            wait_time <- 2^retry_attempt
            if (verbose) {
              message(sprintf(
                "Request failed (attempt %d/%d): %s. Retrying in %d seconds...",
                retry_attempt,
                max_retries,
                conditionMessage(e),
                wait_time
              ))
            }
            Sys.sleep(wait_time)
            retry_attempt <<- retry_attempt + 1
          } else {
            stop(
              sprintf(
                "Failed to fetch NDC data after %d attempts. Last error: %s",
                max_retries,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        }
      )
    }

    dat <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)
    if (is.list(dat) && !is.null(dat$data)) {
      dat <- dat$data
    }
    if (length(dat) == 0) {
      if (verbose) {
        message("No more data found. Stopping pagination.")
      }
      break
    }
    pages[[page]] <- tibble::as_tibble(dat)
    if (verbose) {
      message(sprintf(
        "Retrieved %d records from page %d",
        nrow(pages[[page]]),
        page
      ))
    }
    page <- page + 1
  }
  if (!length(pages)) {
    empty <- tibble::tibble()
    return(list(
      raw = empty,
      critical = empty,
      summary = empty,
      by_sector = empty
    ))
  }
  raw <- dplyr::bind_rows(pages)
  # map only what we care about
  key_map <- c(
    ad_sec_action = "action",
    ad_sec_tar = "target",
    ad_sec_time = "timeframe",
    ad_sec_conc = "conditional_costs",
    ad_sec_unconc = "unconditional_costs"
  )
  critical <- raw %>%
    dplyr::mutate(
      key = dplyr::recode(indicator_id, !!!key_map, .default = NA_character_),
      value = dplyr::na_if(trimws(value), "Not Available"),
      theme = dplyr::coalesce(
        subsector,
        overview_category,
        sector,
        global_category
      )
    ) %>%
    dplyr::select(country, iso_code3, sector, theme, key, value) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = key,
      values_from = value,
      values_fn = ~ paste(unique(stats::na.omit(.x)), collapse = " | "),
      values_fill = NA_character_
    ) %>%
    dplyr::arrange(sector, theme) %>%
    dplyr::mutate(
      has_action = !is.na(action) & nzchar(action),
      has_target = !is.na(target) & nzchar(target),
      has_timeframe = !is.na(timeframe) & nzchar(timeframe),
      has_cost_info = (!is.na(conditional_costs) & nzchar(conditional_costs)) |
        (!is.na(unconditional_costs) & nzchar(unconditional_costs))
    )
  if (nrow(critical) == 0) {
    return(list(
      raw = raw,
      critical = critical,
      summary = tibble::tibble(),
      by_sector = tibble::tibble()
    ))
  }
  denom <- nrow(critical)
  # Extract submission date if available
  submission_date <- if ("submission_date" %in% names(raw)) {
    unique(raw$submission_date)[1]
  } else if ("updated_at" %in% names(raw)) {
    unique(raw$updated_at)[1]
  } else {
    NA_character_
  }
  summary <- tibble::tibble(
    country = critical$country[1],
    iso_code3 = iso3,
    submission_date = submission_date,
    sectors = dplyr::n_distinct(critical$sector),
    themes = dplyr::n_distinct(critical$theme),
    actions = sum(critical$has_action, na.rm = TRUE),
    with_targets = sum(critical$has_target, na.rm = TRUE),
    with_timeframes = sum(critical$has_timeframe, na.rm = TRUE),
    with_cost_info = sum(critical$has_cost_info, na.rm = TRUE),
    pct_with_targets = round(100 * with_targets / denom, 1),
    pct_with_timeframes = round(100 * with_timeframes / denom, 1),
    pct_with_cost_info = round(100 * with_cost_info / denom, 1)
  )
  # Count actual number of actions (some themes have multiple)
  action_counts <- raw %>%
    dplyr::filter(indicator_id == "ad_sec_action") %>%
    dplyr::filter(
      !is.na(value) & value != "Not Available" & nzchar(trimws(value))
    ) %>%
    dplyr::mutate(
      theme = dplyr::coalesce(
        subsector,
        overview_category,
        sector,
        global_category
      )
    ) %>%
    dplyr::group_by(sector, theme) %>%
    dplyr::summarise(n_actions = dplyr::n(), .groups = "drop")
  # First aggregate by sector
  by_sector_base <- critical %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      themes = dplyr::n_distinct(theme),
      themes_with_actions = sum(has_action, na.rm = TRUE),
      with_targets = sum(has_target, na.rm = TRUE),
      with_timeframes = sum(has_timeframe, na.rm = TRUE),
      with_cost_info = sum(has_cost_info, na.rm = TRUE),
      .groups = "drop"
    )
  # Add action counts
  by_sector <- by_sector_base %>%
    dplyr::left_join(
      action_counts %>%
        dplyr::group_by(sector) %>%
        dplyr::summarise(
          total_actions = sum(n_actions, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "sector"
    ) %>%
    dplyr::mutate(
      total_actions = ifelse(is.na(total_actions), 0, total_actions),
      actions = total_actions # Keep for backward compatibility
    ) %>%
    dplyr::arrange(dplyr::desc(actions), dplyr::desc(with_targets), sector)

  message(sprintf(
    "✓ NDC data retrieved successfully: %d sectors, %d themes analyzed",
    ifelse(nrow(summary) > 0 && !is.na(summary$sectors), summary$sectors, 0),
    ifelse(nrow(summary) > 0 && !is.na(summary$themes), summary$themes, 0)
  ))

  list(raw = raw, critical = critical, summary = summary, by_sector = by_sector)
}

#' Resolve Global Fishing Watch EEZ IDs
#'
#' Resolves every Global Fishing Watch Exclusive Economic Zone (EEZ) ID
#' associated with an exact ISO3 country code.
#'
#' @param iso3 Character scalar. ISO 3166-1 alpha-3 country code.
#' @param api_key Character. Global Fishing Watch API token (required).
#'
#' @return A tibble with columns `id` (double), `label` (character), `iso3`
#'   (character), `GEONAME` (character), and `POL_TYPE` (character). A valid
#'   landlocked country returns a zero-row tibble with these columns. Lookup
#'   and authentication errors are propagated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kenya_eez <- get_fishwatch_eez_ids("KEN", Sys.getenv("FISHWATCH_TOKEN"))
#' }
get_fishwatch_eez_ids <- function(iso3, api_key = NULL) {
  if (is.null(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("api_key parameter is required for Global Fishing Watch API")
  }
  if (!is.character(iso3) || length(iso3) != 1L || is.na(iso3)) {
    stop("iso3 must be a single ISO 3166-1 alpha-3 country code")
  }

  iso3 <- toupper(iso3)
  country <- suppressWarnings(countrycode::countrycode(
    iso3,
    origin = "iso3c",
    destination = "country.name"
  ))
  if (is.na(country)) {
    stop("Unknown ISO3 country code: ", iso3)
  }

  result <- .gfwr_region_id(region = iso3, key = api_key)
  result <- tibble::as_tibble(result)

  expected_columns <- c("id", "label", "iso3", "GEONAME", "POL_TYPE")
  missing_columns <- setdiff(expected_columns, names(result))
  if (length(missing_columns) > 0L) {
    stop(
      "Unexpected response from gfwr::gfw_region_id(); missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  exact_match <- !is.na(result$iso3) & result$iso3 == iso3
  result <- result[exact_match, expected_columns]
  result[order(result$id), ]
}

.gfwr_region_id <- function(region, key) {
  gfwr::gfw_region_id(
    region = region,
    region_source = "EEZ",
    key = key
  )
}

.gfwr_fishing_hours <- function(...) {
  gfwr::gfw_ais_fishing_hours(...)
}

.fishwatch_sleep <- function(seconds) {
  if (seconds > 0) {
    Sys.sleep(seconds)
  }
}

.fishwatch_is_rate_limit_error <- function(error) {
  grepl(
    "HTTP[[:space:]]+429|429 Too Many Requests|Too Many Requests",
    conditionMessage(error),
    ignore.case = TRUE
  )
}

.fishwatch_retry_wait <- function(error, attempt) {
  if (.fishwatch_is_rate_limit_error(error)) {
    return(60)
  }

  2^attempt
}

.empty_fishwatch_data <- function(group_by) {
  result <- tibble::tibble(
    Lat = double(),
    Lon = double(),
    `Time Range` = double()
  )

  if (identical(group_by, "FLAG")) {
    result$flag <- character()
  }

  result$`Vessel IDs` <- double()
  result$`Apparent Fishing Hours` <- double()
  result
}

.normalize_fishwatch_data <- function(data, group_by) {
  if (is.null(data) || nrow(data) == 0L) {
    return(.empty_fishwatch_data(group_by))
  }

  required_columns <- c(
    "Lat",
    "Lon",
    "Time Range",
    "Apparent Fishing Hours"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Unexpected Global Fishing Watch response; missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!"flag" %in% names(data) && identical(group_by, "FLAG")) {
    data$flag <- NA_character_
  }
  if (!"Vessel IDs" %in% names(data)) {
    data$`Vessel IDs` <- NA_real_
  }

  data |>
    dplyr::mutate(
      Lat = suppressWarnings(as.numeric(.data$Lat)),
      Lon = suppressWarnings(as.numeric(.data$Lon)),
      `Time Range` = suppressWarnings(as.numeric(.data$`Time Range`)),
      `Vessel IDs` = suppressWarnings(as.numeric(.data$`Vessel IDs`)),
      `Apparent Fishing Hours` =
        suppressWarnings(as.numeric(.data$`Apparent Fishing Hours`))
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("flag"), as.character)
    )
}

# HDX HAPI endpoint registry used by get_hdx_hapi().
.hdx_hapi_endpoints <- c(
  availability = "/api/v2/metadata/data-availability",
  data_availability = "/api/v2/metadata/data-availability",
  wfp_prices = "/api/v2/food-security-nutrition-poverty/food-prices-market-monitor",
  food_prices = "/api/v2/food-security-nutrition-poverty/food-prices-market-monitor",
  wfp_markets = "/api/v2/metadata/wfp-market",
  wfp_market = "/api/v2/metadata/wfp-market",
  wfp_commodities = "/api/v2/metadata/wfp-commodity",
  wfp_commodity = "/api/v2/metadata/wfp-commodity",
  food_security = "/api/v2/food-security-nutrition-poverty/food-security",
  poverty = "/api/v2/food-security-nutrition-poverty/poverty-rate",
  poverty_rate = "/api/v2/food-security-nutrition-poverty/poverty-rate",
  population = "/api/v2/geography-infrastructure/baseline-population",
  baseline_population = "/api/v2/geography-infrastructure/baseline-population"
)

.hdx_hapi_endpoint_path <- function(endpoint) {
  if (!is.character(endpoint) || length(endpoint) != 1 || !nzchar(endpoint)) {
    stop("endpoint must be a non-empty character string.", call. = FALSE)
  }

  if (grepl("^https?://", endpoint)) {
    return(endpoint)
  }

  if (startsWith(endpoint, "/")) {
    return(endpoint)
  }

  endpoint_id <- gsub("-", "_", tolower(endpoint))
  if (endpoint_id %in% names(.hdx_hapi_endpoints)) {
    return(unname(.hdx_hapi_endpoints[[endpoint_id]]))
  }

  paste0("/api/v2/", endpoint)
}

.hdx_hapi_compact_query <- function(query) {
  query <- query[!vapply(query, is.null, logical(1))]
  query <- query[!vapply(query, function(x) length(x) == 1 && is.na(x), logical(1))]
  query <- query[!vapply(query, function(x) length(x) == 1 && identical(x, ""), logical(1))]

  bad_lengths <- names(query)[vapply(query, function(x) length(x) > 1, logical(1))]
  if (length(bad_lengths) > 0) {
    stop(
      "HDX HAPI query parameters must be scalar. Non-scalar parameter(s): ",
      paste(bad_lengths, collapse = ", "),
      call. = FALSE
    )
  }

  lapply(query, function(x) {
    if (inherits(x, "Date")) {
      return(format(x, "%Y-%m-%d"))
    }
    if (inherits(x, "POSIXt")) {
      return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    }
    x
  })
}

.hdx_hapi_parse_response <- function(resp, output_format) {
  body <- httr2::resp_body_string(resp)

  if (tolower(output_format) == "csv") {
    if (!nzchar(body)) {
      return(tibble::tibble())
    }
    return(readr::read_csv(I(body), show_col_types = FALSE))
  }

  parsed <- jsonlite::fromJSON(body, flatten = TRUE)
  if (is.null(parsed$data) || length(parsed$data) == 0) {
    return(tibble::tibble())
  }

  tibble::as_tibble(parsed$data)
}

.hdx_hapi_perform <- function(req, max_retries, retry_base) {
  req <- httr2::req_retry(
    req,
    max_tries = max_retries,
    retry_on_failure = TRUE,
    is_transient = function(resp) {
      status <- httr2::resp_status(resp)
      status == 429 || status >= 500
    },
    backoff = function(attempt) {
      min(60, retry_base ^ attempt)
    }
  )

  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop(
      sprintf("HDX HAPI request failed with HTTP %s. %s", status, body),
      call. = FALSE
    )
  }

  resp
}

.hdx_hapi_parse_reference_periods <- function(x) {
  reference_cols <- intersect(
    c("reference_period_start", "reference_period_end"),
    names(x)
  )

  for (col in reference_cols) {
    x[[col]] <- as.POSIXct(x[[col]], tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  }

  if ("reference_period_start" %in% names(x)) {
    x$Year <- as.integer(format(x$reference_period_start, "%Y"))
  }

  x
}

.hdx_hapi_normalize_common <- function(x) {
  x <- .hdx_hapi_parse_reference_periods(x)

  if ("location_code" %in% names(x) && !"iso3" %in% names(x)) {
    x$iso3 <- x$location_code
  }

  if ("location_name" %in% names(x) && !"country" %in% names(x)) {
    x$country <- x$location_name
  }

  x$source <- "HDX HAPI"
  x
}

.hdx_hapi_ensure_columns <- function(x, columns) {
  for (col in columns) {
    if (!col %in% names(x)) {
      x[[col]] <- NA
    }
  }
  x[, columns, drop = FALSE]
}

.hdx_hapi_empty_schema <- list(
  availability = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin2_code", "admin2_name", "admin_level", "category", "subcategory",
    "hapi_updated_date", "iso3", "country", "source"
  ),
  wfp_markets = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin2_code", "admin2_name", "admin_level", "code", "name", "lat",
    "lon", "iso3", "country", "source"
  ),
  wfp_commodities = c("code", "category", "name", "source"),
  wfp_prices = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin2_code", "admin2_name", "admin_level", "resource_hdx_id",
    "market_code", "market_name", "commodity_code", "commodity_name",
    "commodity_category", "currency_code", "unit", "price_flag",
    "price_type", "price", "lat", "lon", "reference_period_start",
    "reference_period_end", "Year", "iso3", "country", "source"
  ),
  food_security = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin2_code", "admin2_name", "admin_level", "resource_hdx_id",
    "ipc_phase", "ipc_type", "population_in_phase",
    "population_fraction_in_phase", "reference_period_start",
    "reference_period_end", "Year", "iso3", "country", "source"
  ),
  poverty = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin_level", "resource_hdx_id", "mpi", "headcount_ratio",
    "intensity_of_deprivation", "vulnerable_to_poverty",
    "in_severe_poverty", "reference_period_start", "reference_period_end",
    "Year", "iso3", "country", "source"
  ),
  population = c(
    "location_code", "location_name", "admin1_code", "admin1_name",
    "admin2_code", "admin2_name", "admin_level", "resource_hdx_id",
    "gender", "age_range", "min_age", "max_age", "population",
    "reference_period_start", "reference_period_end", "Year", "iso3",
    "country", "source"
  )
)

.hdx_hapi_apply_mrv <- function(x, mrv) {
  if (is.null(mrv)) {
    return(x)
  }

  if (!is.numeric(mrv) || length(mrv) != 1 || is.na(mrv) || mrv < 1) {
    stop("mrv must be a positive integer.", call. = FALSE)
  }

  if (!"Year" %in% names(x) || nrow(x) == 0) {
    return(x)
  }

  years <- sort(unique(stats::na.omit(x$Year)), decreasing = TRUE)
  keep <- years[seq_len(min(as.integer(mrv), length(years)))]
  x[x$Year %in% keep, , drop = FALSE]
}

.hdx_hapi_as_sf <- function(x) {
  if (!all(c("lon", "lat") %in% names(x))) {
    stop("as_sf = TRUE requires lon and lat columns in the HDX HAPI response.", call. = FALSE)
  }

  if (nrow(x) == 0) {
    return(sf::st_sf(x, geometry = sf::st_sfc(crs = 4326)))
  }

  sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326, remove = FALSE, na.fail = FALSE)
}

#' Fetch data from the HDX Humanitarian API
#'
#' @description
#' Low-level helper for HDX HAPI endpoints. This uses the HDX Humanitarian API,
#' not the generic HDX CKAN API. It accepts either a full endpoint path such as
#' \code{"/api/v2/metadata/data-availability"} or a short endpoint id such as
#' \code{"availability"} or \code{"wfp_prices"}.
#'
#' @param endpoint Character. HAPI endpoint path, URL, or short endpoint id.
#' @param ... Additional scalar query parameters passed to the endpoint.
#' @param app_identifier Character. Base64 encoded HAPI app identifier. Defaults
#'   to \code{Sys.getenv("HDX_HAPI_APP_IDENTIFIER")}.
#' @param app_identifier_in Character. Send the app identifier as a query
#'   parameter (\code{"query"}) or as the
#'   \code{X-HDX-HAPI-APP-IDENTIFIER} header (\code{"header"}).
#' @param page_size Integer. Records per HAPI page. HAPI caps this at 10,000;
#'   this controls request size only, not the number of rows returned.
#' @param output_format Character. HAPI output format. Defaults to \code{"json"}.
#' @param max_retries Integer. Maximum retry attempts for HTTP 429 and 5xx
#'   responses.
#' @param retry_base Numeric. Exponential backoff base in seconds.
#' @param base_url Character. HAPI base URL.
#'
#' @return A tibble containing rows from the HAPI \code{data} array.
#' @export
#'
#' @examples
#' \dontrun{
#' availability <- get_hdx_hapi("availability", location_code = "KEN")
#' prices <- get_hdx_hapi("wfp_prices", location_code = "KEN", page_size = 1000)
#' }
get_hdx_hapi <- function(
  endpoint,
  ...,
  app_identifier = Sys.getenv("HDX_HAPI_APP_IDENTIFIER"),
  app_identifier_in = c("query", "header"),
  page_size = 10000,
  output_format = "json",
  max_retries = 3,
  retry_base = 2,
  base_url = "https://hapi.humdata.org"
) {
  app_identifier_in <- match.arg(app_identifier_in)
  output_format <- tolower(output_format)

  if (!nzchar(app_identifier)) {
    stop(
      "HDX HAPI app identifier is required. Set HDX_HAPI_APP_IDENTIFIER or ",
      "pass app_identifier. You can generate one with encode_hapi_app_identifier().",
      call. = FALSE
    )
  }

  if (!output_format %in% c("json", "csv")) {
    stop("output_format must be 'json' or 'csv'.", call. = FALSE)
  }

  if (!is.numeric(page_size) || length(page_size) != 1 || is.na(page_size) ||
      page_size < 1 || page_size > 10000) {
    stop("page_size must be a positive integer no greater than 10,000.", call. = FALSE)
  }

  page_size <- as.integer(page_size)
  path <- .hdx_hapi_endpoint_path(endpoint)
  url <- if (grepl("^https?://", path)) path else paste0(base_url, path)

  query <- .hdx_hapi_compact_query(list(...))
  managed_params <- intersect(names(query), c("limit", "offset"))
  if (length(managed_params) > 0) {
    stop(
      "limit and offset are managed internally so HDX HAPI calls fetch all pages. ",
      "Use page_size only to tune per-page request size.",
      call. = FALSE
    )
  }

  query$output_format <- output_format
  query$limit <- page_size

  pages <- list()
  current_offset <- 0L

  repeat {
    query$offset <- current_offset

    if (app_identifier_in == "query") {
      query$app_identifier <- app_identifier
    }

    req <- httr2::request(url)
    req <- do.call(httr2::req_url_query, c(list(req), query))

    if (app_identifier_in == "header") {
      req <- httr2::req_headers(
        req,
        `X-HDX-HAPI-APP-IDENTIFIER` = app_identifier
      )
    }

    resp <- .hdx_hapi_perform(req, max_retries = max_retries, retry_base = retry_base)
    page <- .hdx_hapi_parse_response(resp, output_format = output_format)
    pages[[length(pages) + 1]] <- page

    if (nrow(page) < page_size) {
      break
    }

    current_offset <- current_offset + page_size
  }

  dplyr::bind_rows(pages)
}

#' Encode an HDX HAPI app identifier
#'
#' @param application Character. Calling application name.
#' @param email Character. Contact email address.
#'
#' @return Character. Encoded app identifier for HDX HAPI calls.
#' @export
#'
#' @examples
#' \dontrun{
#' encode_hapi_app_identifier("my-analysis", "me@example.org")
#' }
encode_hapi_app_identifier <- function(application, email) {
  resp <- httr2::request("https://hapi.humdata.org/api/v2/encode_app_identifier") |>
    httr2::req_url_query(application = application, email = email) |>
    .hdx_hapi_perform(max_retries = 3, retry_base = 2)

  parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  parsed$encoded_app_identifier
}

#' Get HDX HAPI data availability
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param category Character. Optional HAPI category.
#' @param subcategory Character. Optional HAPI subcategory.
#' @param admin_level Integer. Optional admin level.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble.
#' @export
get_hdx_hapi_availability <- function(
  iso3 = NULL,
  category = NULL,
  subcategory = NULL,
  admin_level = NULL,
  ...
) {
  out <- get_hdx_hapi(
    "availability",
    location_code = iso3,
    category = category,
    subcategory = subcategory,
    admin_level = admin_level,
    ...
  )
  out <- .hdx_hapi_normalize_common(out)
  .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$availability)
}

#' Get HDX HAPI WFP food prices
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param commodity_code Character. Optional WFP commodity code.
#' @param commodity_name Character. Optional commodity name.
#' @param commodity_category Character. Optional commodity category.
#' @param start_date,end_date Character or Date. Optional reference period
#'   overlap filters.
#' @param admin_level Integer. Optional admin level.
#' @param mrv Integer. Optional number of most recent years to keep after
#'   retrieval, based on \code{reference_period_start}.
#' @param as_sf Logical. If TRUE, return an sf object using lon/lat coordinates.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble, or an sf object when \code{as_sf = TRUE}.
#' @export
get_hdx_hapi_wfp_prices <- function(
  iso3 = NULL,
  commodity_code = NULL,
  commodity_name = NULL,
  commodity_category = NULL,
  start_date = NULL,
  end_date = NULL,
  admin_level = NULL,
  mrv = NULL,
  as_sf = FALSE,
  ...
) {
  out <- get_hdx_hapi(
    "wfp_prices",
    location_code = iso3,
    commodity_code = commodity_code,
    commodity_name = commodity_name,
    commodity_category = commodity_category,
    start_date = start_date,
    end_date = end_date,
    admin_level = admin_level,
    ...
  )
  out <- .hdx_hapi_normalize_common(out)
  out <- .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$wfp_prices)
  out <- .hdx_hapi_apply_mrv(out, mrv)

  if (isTRUE(as_sf)) {
    out <- .hdx_hapi_as_sf(out)
  }

  out
}

#' Get HDX HAPI WFP markets
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param as_sf Logical. If TRUE, return an sf object using lon/lat coordinates.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble, or an sf object when \code{as_sf = TRUE}.
#' @export
get_hdx_hapi_wfp_markets <- function(iso3 = NULL, as_sf = FALSE, ...) {
  out <- get_hdx_hapi("wfp_markets", location_code = iso3, ...)
  out <- .hdx_hapi_normalize_common(out)
  out <- .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$wfp_markets)

  if (isTRUE(as_sf)) {
    out <- .hdx_hapi_as_sf(out)
  }

  out
}

#' Get HDX HAPI WFP commodities
#'
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble.
#' @export
get_hdx_hapi_wfp_commodities <- function(...) {
  out <- get_hdx_hapi("wfp_commodities", ...)
  out <- .hdx_hapi_normalize_common(out)
  .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$wfp_commodities)
}

#' Get HDX HAPI food security data
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param ipc_phase Character. Optional IPC phase.
#' @param ipc_type Character. Optional IPC type.
#' @param start_date,end_date Character or Date. Optional reference period
#'   overlap filters.
#' @param admin_level Integer. Optional admin level.
#' @param mrv Integer. Optional number of most recent years to keep after
#'   retrieval, based on \code{reference_period_start}.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble.
#' @export
get_hdx_hapi_food_security <- function(
  iso3 = NULL,
  ipc_phase = NULL,
  ipc_type = NULL,
  start_date = NULL,
  end_date = NULL,
  admin_level = NULL,
  mrv = NULL,
  ...
) {
  out <- get_hdx_hapi(
    "food_security",
    location_code = iso3,
    ipc_phase = ipc_phase,
    ipc_type = ipc_type,
    start_date = start_date,
    end_date = end_date,
    admin_level = admin_level,
    ...
  )
  out <- .hdx_hapi_normalize_common(out)
  out <- .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$food_security)
  .hdx_hapi_apply_mrv(out, mrv)
}

#' Get HDX HAPI poverty data
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param admin_level Integer. Optional admin level.
#' @param start_date,end_date Character or Date. Optional reference period
#'   overlap filters.
#' @param mrv Integer. Optional number of most recent years to keep after
#'   retrieval, based on \code{reference_period_start}.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble.
#' @export
get_hdx_hapi_poverty <- function(
  iso3 = NULL,
  admin_level = NULL,
  start_date = NULL,
  end_date = NULL,
  mrv = NULL,
  ...
) {
  out <- get_hdx_hapi(
    "poverty",
    location_code = iso3,
    admin_level = admin_level,
    start_date = start_date,
    end_date = end_date,
    ...
  )
  out <- .hdx_hapi_normalize_common(out)
  out <- .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$poverty)
  .hdx_hapi_apply_mrv(out, mrv)
}

#' Get HDX HAPI baseline population data
#'
#' @param iso3 Character. Optional ISO3 country code.
#' @param gender Character. Optional gender filter.
#' @param age_range Character. Optional age range filter.
#' @param admin_level Integer. Optional admin level.
#' @param start_date,end_date Character or Date. Optional reference period
#'   overlap filters.
#' @param mrv Integer. Optional number of most recent years to keep after
#'   retrieval, based on \code{reference_period_start}.
#' @param ... Additional arguments passed to \code{get_hdx_hapi()}.
#'
#' @return A tibble.
#' @export
get_hdx_hapi_population <- function(
  iso3 = NULL,
  gender = NULL,
  age_range = NULL,
  admin_level = NULL,
  start_date = NULL,
  end_date = NULL,
  mrv = NULL,
  ...
) {
  out <- get_hdx_hapi(
    "population",
    location_code = iso3,
    gender = gender,
    age_range = age_range,
    admin_level = admin_level,
    start_date = start_date,
    end_date = end_date,
    ...
  )
  out <- .hdx_hapi_normalize_common(out)
  out <- .hdx_hapi_ensure_columns(out, .hdx_hapi_empty_schema$population)
  .hdx_hapi_apply_mrv(out, mrv)
}

#' Get Global Fishing Watch Data
#'
#' @description
#' Retrieves apparent fishing effort data from Global Fishing Watch API using
#' the gfwr package. Supports various spatial and temporal resolutions with
#' flexible region definitions.
#'
#' @param spatial_resolution Character. Raster spatial resolution. Can be "LOW" (0.1 degree)
#'   or "HIGH" (0.01 degree). Default is "LOW".
#' @param temporal_resolution Character. Raster temporal resolution. Can be "HOURLY",
#'   "DAILY", "MONTHLY", "YEARLY". Default is "MONTHLY".
#' @param start_date Character. Start of date range in YYYY-MM-DD format. Default is "2023-01-01".
#' @param end_date Character. Exclusive end of the date range in YYYY-MM-DD
#'   format. To request a complete calendar year `YYYY`, use
#'   `start_date = "YYYY-01-01"` and `end_date = "YYYY+1-01-01"`.
#'   Default is "2024-01-01".
#' @param region_source Character. Source of the region: "EEZ", "MPA", "RFMO" or "USER_SHAPEFILE".
#' @param region Character, numeric, or sf object. If region_source is "EEZ", "MPA" or "RFMO",
#'   GFW region ID. Multiple EEZ IDs are supported and are requested
#'   individually before being combined. Use [get_fishwatch_eez_ids()] to
#'   resolve every EEZ ID for an ISO3 code. If region_source =
#'   "USER_SHAPEFILE", an sf polygon with the area of interest.
#' @param group_by Character. Parameter to group by. Can be "VESSEL_ID", "FLAG",
#'   "GEARTYPE", "FLAGANDGEARTYPE" or "MMSI". Optional.
#' @param filter_by Character. Fields to filter AIS-based apparent fishing effort.
#'   SQL expressions like filter_by = "flag IN ('ESP')". Optional.
#' @param api_key Character. Global Fishing Watch API token (required).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is TRUE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A tibble. For `group_by = "FLAG"`, columns are `Lat` (double),
#'   `Lon` (double), `Time Range` (double for `YEARLY`; date/datetime-like for
#'   finer temporal resolutions as parsed by gfwr), `flag` (character),
#'   `Vessel IDs` (double), and `Apparent Fishing Hours` (double). Other
#'   `group_by` values replace `flag` with the corresponding grouping columns.
#'   A successful response with zero fishing activity returns a zero-row
#'   tibble, never `NULL`. Lookup, authentication, and partial request failures
#'   raise an error.
#'
#' @details
#' This function provides access to Global Fishing Watch's apparent fishing effort data
#' through their API. It requires authentication with a valid API key.
#'
#' **Authentication Required:** Obtain an API key from Global Fishing Watch.
#'
#' **Date Range Limitation:** Start and end dates must be 366 days apart or
#' less. `start_date` is inclusive and `end_date` is exclusive. For a complete
#' calendar year, request through January 1 of the following year.
#'
#' **Spatial Resolution:** `"HIGH"` is 0.01 degrees and `"LOW"` is 0.1 degrees.
#'
#' **Multiple EEZs:** gfwr accepts one EEZ per API request. When `region`
#' contains multiple EEZ IDs, this function requests all of them and combines
#' the rows. If any request fails after retries, the function errors rather
#' than returning partial data.
#'
#' **Rate limits:** HTTP 429 responses wait 60 seconds before retrying.
#'
#' **Region Sources:**
#' - EEZ: Exclusive Economic Zone
#' - MPA: Marine Protected Area
#' - RFMO: Regional Fisheries Management Organization
#' - USER_SHAPEFILE: Custom sf polygon object
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Get fishing effort for Spain EEZ
#' fishing_data <- get_fishwatch_data(
#'   spatial_resolution = "LOW",
#'   temporal_resolution = "MONTHLY",
#'   start_date = "2023-01-01",
#'   end_date = "2024-01-01",
#'   region_source = "EEZ",
#'   region = get_fishwatch_eez_ids("ESP", "your_api_key")$id,
#'   group_by = "FLAG",
#'   api_key = "your_api_key"
#' )
#'
#' # Get fishing effort for a custom region
#' library(sf)
#' custom_region <- st_read("my_area.shp")
#' fishing_data <- get_fishwatch_data(
#'   spatial_resolution = "HIGH",
#'   temporal_resolution = "DAILY",
#'   start_date = "2023-06-01",
#'   end_date = "2023-06-30",
#'   region_source = "USER_SHAPEFILE",
#'   region = custom_region,
#'   api_key = "your_api_key"
#' )
#' }
get_fishwatch_data <- function(
  spatial_resolution = "LOW",
  temporal_resolution = "MONTHLY",
  start_date = "2023-01-01",
  end_date = "2024-01-01",
  region_source = NULL,
  region = NULL,
  group_by = NULL,
  filter_by = NULL,
  api_key = NULL,
  verbose = TRUE,
  max_retries = 3
) {
  # Validate required parameters
  if (is.null(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("api_key parameter is required for Global Fishing Watch API")
  }

  if (is.null(region_source)) {
    stop("region_source parameter is required")
  }

  if (is.null(region)) {
    stop("region parameter is required")
  }
  if (length(max_retries) != 1L || is.na(max_retries) || max_retries < 1L) {
    stop("max_retries must be a positive integer")
  }

  if (verbose) {
    message("Fetching Global Fishing Watch data...")
    message(sprintf("Spatial resolution: %s", spatial_resolution))
    message(sprintf("Temporal resolution: %s", temporal_resolution))
    message(sprintf("Date range: %s to %s", start_date, end_date))
    message(sprintf("Region source: %s", region_source))
  }

  region_source <- toupper(region_source)
  regions <- if (identical(region_source, "EEZ")) region else list(region)
  results <- vector("list", length(regions))

  for (region_index in seq_along(regions)) {
    current_region <- regions[[region_index]]

    for (attempt in seq_len(max_retries)) {
      result <- tryCatch(
        .gfwr_fishing_hours(
          spatial_resolution = spatial_resolution,
          temporal_resolution = temporal_resolution,
          start_date = start_date,
          end_date = end_date,
          region_source = region_source,
          region = current_region,
          group_by = group_by,
          filter_by = filter_by,
          key = api_key,
          print_request = verbose
        ),
        error = identity
      )

      if (!inherits(result, "error")) {
        results[[region_index]] <- .normalize_fishwatch_data(result, group_by)
        break
      }

      if (attempt == max_retries) {
        stop(
          "Failed to retrieve Global Fishing Watch data for region ",
          current_region,
          " after ",
          max_retries,
          " attempts: ",
          conditionMessage(result),
          if (.fishwatch_is_rate_limit_error(result)) {
            paste0(
              ". The API is rate limiting requests; retry later or increase max_retries."
            )
          } else {
            ""
          }
        )
      }
      wait_time <- .fishwatch_retry_wait(result, attempt)
      if (verbose) {
        message(sprintf(
          "Attempt %d for region %s failed, retrying in %.1f seconds...",
          attempt,
          current_region,
          wait_time
        ))
      }
      .fishwatch_sleep(wait_time)
    }
  }

  non_null_results <- Filter(Negate(is.null), results)
  if (length(non_null_results) == 0L) {
    result <- .empty_fishwatch_data(group_by)
  } else {
    result <- dplyr::bind_rows(non_null_results)
  }

  if (verbose) {
    message(sprintf("Successfully retrieved %d records", nrow(result)))
  }

  result
}
