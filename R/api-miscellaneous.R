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
#'    - Spatially clips to region boundaries using GDAL
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
  max_download_size_mb = 1500 # set to NA to disable the check
) {
  stopifnot(inherits(region_sf, "sf"))

  # ---- Work in EPSG:4326 for osmextract ----
  region_sf <- sf::st_transform(region_sf, 4326)
  bb <- sf::st_bbox(region_sf)

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

  # ---- Use a point inside the region just for matching the provider zone ----
  match_geom <- region_sf |>
    sf::st_union() |>
    sf::st_point_on_surface()

  # Pre-check which extract will be used and how big it is
  match_info <- osmextract::oe_match(
    place = match_geom,
    provider = provider,
    level = match_level,
    quiet = !verbose
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

  # ---- Helper: empty sf ----
  empty_min <- function() {
    sf::st_sf(osm_id = character(), geometry = sf::st_sfc(crs = 4326))
  }

  # ---- Helper: Build SQL WHERE clause from tag_sets ----
  # tag_sets = list("amenity" = c("restaurant", "cafe"), "shop" = "supermarket")
  # -> "(amenity IN ('restaurant', 'cafe') OR shop = 'supermarket')"
  build_where_clause <- function(tag_sets) {
    conditions <- lapply(names(tag_sets), function(tag) {
      values <- tag_sets[[tag]]
      if (length(values) == 1) {
        sprintf("%s = '%s'", tag, values)
      } else {
        quoted_values <- paste0("'", values, "'", collapse = ", ")
        sprintf("%s IN (%s)", tag, quoted_values)
      }
    })
    paste0("(", paste(conditions, collapse = " OR "), ")")
  }

  where_clause <- build_where_clause(tag_sets)
  extra_tags <- unique(names(tag_sets))

  if (verbose) {
    message("SQL WHERE clause: ", where_clause)
    message("Ensuring columns: ", paste(extra_tags, collapse = ", "))
    message("Downloading OSM extract and querying layers...")
  }

  # ---- Helper: Query a specific layer with error handling ----
  query_layer <- function(layer_name, where_clause, extra_tags, verbose) {
    tryCatch(
      {
        if (verbose) {
          message(sprintf("Querying %s layer...", layer_name))
        }

        sql_query <- sprintf(
          "SELECT * FROM '%s' WHERE %s",
          layer_name,
          where_clause
        )

        result <- osmextract::oe_get(
          place = match_geom, # **point used for matching**
          provider = provider,
          layer = layer_name,
          query = sql_query, # SQL runs at st_read() stage
          extra_tags = extra_tags,
          quiet = !verbose,
          boundary = region_sf, # **full polygon used for clipping**
          boundary_type = "clipsrc"
        )

        if (!is.null(result) && nrow(result) > 0) {
          # Ensure CRS is 4326
          result <- sf::st_transform(result, 4326)

          if (verbose) {
            message(sprintf(
              "  Found %d features in %s",
              nrow(result),
              layer_name
            ))
          }
          result
        } else {
          if (verbose) {
            message(sprintf("  No features found in %s", layer_name))
          }
          empty_min()
        }
      },
      error = function(e) {
        if (verbose) {
          message(sprintf(
            "  Error querying %s: %s",
            layer_name,
            conditionMessage(e)
          ))
        }
        empty_min()
      }
    )
  }

  # ---- Query each layer type ----
  pts <- query_layer("points", where_clause, extra_tags, verbose)
  lines <- query_layer("lines", where_clause, extra_tags, verbose)
  poly <- query_layer("multipolygons", where_clause, extra_tags, verbose)

  multipoly <- empty_min() # placeholder, as in your original function

  total_features <- nrow(pts) + nrow(lines) + nrow(poly) + nrow(multipoly)
  message(sprintf(
    "✓ OSM data retrieved successfully: %d total features",
    total_features
  ))

  list(pts = pts, lines = lines, poly = poly, multipoly = multipoly)
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
#' @param end_date Character. End of date range in YYYY-MM-DD format. Default is "2023-12-31".
#' @param region_source Character. Source of the region: "EEZ", "MPA", "RFMO" or "USER_SHAPEFILE".
#' @param region Character, numeric, or sf object. If region_source is "EEZ", "MPA" or "RFMO",
#'   GFW region ID (e.g., 5682 for Italian EEZ). If region_source = "USER_SHAPEFILE",
#'   sf shapefile with area of interest.
#' @param group_by Character. Parameter to group by. Can be "VESSEL_ID", "FLAG",
#'   "GEARTYPE", "FLAGANDGEARTYPE" or "MMSI". Optional.
#' @param filter_by Character. Fields to filter AIS-based apparent fishing effort.
#'   SQL expressions like filter_by = "flag IN ('ESP')". Optional.
#' @param api_key Character. Global Fishing Watch API token (required).
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is TRUE.
#' @param max_retries Integer. Maximum number of retry attempts for failed requests.
#'   Default is 3.
#'
#' @return A tibble containing fishing effort data with spatial and temporal information.
#'
#' @details
#' This function provides access to Global Fishing Watch's apparent fishing effort data
#' through their API. It requires authentication with a valid API key.
#'
#' **Authentication Required:** Obtain an API key from Global Fishing Watch.
#'
#' **Date Range Limitation:** Start and end dates should be apart 366 days or less.
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
#'   end_date = "2023-12-31",
#'   region_source = "EEZ",
#'   region = 5682,  # Italian EEZ ID (use get_region_id("ITA", "EEZ") to find)
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
  end_date = "2023-12-31",
  region_source = NULL,
  region = NULL,
  group_by = NULL,
  filter_by = NULL,
  api_key = NULL,
  verbose = TRUE,
  max_retries = 3
) {
  # Validate required parameters
  if (is.null(api_key)) {
    stop("api_key parameter is required for Global Fishing Watch API")
  }

  if (is.null(region_source)) {
    stop("region_source parameter is required")
  }

  if (is.null(region)) {
    stop("region parameter is required")
  }

  if (verbose) {
    message("Fetching Global Fishing Watch data...")
    message(sprintf("Spatial resolution: %s", spatial_resolution))
    message(sprintf("Temporal resolution: %s", temporal_resolution))
    message(sprintf("Date range: %s to %s", start_date, end_date))
    message(sprintf("Region source: %s", region_source))
  }

  # Retry logic for API calls
  for (attempt in 1:max_retries) {
    tryCatch(
      {
        # Call the gfwr package's get_raster function
        result <- gfwr::get_raster(
          spatial_resolution = spatial_resolution,
          temporal_resolution = temporal_resolution,
          start_date = start_date,
          end_date = end_date,
          region_source = region_source,
          region = region,
          group_by = group_by,
          filter_by = filter_by,
          key = api_key,
          print_request = verbose
        )

        if (verbose) {
          message(sprintf("Successfully retrieved %d records", nrow(result)))
        }

        return(result)
      },
      error = function(e) {
        if (attempt == max_retries) {
          stop(
            "Failed to retrieve Global Fishing Watch data after ",
            max_retries,
            " attempts: ",
            e$message
          )
        }
        if (verbose) {
          message(sprintf("Attempt %d failed, retrying...", attempt))
        }
        Sys.sleep(2^attempt) # Exponential backoff
      }
    )
  }
}
